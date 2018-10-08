#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "tv_spike_intf.h"

void run_elf(const char *isa, const char *filename)
{
  struct tv_spike_t *s = tv_init(isa, 1);
  tv_set_verbose(s, 1);
  tv_set_dtb_in_rom(s, 1);
  tv_load_elf(s, filename);
  tv_reset(s);

  { size_t insns_per_tick = tv_get_insns_per_tick(s);
    size_t cnt = 0;
    while (!tv_is_done(s)) {
      tv_step(s);
      cnt++;
      if (cnt == insns_per_tick) {
        tv_tick_clock(s);
        tv_step_io(s);
      }
    }
  }
  tv_free(s);
}

void print_usage(const char *argv0)
{
  fprintf(stderr, "Usage: %s <elf_file>\n", argv0);
  exit(0);
}

void print_dts(void)
{
  size_t dts_len = 0;
  struct tv_spike_t *s = tv_init("RV64IMAC", 0);
  tv_get_dts(s, NULL, &dts_len);
  if (dts_len > 0) {
    unsigned char *dts = (unsigned char *)malloc(dts_len + 1);
    dts[dts_len] = '\0';
    tv_get_dts(s, dts, &dts_len);
    fprintf(stdout, "%s\n", dts);
  }
  exit(0);
}

int is_file(const char *f)
{
  struct stat st;
  return (!stat(f, &st) && (st.st_mode & S_IFMT) == S_IFREG);
}

int main(int argc, const char **argv)
{
  if (argc != 2) print_usage(argv[0]);
  if (!is_file(argv[1])) print_usage(argv[0]);

  run_elf("RV64IMAFDC", argv[1]);
}
