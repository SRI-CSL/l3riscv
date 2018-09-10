#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "tv_spike_intf.h"

void run_elf(const char *isa, const char *filename)
{
  struct tv_spike_t *s = tv_init(isa);
  tv_set_verbose(s, 1);
  tv_load_elf(s, filename);
  tv_reset(s);

  while (!tv_is_done(s)) tv_step(s);

  tv_free(s);
}

void print_usage(const char *argv0)
{
  fprintf(stderr, "Usage: %s <elf_file>\n", argv0);
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
