#include "tv_spike.h"

int run(tv_spike_t *tv)
{
  int code;
  tv->reset();
  while(!tv->exited(code))
    tv->step();
  fprintf(stderr, "Exited with code %d.\n", code);
  return code;
}

void print_usage(char *prog)
{
  fprintf(stderr, "Usage: %s elf_file\n", prog);
  exit(1);
}

int main(int argc, char **argv)
{
  if (argc < 2) print_usage(argv[0]);

  tv_spike_t s("RV64IMAFDC");
  s.init_elf(argv[1]);
  exit(run(&s));
}
