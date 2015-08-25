#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>
#include <assert.h>
#include <stdbool.h>
#include <fenv.h>
#include <math.h>
#ifdef __APPLE__
#include <float.h>     // For Mac OS X
#else
#include <values.h>    // For Linux
#endif

#define PART_OF_L3RISCV 1
#include "riscv_ffi.h"

#define ENV_FILENAME "SIM_ELF_FILENAME"

static int lib_is_opened = 0;

static void check_open() {
  if (!lib_is_opened) {
    char *argv = NULL;

    l3riscv_open(0, &argv);
    _l3r_init_model();

    lib_is_opened = 1;
  }
}

void l3riscv_load_elf(const char *filename)
{
  char *f;
  check_open();

  f = filename ? filename : getenv(ENV_FILENAME);
  if (NULL == f) {
    fprintf(stderr, "l3riscv_load_elf: no filename specified.\n");
    exit(1);
  }

  (void) _l3r_load_elf(f);
}

uint64_t l3riscv_get_min_mem_addr ()
{
  check_open();
  return _l3_get_mem_base ();
}

uint64_t l3riscv_get_max_mem_addr ()
{
  check_open();

  uint64_t base = _l3_get_mem_base();
  uint64_t size = _l3_get_mem_size();
  return base + size;
}

uint64_t l3riscv_read_mem_64 (uint64_t mem_addr)
{
  check_open();
  return 0;
}

uint32_t l3riscv_read_mem_32 (uint64_t mem_addr)
{
  check_open();
  return 0;
}

uint32_t l3riscv_verify (uint32_t exc_taken,
                         uint64_t pc,
                         uint64_t addr,
                         uint64_t data1,
                         uint64_t data2,
                         uint64_t data3,
                         uint64_t fpdata,
                         uint32_t verbosity)
{ uint32_t ret = 0;
  check_open();
  return ret;
}

uint64_t l3riscv_get_exit_pc ()
{
  check_open();
  return 0;
}
