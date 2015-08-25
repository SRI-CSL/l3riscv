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

uint64_t l3riscv_get_min_mem_addr()
{
  check_open();
  return _l3_get_mem_base();
}

uint64_t l3riscv_get_max_mem_addr()
{
  check_open();

  uint64_t base = _l3_get_mem_base();
  uint64_t size = _l3_get_mem_size();
  return base + size;
}

/* _l3r_read_mem internally handles unaligned reads */

uint64_t l3riscv_read_mem_64(uint64_t mem_addr)
{
  check_open();
  return _l3r_read_mem(mem_addr);
}

uint32_t l3riscv_read_mem_32(uint64_t mem_addr)
{
  check_open();
  return (uint32_t) _l3r_read_mem(mem_addr);
}

/* TODO: */

uint32_t l3riscv_verify(uint32_t exc_taken,
                        uint64_t pc,
                        uint64_t addr,
                        uint64_t data1,
                        uint64_t data2,
                        uint64_t data3,
                        uint64_t fpdata,
                        uint32_t verbosity)
{  check_open();
  return 0;
}

uint64_t l3riscv_get_exit_pc()
{
  check_open();
  return 0;
}

void l3riscv_cpu_reset(uint64_t mem_base_addr, uint64_t mem_size)
{ assert(false); }

void l3riscv_cpu_write_pc(uint64_t pc)
{ assert(false); }

void l3riscv_cpu_write_gpr(uint8_t gpr, uint64_t val)
{ assert(false); }

void l3riscv_cpu_write_csr(uint16_t csr, uint64_t val)
{ assert(false); }

void l3riscv_cpu_write_mem32(uint64_t addr, uint32_t val)
{ assert(false); }

void l3riscv_cpu_write_fpr(uint8_t fpr, uint64_t val)
{ assert(false); }

void l3riscv_cpu_write_fsr(uint32_t fsr)
{ assert(false); }
