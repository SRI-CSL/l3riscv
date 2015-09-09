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

static int lib_is_opened = 0;

const char *dummy_argv[] = {
  "libl3riscv.so",
  "-v",
  "true",
  NULL
};

static void check_open() {
  if (!lib_is_opened) {
    l3riscv_open(3, dummy_argv);
    lib_is_opened = 1;
  }
}

void l3riscv_init() {
  check_open();
}

void l3riscv_done() {
  l3riscv_close();
}

void l3riscv_mem_load_elf() {
  check_open();
  (void) _l3r_load_elf();
}

uint64_t l3riscv_cpu_get_cpuid(uint64_t cpu) {
  fprintf(stdout, "l3riscv_cpu_get_cpuid: not implemented.\n");
  /* TODO: */
  return 0;
}

uint64_t l3riscv_mem_get_min_addr()
{
  check_open();
  return _l3r_get_mem_base();
}

uint64_t l3riscv_mem_get_max_addr() {
  check_open();

  uint64_t base = _l3r_get_mem_base();
  uint64_t size = _l3r_get_mem_size();
  return base + size;
}

/* _l3r_read_mem internally handles unaligned reads */

uint64_t l3riscv_mem_read_64(uint64_t mem_addr) {
  check_open();
  return _l3r_read_mem(mem_addr);
}

uint32_t l3riscv_mem_read_32(uint64_t mem_addr) {
  check_open();
  return (uint32_t) _l3r_read_mem(mem_addr);
}

uint32_t l3riscv_verify(uint64_t cpu,
                        uint32_t cmd,
                        uint32_t exc_taken,
                        uint64_t pc,
                        uint64_t addr,
                        uint64_t data1,
                        uint64_t data2,
                        uint64_t data3,
                        uint64_t fpdata,
                        uint32_t verbosity) {
  check_open();
  return _l3r_verify_instr(cpu, cmd, exc_taken, pc, addr,
                           data1, data2, data3, fpdata,
                           verbosity);
}
