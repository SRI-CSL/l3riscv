/* These are for C_isa.h, taken from C_isa.c */
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
/* End prologue. */

#include <Cissr/C_isa_verify.h>
#include "riscv_oracle.h"

void reset_oracle (uint64_t mem_base, uint64_t mem_size)
{
  cissr_cpu_reset(mem_base, mem_size);
}

void load_oracle (const char *filename)
{
  c_load_elf(filename);
}

uint32_t call_oracle (uint32_t exc_taken,
                      uint64_t pc,
                      uint64_t addr,
                      uint64_t data1,
                      uint64_t data2,
                      uint64_t data3,
                      uint64_t fpdata)
{
  return !cissr_verify_instr(exc_taken, pc, addr, data1, data2, data3, fpdata, 2);
}
