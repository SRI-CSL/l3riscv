#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#ifdef __APPLE__
#include <float.h>     // For Mac OS X
#else
#include <values.h>    // For Linux
#endif
#include <assert.h>
#include <stdlib.h>

#include "TandemVerification_api.h"
#include "l3riscv.h"

uint64_t bsv_tandem_mkCPU(uint64_t mem_base_addr, uint64_t mem_size) {
  /* No-op currently. */
  return 0;
}

void     bsv_tandem_load_elf2    (uint64_t   p_cpu_state, uint64_t bitwidth) {
  l3riscv_mem_load_elf(NULL);
  /* TODO: Check bitwidth. */
}

uint64_t bsv_tandem_get_exit_pc  (uint64_t   p_cpu_state) {
  /* TODO: */
  return 0;
}

uint64_t bsv_tandem_get_min_addr (uint64_t   p_cpu_state) {
  return  l3riscv_mem_get_min_addr();
}

uint64_t bsv_tandem_get_max_addr (uint64_t   p_cpu_state) {
  return l3riscv_mem_get_max_addr();
}

uint32_t bsv_tandem_read_mem32   (uint64_t   p_cpu_state, uint64_t addr) {
  return l3riscv_mem_read_32(addr);
}

uint32_t bsv_tandem_verify (uint64_t    p_cpu_state,
                            uint32_t    cmd,
                            uint32_t    dut_exc_taken,
                            uint64_t    dut_pc,
                            uint64_t    dut_addr,
                            uint64_t    dut_data1,
                            uint64_t    dut_data2,
                            uint64_t    dut_data3,
                            uint64_t    dut_fpdata,
                            int         dut_verbosity) {
  return l3riscv_verify(p_cpu_state,
                        cmd,
                        dut_exc_taken,
                        dut_pc,
                        dut_addr,
                        dut_data1,
                        dut_data2,
                        dut_data3,
                        dut_fpdata,
                        dut_verbosity);
}
