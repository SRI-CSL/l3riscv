#include <assert.h>
#include <stdlib.h>

#include "Cissr_api.h"
#include "l3riscv.h"

void cissr_cpu_reset(uint64_t mem_base_addr, uint64_t mem_size) {
  l3riscv_cpu_reset(mem_base_addr, mem_size);
}

void c_load_elf2 (void) {
  l3riscv_mem_load_elf(NULL);
}

uint64_t c_get_min_addr (void) {
  return  l3riscv_mem_get_min_addr();
}

uint64_t c_get_max_addr (void) {
  return l3riscv_mem_get_max_addr();
}

uint64_t c_get_exit_pc (void) {
  return 0;
}

uint32_t c_read32 (uint64_t addr) {
  return l3riscv_mem_read_32(addr);
}

void cissr_write_pc(uint64_t pc) {
  l3riscv_cpu_write_pc(pc);
}

void cissr_write_gpr(uint8_t  gpr, uint64_t v) {
  l3riscv_cpu_write_gpr(gpr, v);
}

void cissr_write_csr(uint16_t csr, uint64_t v) {
  l3riscv_cpu_write_csr(csr, v);
}

void cissr_write_fpr(uint8_t fpr, uint64_t v) {
  l3riscv_cpu_write_fpr(fpr, v);
}

void cissr_write_fsr(uint64_t fsr) {
  assert(false);
}

void cissr_write_mem32(uint64_t addr, uint32_t v) {
  assert(false);
}

uint32_t cissr_verify_instr(uint32_t dut_exc_taken,
                            uint64_t dut_pc,
                            uint64_t dut_addr,
                            uint64_t dut_data1,
                            uint64_t dut_data2,
                            uint64_t dut_data3,
                            uint64_t dut_fpdata,
                            uint32_t verbosity)
{
  return l3riscv_verify(dut_exc_taken,
                        dut_pc,
                        dut_addr,
                        dut_data1,
                        dut_data2,
                        dut_data3,
                        dut_fpdata,
                        verbosity);
}


