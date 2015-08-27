#ifndef _TANDEM_VERIFICATION_H_
#define _TANDEM_VERIFICATION_H_

/* This API is based on the BSV Tandem Verification API being used for
 * the RISC-V processors from Bluespec, Inc.
 */

uint64_t bsv_tandem_mkCPU (uint64_t mem_base_addr, uint64_t mem_size);

void     bsv_tandem_load_elf2    (uint64_t   p_cpu_state, uint64_t bitwidth);
uint64_t bsv_tandem_get_exit_pc  (uint64_t   p_cpu_state);
uint64_t bsv_tandem_get_min_addr (uint64_t   p_cpu_state);
uint64_t bsv_tandem_get_max_addr (uint64_t   p_cpu_state);
uint32_t bsv_tandem_read_mem32   (uint64_t   p_cpu_state, uint64_t addr);

uint32_t bsv_tandem_verify (uint64_t    p_cpu_state,
                            uint32_t    cmd,
                            uint32_t    dut_exc_taken,
                            uint64_t    dut_pc,
                            uint64_t    dut_addr,
                            uint64_t    dut_data1,
                            uint64_t    dut_data2,
                            uint64_t    dut_data3,
                            uint64_t    dut_fpdata,
                            int         dut_verbosity);

#endif
