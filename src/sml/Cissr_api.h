#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#ifdef __APPLE__
#include <float.h>     // For Mac OS X
#else
#include <values.h>    // For Linux
#endif

void cissr_cpu_reset(uint64_t mem_base_addr, uint64_t mem_size);

void c_load_elf2 (void);

uint64_t c_get_min_addr (void);
uint64_t c_get_max_addr (void);
uint64_t c_get_exit_pc (void);
uint32_t c_read32 (uint64_t addr);

void cissr_write_pc(uint64_t pc);
void cissr_write_gpr(uint8_t  gpr, uint64_t v);
void cissr_write_csr(uint16_t csr, uint64_t v);
void cissr_write_fpr(uint8_t  fpr, uint64_t v);
void cissr_write_fsr(uint64_t fsr);

void cissr_write_mem32(uint64_t addr, uint32_t v);

uint32_t cissr_verify_instr(uint32_t dut_exc_taken,
                            uint64_t dut_pc,
                            uint64_t dut_addr,
                            uint64_t dut_data1,
                            uint64_t dut_data2,
                            uint64_t dut_data3,
                            uint64_t dut_fpdata,
                            uint32_t verbosity);

