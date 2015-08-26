#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#ifdef __APPLE__
#include <float.h>     // For Mac OS X
#else
#include <values.h>    // For Linux
#endif

void l3riscv_init();
void l3riscv_done();

void l3riscv_mem_load_elf(const char *filename);

uint64_t l3riscv_mem_get_min_addr();
uint64_t l3riscv_mem_get_max_addr();
uint64_t l3riscv_mem_read_64(uint64_t mem_addr);
uint32_t l3riscv_mem_read_32(uint64_t mem_addr);
void     l3riscv_mem_write_32(uint64_t addr, uint32_t val);
void     l3riscv_mem_write_64(uint64_t addr, uint64_t val);

void l3riscv_cpu_reset(uint64_t mem_base_addr, uint64_t mem_size);
void l3riscv_cpu_write_pc(uint64_t pc);
void l3riscv_cpu_write_gpr(uint8_t gpr, uint64_t val);
void l3riscv_cpu_write_csr(uint16_t csr, uint64_t val);
void l3riscv_cpu_write_fpr(uint8_t fpr, uint64_t val);

uint32_t l3riscv_verify(uint32_t exc_taken,
                        uint64_t pc,
                        uint64_t addr,
                        uint64_t data1,
                        uint64_t data2,
                        uint64_t data3,
                        uint64_t fpdata,
                        uint32_t verbosity);
