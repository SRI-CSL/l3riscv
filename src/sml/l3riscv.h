#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#ifdef __APPLE__
#include <float.h>     // For Mac OS X
#else
#include <values.h>    // For Linux
#endif

/* This should be called before any other calls into l3riscv. */
void l3riscv_init();

/* This should be called at the end of the program. */
void l3riscv_done();

/* The filename is specified using the SIM_ELF_FILENAME environment variable. */
void l3riscv_mem_load_elf();

/* Use these interfaces to retrieve the memory ranges and contents of
   the loaded ELF file.
 */
uint64_t l3riscv_mem_get_min_addr();
uint64_t l3riscv_mem_get_max_addr();
uint32_t l3riscv_mem_read_32(uint64_t addr);

/* Currently undocumented interfaces to support debuggers and other uses. */

void l3riscv_cpu_write_pc(uint64_t pc);
void l3riscv_cpu_write_gpr(uint8_t gpr, uint64_t val);
void l3riscv_cpu_write_csr(uint16_t csr, uint64_t val);
void l3riscv_cpu_write_fpr(uint8_t fpr, uint64_t val);

/* This function should be called with information about every retired
   instruction.  The arguments are as follows:

   - cpu:  the identifier for the cpu
           -- currently unused, but will support multi-core verification

   - cmd:  the command to the verifier
           -- the only supported value is 0 to verify a retired instruction

   - exc_taken: non-zero when the retired instruction caused an exception, zero otherwise

   - pc:   PC for the retired instruction

   - addr:  address argument for instruction:
            --   new control flow target for jump, exception branch, ERET
            --   memory address for memory ops and AMOs
            --   CSR register address for CSR instructions

   - data1: data result for instruction:
            --   new value for rd for ALU ops, LOAD, LOAD_FP, LR, SC, CSR ops
            --   new csr_status for exceptions and ERET

   - data2: data argument for instruction:
            --   new csr_cause for exceptions
            --   new memory value for STORE, STORE_FP, SC, AMOs
            --   argument for CSR ops

   - fp_data: floating point value, currently unused

   - verbosity: for any generated tracing, currently unused

   Note that some of the state arguments are often undefined.  For e.g., after a
   fetch exception, only exc_taken and pc will be checked.

 */
uint32_t l3riscv_verify(uint64_t cpu,
                        uint32_t cmd,
                        uint32_t exc_taken,
                        uint64_t pc,
                        uint64_t addr,
                        uint64_t data1,
                        uint64_t data2,
                        uint64_t data3,
                        uint64_t fpdata,
                        uint32_t verbosity);
