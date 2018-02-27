#ifndef __TV_SPIKE_H_
#define __TV_SPIKE_H_

#include <inttypes.h>

#include <spike/config.h>
#include <spike/decode.h>
#include <spike/devices.h>
#include <spike/mmu.h>
#include <spike/sim.h>
#include <fesvr/memif.h>

/*
 * This class implements a single-CPU in-order RISC-V system, using the Spike
 * reference simulator as a backend.
 */

class tv_spike_t : public simif_t, public chunked_memif_t
{
public:
  tv_spike_t(const char *isa);

  /* initialization */
  reg_t init_elf(const char *elf_file);
  void set_pc_reg(uint64_t pc);
  void reset(void);
  reg_t get_entry_point(void) { return entry; }

  /* execution */
  void step(void);

  /* riscv-fesvr chunked_memif_t interface */
  size_t chunk_align() { return 8; }
  size_t chunk_max_size() { return 8; }
  void read_chunk(addr_t taddr, size_t len, void* dst);
  void write_chunk(addr_t taddr, size_t len, const void* src);
  void clear_chunk(addr_t taddr, size_t len) {}

  /* riscv-isa-sim simif_t interface */

  // physical memory mapping: returns a pointer to the byte in the physical
  // memory model addressed by the physical address 'addr'.  NULL is returned if
  // 'addr' does not map to a valid physical memory range.
  char* addr_to_mem(reg_t addr);

  // MMIO operations:
  bool mmio_load(reg_t addr, size_t len, uint8_t* bytes);
  bool mmio_store(reg_t addr, size_t len, const uint8_t* bytes);

  // misc
  bool exited(int& exit_code);

  // verification API
  bool set_verbose(bool enable);

  bool check_pc(uint64_t pc);
  bool check_gpr(size_t regno, uint64_t val);
  bool check_csr(size_t regno, uint64_t val);
  bool check_priv(uint8_t prv);
  //bool check_fpr(size_t regno, uint64_t val);

private:
  std::vector<std::pair<reg_t, mem_t*>> mem_regions;
  mmu_t* debug_mmu; // used for initialization of memory regions
  processor_t *cpu;
  memif_t memif;    // used by ELF loader
  std::unique_ptr<rom_device_t> boot_rom; // holds reset vector
  bus_t bus;

  // used for riscv-tests
  addr_t tohost_addr;
  addr_t fromhost_addr;
  reg_t  entry;

  // verification API
  bool verbose_verify;

  // miscellaneous
  bool debug_log;

  // internal
  reg_t read_csr(size_t which);
  void  log_check(const char *regfile, size_t regno, uint64_t model_val, uint64_t val);
};

#endif // __TV_SPIKE_H_
