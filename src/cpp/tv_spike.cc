#include "tv_spike.h"
#include <fesvr/elfloader.h>
#include <assert.h>
#include <iostream>

tv_spike_t::tv_spike_t(const char *isa)
  : memif(this), tohost_addr(0), fromhost_addr(0), debug_log(true)
{
  cpu = new processor_t(isa, this, /*hartid*/ 0, /*halted*/ false);
  debug_mmu = new mmu_t(this, /*processor_t**/NULL);
  /* use the default memory size for Spike: 2GB at DRAM_BASE */
  size_t size = reg_t(2048) << 20;
  mem_regions = std::vector<std::pair<reg_t, mem_t*>>(1, std::make_pair(reg_t(DRAM_BASE), new mem_t(size)));

  for (auto& x : mem_regions) {
    std::cout << "Adding mem device @0x" << std::hex << x.first
              << " size:0x" << x.second->size() << std::endl;
    bus.add_device(x.first, x.second);
  }
}

reg_t tv_spike_t::init_elf(const char *elf_file)
{
  std::map<std::string, uint64_t> symbols = load_elf(elf_file, &memif, &entry);
  if (symbols.count("tohost") && symbols.count("fromhost")) {
    tohost_addr = symbols["tohost"];
    fromhost_addr = symbols["fromhost"];
    std::cout << "tohost   <- 0x" << std::hex << tohost_addr   << std::endl;
    std::cout << "fromhost <- 0x" << std::hex << fromhost_addr << std::endl;
  } else {
    std::cerr << "warning: tohost and fromhost symbols not in ELF;"
              << "can't communicate with target" << std::endl;
  }

  return entry;
}

void tv_spike_t::set_pc_reg(uint64_t pc)
{
  cpu->get_state()->pc = pc;
}

void tv_spike_t::reset()
{
  reg_t start_pc = get_entry_point();
  const int reset_vec_size = 8;

  uint32_t reset_vec[reset_vec_size] = {
    0x297,                                      // auipc  t0,0x0
    0x28593 + (reset_vec_size * 4 << 20),       // addi   a1, t0, &dtb
    0xf1402573,                                 // csrr   a0, mhartid
    cpu->get_xlen() == 32 ?
      0x0182a283u :                             // lw     t0,24(t0)
      0x0182b283u,                              // ld     t0,24(t0)
    0x28067,                                    // jr     t0
    0,
    (uint32_t) (start_pc & 0xffffffff),
    (uint32_t) (start_pc >> 32)
  };

  std::vector<char> rom((char*)reset_vec, (char*)reset_vec + sizeof(reset_vec));
  boot_rom.reset(new rom_device_t(rom));
  std::cout << "Adding rom device @0x" << std::hex << DEFAULT_RSTVEC
            << " size:0x" << boot_rom.get()->contents().size() << std::endl;
  bus.add_device(DEFAULT_RSTVEC, boot_rom.get());

  cpu->set_debug(debug_log);
}

void tv_spike_t::step(void)
{
  cpu->step(1);
}

char* tv_spike_t::addr_to_mem(reg_t addr)
{
  auto desc = bus.find_device(addr);
  if (auto mem = dynamic_cast<mem_t*>(desc.second)) {
    if (addr - desc.first < mem->size())
      return mem->contents() + (addr - desc.first);
  }
  return NULL;
}

bool tv_spike_t::mmio_load(reg_t addr, size_t len, uint8_t* bytes)
{
  if (addr + len < addr)
    return false;
  return bus.load(addr, len, bytes);
}

bool tv_spike_t::mmio_store(reg_t addr, size_t len, const uint8_t* bytes)
{
  if (addr + len < addr)
    return false;
  return bus.store(addr, len, bytes);
}

void tv_spike_t::read_chunk(addr_t taddr, size_t len, void* dst)
{
  assert(len == 8);
  auto data = debug_mmu->load_uint64(taddr);
  memcpy(dst, &data, sizeof data);
}

void tv_spike_t::write_chunk(addr_t taddr, size_t len, const void* src)
{
  assert(len == 8);
  uint64_t data;
  memcpy(&data, src, sizeof data);
  debug_mmu->store_uint64(taddr, data);
}

bool tv_spike_t::exited(int& exit_code)
{
  // primitive htif to-host/from-host protocol
  uint64_t cmd = memif.read_uint64(tohost_addr);
  // using encodings found in:
  // . fesvr/device.h
  // . fesvr/syscall.cc:syscall_t::handle_syscall()
  // . isa-sim/htif.cc:htif_t::exit_code()
  uint64_t payload = cmd << 16 >> 16;
  if (payload & 1) { // test pass/fail
    exit_code = payload >> 1;
    return true;
  }
  return false;
}
