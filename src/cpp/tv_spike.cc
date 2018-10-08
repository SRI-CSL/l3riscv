/*-
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2018 SRI International.
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA/AFRL contract FA8650-18-C-7809 ("CIFV").
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "tv_spike.h"
#include <spike/disasm.h>
#include <fesvr/elfloader.h>
#include <assert.h>
#include <iostream>

tv_spike_t::tv_spike_t(const char *isa, bool debug)
  : memif(this), tohost_addr(0), fromhost_addr(0), dtb_inited(false),
    entry(-1), has_exited(false), exit_code(0),
    verbose_verify(true), insert_dts(false), debug_log(debug)
{
  cpu = new processor_t(isa, this, /*hartid*/ 0, /*halted*/ false);
  procs = std::vector<processor_t*>(1, cpu);

  debug_mmu = new mmu_t(this, /*processor_t**/NULL);
  /* use the default memory size for Spike: 2GB at DRAM_BASE */
  size_t size = reg_t(2048) << 20;
  dram = new mem_t(size);
  mem_regions = std::vector<std::pair<reg_t, mem_t*>>(1, std::make_pair(reg_t(DRAM_BASE), dram));

  for (auto& x : mem_regions) {
    if (debug_log)
      std::cerr << "Adding mem device @0x" << std::hex << x.first
                << " size:0x" << x.second->size() << std::endl;
    bus.add_device(x.first, x.second);
  }
  /* MMIO */
  clint.reset(new clint_t(procs));
  bus.add_device(CLINT_BASE, clint.get());

  /* HTIF: devices need to be registered in the same order as Spike, since htif
   * device identifiers in commands are based on the registration index.  Use a
   * null-device for now instead of the syscall-proxy.
   */
  device_list.register_device(&nulld);
  device_list.register_device(&bcd);
}

tv_spike_t::~tv_spike_t()
{
  delete dram;
  delete debug_mmu;
  delete cpu;
}

int tv_spike_t::is_dirty_enabled()
{
  return cpu->get_mmu()->is_dirty_enabled();
}

int tv_spike_t::is_misaligned_enabled()
{
  return cpu->get_mmu()->is_misaligned_enabled();
}

void tv_spike_t::dtb_in_rom(bool enable)
{
  insert_dts = enable;
}

reg_t tv_spike_t::init_elf(const char *elf_file)
{
  std::map<std::string, uint64_t> symbols = load_elf(elf_file, &memif, &entry);
  if (debug_log) std::cerr << " loading " << elf_file << std::endl;
  if (symbols.count("tohost") && symbols.count("fromhost")) {
    tohost_addr = symbols["tohost"];
    fromhost_addr = symbols["fromhost"];
    if (debug_log) {
      std::cerr << "tohost   <- 0x" << std::hex << tohost_addr   << std::endl;
      std::cerr << "fromhost <- 0x" << std::hex << fromhost_addr << std::endl;
    }
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

void tv_spike_t::setup_dtb()
{
  dtb_inited = true;
  dts = make_dts(INSNS_PER_RTC_TICK, CPU_HZ, procs, mem_regions);
  dtb = dts_compile(dts);
}

const std::string tv_spike_t::get_dts()
{
  if (!dtb_inited) setup_dtb();
  return dts;
}

const std::string tv_spike_t::get_dtb()
{
  if (!dtb_inited) setup_dtb();
  return dtb;
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

  if (insert_dts) {
    /* Imitate the spike platform. */
    rom.insert(rom.end(), dtb.begin(), dtb.end());
    if (debug_log)
      std::cerr << "Inserted platform dtb into rom." << std::endl;
  }
  const int align = 0x1000;
  int old_rom_size = rom.size();
  rom.resize((rom.size() + align - 1) / align * align);

  boot_rom.reset(new rom_device_t(rom));
  if (debug_log) {
    std::cerr << "Adding rom device @0x" << std::hex << DEFAULT_RSTVEC
              << " size:0x" << boot_rom.get()->contents().size()
              << " (resized from 0x" << old_rom_size << ")"
              << std::endl;
  }

  bus.add_device(DEFAULT_RSTVEC, boot_rom.get());
  cpu->set_debug(debug_log);
  debug_mmu->set_debug(debug_log);
}

void tv_spike_t::step(size_t steps)
{
  for (size_t i = 0; (i < steps) && !has_exited; i++) {
    cpu->step(1);
    check_exited();
  }
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
  bool mmio = bus.load(addr, len, bytes);
  return mmio;
}

bool tv_spike_t::mmio_store(reg_t addr, size_t len, const uint8_t* bytes)
{
  if (addr + len < addr)
    return false;
  bool mmio = bus.store(addr, len, bytes);
  return mmio;
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

void tv_spike_t::clear_chunk(addr_t taddr, size_t len)
{
  char zeros[chunk_max_size()];
  memset(zeros, 0, chunk_max_size());

  for (size_t pos = 0; pos < len; pos += chunk_max_size())
    write_chunk(taddr + pos, std::min(len - pos, chunk_max_size()), zeros);
}

void tv_spike_t::step_io(void)
{
  cpu->yield_load_reservation();
  uint64_t tohost = memif.read_uint64(tohost_addr);
  if (debug_log) std::cerr << "htif::tick 0x" << std::hex << tohost << std::endl;
  if (tohost) {
    auto enq_func = [](std::queue<reg_t>* q, uint64_t x) { q->push(x); };
    std::function<void(reg_t)> fromhost_callback =
      std::bind(enq_func, &fromhost_queue, std::placeholders::_1);

    command_t cmd(memif, tohost, fromhost_callback);
    memif.write_uint64(tohost_addr, 0);
    device_list.handle_command(cmd);
    { /* Special handling of the exit code for riscv-tests, since this is
       * normally handled by the syscall-proxy device, and we have a
       * null-device instead.
       */
      if (cmd.device() == 0 && cmd.payload() & 1) {
        has_exited = true;
        exit_code = cmd.payload() >> 1;
      }
    }
  }
  device_list.tick();
}

void tv_spike_t::tick(reg_t inc)
{
  clint->increment(inc);
}

void tv_spike_t::check_exited(void)
{
  uint64_t tohost = memif.read_uint64(tohost_addr);
  uint8_t dev_cmd = tohost >> 48;
  uint64_t payload = tohost << 16 >> 16;
  if ((dev_cmd == 0) && (payload & 1)) {
    has_exited = true;
    exit_code = payload >> 1;
  }
}

bool tv_spike_t::exited(int& code)
{
  if (has_exited) code = exit_code;
  return has_exited;
}

bool tv_spike_t::set_verbose(bool enable)
{
  bool prev = verbose_verify;
  verbose_verify = enable;
  return prev;
}

bool tv_spike_t::check_pc(uint64_t val)
{
  uint64_t model_val = cpu->get_state()->pc;
  bool chk = model_val == val;

  if (verbose_verify && !chk)
    fprintf(stderr, " PC: expected %0" PRIx64 " got %" PRIx64 "\n",
            model_val, val);
  return chk;
}

bool tv_spike_t::check_gpr(size_t regno, uint64_t val)
{
  uint64_t model_val = uint64_t(-1);
  bool chk = false;
  if (regno < NXPR) {
    model_val = cpu->get_state()->XPR[regno];
    chk = model_val == val;
  }
  if (verbose_verify && !chk)
    fprintf(stderr, " GPR reg %ld: expected %0" PRIx64 " got %" PRIx64 "\n",
            regno, model_val, val);
  return chk;
}

bool tv_spike_t::check_csr(size_t regno, uint64_t val)
{
  uint64_t model_val = read_csr(regno);
  bool chk = model_val == val;
  if (verbose_verify && !chk)
    fprintf(stderr, " CSR reg %lx (%s): expected %0" PRIx64 " got %" PRIx64 "\n",
            regno, csr_name(regno), model_val, val);
  return chk;
}

bool tv_spike_t::check_priv(uint8_t val)
{
  uint8_t model_val = cpu->get_state()->prv;
  bool chk = model_val == val;
  if (verbose_verify && !chk)
    fprintf(stderr, " PRIV: expected %0x got %0x\n",
            model_val, val);
  return chk;
}


// This will need to be kept manually in sync with upstream, until we can get a
// suitable api/patch accepted.
reg_t tv_spike_t::read_csr(size_t which)
{
  unsigned xlen = cpu->get_xlen();
  unsigned max_xlen = cpu->get_max_xlen();
  state_t* state = cpu->get_state();
  reg_t isa = state->misa;

  if (which >= CSR_HPMCOUNTER3 && which <= CSR_HPMCOUNTER31)
    return 0;
  if (xlen == 32 && which >= CSR_HPMCOUNTER3H && which <= CSR_HPMCOUNTER31H)
    return 0;
  if (which >= CSR_MHPMCOUNTER3 && which <= CSR_MHPMCOUNTER31)
    return 0;
  if (cpu->get_xlen() == 32 && which >= CSR_MHPMCOUNTER3H && which <= CSR_MHPMCOUNTER31H)
    return 0;
  if (which >= CSR_MHPMEVENT3 && which <= CSR_MHPMEVENT31)
    return 0;
  switch (which) {
  case CSR_FFLAGS:
    return state->fflags;
  case CSR_FRM:
    return state->frm;
  case CSR_FCSR:
    return (state->fflags << FSR_AEXC_SHIFT) | (state->frm << FSR_RD_SHIFT);
  case CSR_INSTRET:
  case CSR_CYCLE:
    return state->minstret;
  case CSR_MINSTRET:
  case CSR_MCYCLE:
    return state->minstret;
  case CSR_INSTRETH:
  case CSR_CYCLEH:
    return state->minstret >> 32;
  case CSR_MINSTRETH:
  case CSR_MCYCLEH:
    return state->minstret >> 32;
  case CSR_SCOUNTEREN: return state->scounteren;
  case CSR_MCOUNTEREN: return state->mcounteren;
  case CSR_SSTATUS: {
    reg_t mask = SSTATUS_SIE | SSTATUS_SPIE | SSTATUS_SPP | SSTATUS_FS
                 | SSTATUS_XS | SSTATUS_SUM | SSTATUS_UXL;
    reg_t sstatus = state->mstatus & mask;
    if ((sstatus & SSTATUS_FS) == SSTATUS_FS ||
        (sstatus & SSTATUS_XS) == SSTATUS_XS)
      sstatus |= (xlen == 32 ? SSTATUS32_SD : SSTATUS64_SD);
    return sstatus;
  }
  case CSR_SIP: return state->mip & state->mideleg;
  case CSR_SIE: return state->mie & state->mideleg;
  case CSR_SEPC: return state->sepc;
  case CSR_STVAL: return state->stval;
  case CSR_STVEC: return state->stvec;
  case CSR_SCAUSE:
    if (max_xlen > xlen)
      return state->scause | ((state->scause >> (max_xlen-1)) << (xlen-1));
    return state->scause;
  case CSR_SATP:
    return state->satp;
  case CSR_SSCRATCH: return state->sscratch;
  case CSR_MSTATUS: return state->mstatus;
  case CSR_MIP: return state->mip;
  case CSR_MIE: return state->mie;
  case CSR_MEPC: return state->mepc;
  case CSR_MSCRATCH: return state->mscratch;
  case CSR_MCAUSE: return state->mcause;
  case CSR_MTVAL: return state->mtval;
  case CSR_MISA: return isa;
  case CSR_MARCHID: return 0;
  case CSR_MIMPID: return 0;
  case CSR_MVENDORID: return 0;
  case CSR_MHARTID: return 0;
  case CSR_MTVEC: return state->mtvec;
  case CSR_MEDELEG: return state->medeleg;
  case CSR_MIDELEG: return state->mideleg;
  case CSR_TSELECT: return state->tselect;
  case CSR_TDATA1:
      if (state->tselect < state->num_triggers) {
        reg_t v = 0;
        mcontrol_t *mc = &state->mcontrol[state->tselect];
        v = set_field(v, MCONTROL_TYPE(xlen), mc->type);
        v = set_field(v, MCONTROL_DMODE(xlen), mc->dmode);
        v = set_field(v, MCONTROL_MASKMAX(xlen), mc->maskmax);
        v = set_field(v, MCONTROL_SELECT, mc->select);
        v = set_field(v, MCONTROL_TIMING, mc->timing);
        v = set_field(v, MCONTROL_ACTION, mc->action);
        v = set_field(v, MCONTROL_CHAIN, mc->chain);
        v = set_field(v, MCONTROL_MATCH, mc->match);
        v = set_field(v, MCONTROL_M, mc->m);
        v = set_field(v, MCONTROL_H, mc->h);
        v = set_field(v, MCONTROL_S, mc->s);
        v = set_field(v, MCONTROL_U, mc->u);
        v = set_field(v, MCONTROL_EXECUTE, mc->execute);
        v = set_field(v, MCONTROL_STORE, mc->store);
        v = set_field(v, MCONTROL_LOAD, mc->load);
        return v;
      } else {
        return 0;
      }
      break;
  case CSR_TDATA2:
    if (state->tselect < state->num_triggers) {
      return state->tdata2[state->tselect];
    } else {
      return 0;
    }
    break;
  case CSR_TDATA3: return 0;
  case CSR_DCSR:
    {
      uint32_t v = 0;
      v = set_field(v, DCSR_XDEBUGVER, 1);
      v = set_field(v, DCSR_EBREAKM, state->dcsr.ebreakm);
      v = set_field(v, DCSR_EBREAKH, state->dcsr.ebreakh);
      v = set_field(v, DCSR_EBREAKS, state->dcsr.ebreaks);
      v = set_field(v, DCSR_EBREAKU, state->dcsr.ebreaku);
      v = set_field(v, DCSR_STOPCYCLE, 0);
      v = set_field(v, DCSR_STOPTIME, 0);
      v = set_field(v, DCSR_CAUSE, state->dcsr.cause);
      v = set_field(v, DCSR_STEP, state->dcsr.step);
      v = set_field(v, DCSR_PRV, state->dcsr.prv);
      return v;
    }
  case CSR_DPC:
    return state->dpc;
  case CSR_DSCRATCH:
    return state->dscratch;
  }
  return reg_t(-1);
}
