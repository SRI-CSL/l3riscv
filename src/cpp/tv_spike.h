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

#ifndef __TV_SPIKE_H_
#define __TV_SPIKE_H_

#include <inttypes.h>

#include <spike/config.h>
#include <spike/decode.h>
#include <spike/devices.h>
#include <spike/mmu.h>
#include <spike/sim.h>
#include <fesvr/memif.h>
#include <spike/dts.h>

/*
 * This class implements a single-CPU in-order RISC-V system, using the Spike
 * reference simulator as a backend.
 */

class tv_spike_t : public simif_t, public chunked_memif_t
{
public:
  tv_spike_t(const char *isa);
  virtual ~tv_spike_t();

  /* initialization */
  reg_t init_elf(const char *elf_file);
  void set_pc_reg(uint64_t pc);
  void reset(void);
  reg_t get_entry_point(void) { return entry; }

  /* config queries */
  int is_dirty_enabled();
  int is_misaligned_enabled();

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
  // callbacks
  void proc_reset(unsigned id) { }

  // misc
  bool exited(int& exit_code);

  // verification API
  bool set_verbose(bool enable);

  bool check_pc(uint64_t pc);
  bool check_gpr(size_t regno, uint64_t val);
  bool check_csr(size_t regno, uint64_t val);
  bool check_priv(uint8_t prv);
  //bool check_fpr(size_t regno, uint64_t val);

  /* platform info */
  static const size_t INSNS_PER_RTC_TICK = 100; // 10 MHz clock for 1 BIPS core
  static const size_t CPU_HZ = 1000000000; // 1GHz CPU
  std::string get_dts(void);
  std::string get_dtb(void);

private:
  std::vector<std::pair<reg_t, mem_t*>> mem_regions;
  mmu_t* debug_mmu; // used for initialization of memory regions
  processor_t *cpu;
  std::vector<processor_t*> procs; // contains the above singleton cpu
  memif_t memif;    // used by ELF loader
  std::unique_ptr<rom_device_t> boot_rom; // holds reset vector
  std::unique_ptr<clint_t> clint; // clock interface
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
};

#endif // __TV_SPIKE_H_
