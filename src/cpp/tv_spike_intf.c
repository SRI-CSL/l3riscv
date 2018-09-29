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

#include <stdio.h>
#include "tv_spike_intf.h"
#include "tv_spike.h"

struct tv_spike_t* tv_init(const char *isa)
{
  tv_spike_t *tvs = new tv_spike_t(isa);
  fprintf(stderr, "%s(%s)\n", __func__, isa);
  return tvs;
}

int tv_get_dts(struct tv_spike_t *tvs, unsigned char *dts_buf, size_t *len_p)
{
  if (!len_p) return -1;

  const std::string dts = tvs->get_dts();
  if (*len_p < dts.length() || dts_buf == NULL) {
    *len_p = dts.length();
    return -1;
  }

  *len_p = dts.length();
  if (dts_buf) memcpy(dts_buf, (const unsigned char *)dts.c_str(), *len_p);
  return 0;
}

int tv_get_dtb(struct tv_spike_t *tvs, unsigned char *dtb_buf, size_t *len_p)
{
  if (!len_p) return -1;

  const std::string dtb = tvs->get_dtb();
  if (*len_p < dtb.length() || dtb_buf == NULL) {
    *len_p = dtb.length();
    return -1;
  }

  *len_p = dtb.length();
  if (dtb_buf) memcpy(dtb_buf, (const unsigned char *)dtb.c_str(), *len_p);
  return 0;
}

size_t tv_get_insns_per_tick(struct tv_spike_t* tvs)
{
  return tvs->INSNS_PER_RTC_TICK;
}

void tv_set_verbose(struct tv_spike_t* tvs, int enable)
{
  fprintf(stderr, "%s(%d)\n", __func__, enable);
  tvs->set_verbose(enable);
}

void tv_set_dtb_in_rom(struct tv_spike_t* tvs, int enable)
{
  tvs->dtb_in_rom(enable);
}

int tv_is_dirty_enabled(struct tv_spike_t* tvs)
{
  return tvs->is_dirty_enabled();
}

int tv_is_misaligned_enabled(struct tv_spike_t* tvs)
{
  return tvs->is_misaligned_enabled();
}

void tv_load_elf(struct tv_spike_t* tvs, const char *filename)
{
  reg_t entry = tvs->init_elf(filename);
  fprintf(stderr, "%s(%s): %0" PRIx64 "\n",
          __func__, filename, entry);
}

void tv_reset(struct tv_spike_t* tvs)
{
  fprintf(stderr, "%s()\n", __func__);
  tvs->reset();
}

void tv_set_pc(struct tv_spike_t *tvs, uint64_t pc)
{
  fprintf(stderr, "%s()\n", __func__);
  tvs->set_pc_reg(pc);
}

void tv_step(struct tv_spike_t* tvs)
{
  fprintf(stderr, "%s()\n", __func__);
  tvs->step(1);
}

void tv_step_io(struct tv_spike_t* tvs)
{
  fprintf(stderr, "%s()\n", __func__);
  tvs->step_io();
}

void tv_tick_clock(struct tv_spike_t* tvs)
{
  fprintf(stderr, "%s()\n", __func__);
  tvs->tick(1);
}

int tv_is_done(struct tv_spike_t* tvs)
{
  int exit_code;
  return tvs->exited(exit_code);
}

int tv_check_priv(struct tv_spike_t* tvs, uint8_t priv)
{
  return tvs->check_priv(priv);
}

int tv_check_pc(struct tv_spike_t* tvs, uint64_t val)
{
  return tvs->check_pc(val);
}

int tv_check_gpr(struct tv_spike_t* tvs, size_t regno, uint64_t val)
{
  return tvs->check_gpr(regno, val);
}

int tv_check_csr(struct tv_spike_t* tvs, size_t regno, uint64_t val)
{
  return tvs->check_csr(regno, val);
}

void tv_free(struct tv_spike_t *tvs)
{
  fprintf(stderr, "%s(%p)\n", __func__, tvs);
  delete tvs;
}
