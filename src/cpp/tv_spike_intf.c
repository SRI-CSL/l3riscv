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

tv_spike_t* tv_init(const char *isa)
{
  tv_spike_t *tvs = new tv_spike_t(isa);
  fprintf(stderr, "%s(%s)\n", __func__, isa);
  return tvs;
}

void tv_set_verbose(tv_spike_t* tvs, int enable)
{
  fprintf(stderr, "%s(%d)\n", __func__, enable);
  tvs->set_verbose(enable);
}

void tv_load_elf(tv_spike_t* tvs, const char *filename)
{
  reg_t entry = tvs->init_elf(filename);
  fprintf(stderr, "%s(%s): %0" PRIx64 "\n",
          __func__, filename, entry);
}

void tv_reset(tv_spike_t* tvs)
{
  fprintf(stderr, "%s()\n", __func__);
  tvs->reset();
}

void tv_step(tv_spike_t* tvs)
{
  fprintf(stderr, "%s()\n", __func__);
  tvs->step();
}

int tv_is_done(tv_spike_t* tvs)
{
  int exit_code;
  return tvs->exited(exit_code);
}

int tv_check_priv(tv_spike_t* tvs, uint8_t priv)
{
  return tvs->check_priv(priv);
}

int tv_check_pc(tv_spike_t* tvs, uint64_t val)
{
  return tvs->check_pc(val);
}

int tv_check_gpr(tv_spike_t* tvs, size_t regno, uint64_t val)
{
  return tvs->check_gpr(regno, val);
}

int tv_check_csr(tv_spike_t* tvs, size_t regno, uint64_t val)
{
  return tvs->check_csr(regno, val);
}

void tv_free(tv_spike_t *tvs)
{
  fprintf(stderr, "%s(%p)\n", __func__, tvs);
  delete tvs;
}
