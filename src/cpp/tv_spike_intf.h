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


#ifndef _TV_SPIKE_H_
#define _TV_SPIKE_H_

#include <stdlib.h>
#include <inttypes.h>
#include "tv_spike.h"

#ifdef __cplusplus
extern "C" {
#endif

tv_spike_t* tv_init(const char *isa);

void tv_set_verbose(tv_spike_t* tvs, int enable);

void tv_load_elf(tv_spike_t* tvs, const char *filename);

void tv_reset(tv_spike_t *tvs);

void tv_step(tv_spike_t *tvs);

int  tv_is_done(tv_spike_t *tvs);

int  tv_check_priv(tv_spike_t *tvs, uint8_t prv);

int  tv_check_pc(tv_spike_t *tvs, uint64_t val);

int  tv_check_gpr(tv_spike_t *tvs, size_t regno, uint64_t val);

int  tv_check_csr(tv_spike_t *tvs, size_t regno, uint64_t val);

void tv_free(tv_spike_t *tvs);

#ifdef __cplusplus
}
#endif

#endif /* _TV_SPIKE_H_ */
