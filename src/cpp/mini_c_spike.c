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

/* A lightweight C-based spike wrapper to test the C bindings. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "tv_spike_intf.h"

uint64_t ram_size = (64LL << 20);

void run_elf(const char *isa, const char *filename)
{
  struct tv_spike_t *s = tv_init(isa, ram_size, 1);
  tv_set_verbose(s, 1);
  tv_set_dtb_in_rom(s, 1);
  tv_load_elf(s, filename);
  tv_reset(s);

  { size_t insns_per_tick = tv_get_insns_per_tick(s);
    size_t cnt = 0;
    while (!tv_is_done(s)) {
      tv_step(s);
      cnt++;
      if (cnt == insns_per_tick) {
        tv_tick_clock(s);
        tv_step_io(s);
      }
    }
  }
  tv_free(s);
}

void print_usage(const char *argv0)
{
  fprintf(stderr, "Usage: %s <elf_file>\n", argv0);
  exit(0);
}

void print_dts(void)
{
  size_t dts_len = 0;
  struct tv_spike_t *s = tv_init("RV64IMAC", ram_size, 0);
  tv_get_dts(s, NULL, &dts_len);
  if (dts_len > 0) {
    unsigned char *dts = (unsigned char *)malloc(dts_len + 1);
    dts[dts_len] = '\0';
    tv_get_dts(s, dts, &dts_len);
    fprintf(stdout, "%s\n", dts);
  }
  exit(0);
}

int is_file(const char *f)
{
  struct stat st;
  return (!stat(f, &st) && (st.st_mode & S_IFMT) == S_IFREG);
}

int main(int argc, const char **argv)
{
  if (argc != 2)         print_usage(argv[0]);
  if (!is_file(argv[1])) print_usage(argv[0]);

  run_elf("RV64IMAFDC", argv[1]);
}
