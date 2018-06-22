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

#include <unistd.h>
#include <fesvr/option_parser.h>
#include "tv_spike.h"

static int run(tv_spike_t *tv)
{
  int insns = 0;
  int code;
  tv->reset();
  while(!tv->exited(code)) {
    tv->step();
    tv->step_io();

    if (++insns == tv->INSNS_PER_RTC_TICK) {
      tv->tick(1);
      insns = 0;
    }
  }
  fprintf(stderr, "Exited with code %d.\n", code);
  return code;
}

static void run_elf(const char *isa, const char *file)
{
  tv_spike_t s(isa);
  s.dtb_in_rom(true);
  s.init_elf(file);
  exit(run(&s));
}

static void help()
{
  fprintf(stderr, "Usage: mini_spike [--isa=<isa>] [--dump-dts] [--dump-dtb] [--show-isa] <elf_file>\n");
  exit(0);
}

static void print_dts(const char *isa)
{
  tv_spike_t s(isa);
  fprintf(stdout, "%s", s.get_dts().c_str());
}

static void print_dtb(const char *isa)
{
  tv_spike_t s(isa);
  std::string dtb = s.get_dtb();
  write(1, dtb.c_str(), dtb.length());
}

static void print_isa(const char *isa)
{
  fprintf(stdout, "%s\n", isa);
}

int main(int argc, char **argv)
{
  bool dump_dts = false;
  bool dump_dtb = false;
  bool show_isa = false;

  const char *isa = "RV64IMAFDC";
  option_parser_t parser;
  parser.help(&help);

  parser.option('h', 0, 0, [&](const char* s){help();});
  parser.option(0, "dump-dts", 0, [&](const char *s){dump_dts = true;});
  parser.option(0, "dump-dtb", 0, [&](const char *s){dump_dtb = true;});
  parser.option(0, "show-isa", 0, [&](const char *s){show_isa = true;});
  parser.option(0, "isa", 1, [&](const char* s){isa = s;});
  const char* const* file = parser.parse(argv);

  if      (dump_dts) print_dts(isa);
  else if (dump_dtb) print_dtb(isa);
  else if (show_isa) print_isa(isa);
  else if (file && file[0] && file[0][0])
    run_elf(isa, file[0]);
  else help();
}
