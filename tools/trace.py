#!/usr/bin/python3

import sys

NREGS = 32

I_TYPE = 0
E_TYPE = 1

class instruction(object):
    def __init__(self):
        self.pc    = None
        self.mode  = None
        self.cnt   = None
        self.opc   = None
        self.instr = None
        self.typ   = None

        self.regs = {}
        for i in range(NREGS): self.regs[i] = None

    def __str__(self):
        return ("%s: %s" % (hex(self.pc), self.instr))

    def matches(self, ins):
        if (self.pc != ins.pc or self.mode != ins.mode or self.typ != ins.typ):
            print(" match failed: %s  vs  %s" % (self, ins))
            return False
        if self.opc != ins.opc:
            print(" match failed: %s  vs  %s" % (self, ins))
            return False
        return True


class instr_log(object):
    def __init__(self, f):
        self.t     = None
        self.file  = f
        self.user  = []
        self.supr  = []
        self.excp  = []
        self.trace = []

    def print_user(self):
        for i in self.user: print(i)

    def print_supr(self):
        for i in self.supr: print(i)

    def print_excp(self):
        for i in self.excp: print(i)

    def print_trace(self):
        for i in self.trace: print(i)

def guess_log(fname):
    with open(fname) as f:
        for l in f:
            if l.startswith('core '): return "SPIKE"
            elif l.startswith('instr '): return "L3"
        return ""

class spike_log(instr_log):
    def __init__(self, *args):
        super(spike_log, self).__init__(*args)
        self.t = 'SPIKE'

    def parse(self):
        ins = instruction()
        for l in self.file:
            l = l.strip()
            # check for last line of instruction
            if l.find("core") >= 0:
                if l.find("exception") >= 0:
                    wl = l.split(maxsplit=4)
                    ins.instr = "%s %s" % (wl[3], wl[4])
                    ins.typ = E_TYPE
                    self.excp.append(ins)
                else:
                    wl = l.split(maxsplit=6)
                    ins.instr = wl[6]
                    ins.typ   = I_TYPE
                    ins.mode  = wl[2]
                    if   ins.mode == 'U': self.user.append(ins)
                    elif ins.mode == 'S': self.supr.append(ins)
                    else: raise Exception("Unable to parse line '%s'" % l)
                self.trace.append(ins)
                ins = instruction()
                continue

            wl = l.split()
            if len(wl) == 0: continue
            if l.find("PC") >= 0: ins.pc = wl[2]
            elif len(wl) == 4:    ins.regs[int(wl[1])] = wl[3]
            else: print("Skipping unknown line '%s'" % l)

class l3_log(instr_log):
    def __init__(self, *args):
        super(l3_log, self).__init__(*args)
        self.t = 'L3'

    def parse(self):
        for l in self.file:
            l = l.strip()
            if l.find('instr ') == 0:
                wl = l.split(maxsplit=6)
                if len(wl) == 7:
                    ins = instruction()
                    ins.pc    = int(wl[3], 16)
                    ins.instr = wl[6]
                    ins.opc   = wl[6].split()[0]
                    self.user.append(ins)
                else:
                    print("unknown instr: %s" % l)

class cissr_log(instr_log):
    def __init__(self, *args):
        super(cissr_log, self).__init__(*args)
        self.t = 'CISSR'

    def parse(self):
        for l in self.file:
            if l.find("PC:") == 0:
                l   = l.strip()
                wl  = l.split()
                pc  = wl[1].strip(':')
                if wl[2] in ['BRANCH', 'STORE', 'LOAD', 'SYSTEM',
                             'OP', 'OP_IMM', 'OP_IMM_32']:
                    opc = wl[3]
                    i   = ' '.join(wl[3:])
                else:
                    opc = wl[2]
                    i   = ' '.join(wl[2:])
                ins = instruction()
                ins.pc    = int(pc, 16)
                ins.opc   = opc
                ins.instr = i
                self.user.append(ins)

def print_usage():
    print("%s: [-cissr|-spike|-l3] <log-file>" % sys.argv[0])

logs = {'-cissr' : cissr_log,
        '-spike' : spike_log,
        '-l3'    : l3_log}

def is_valid_log(arg):
    return (arg in logs.keys())

def parse_log(typ, fn):
    with open(fn) as fl:
        lg = logs[typ](fl)
        lg.parse()
        return lg

def compare_logs(lft, rgt):
    for c, (l, r) in enumerate(zip(lft.user, rgt.user)):
        if (l.pc != r.pc) or (l.opc != r.opc):
            print("Traces differ at instruction ", c)
            print("< ", lft.t, ':', l)
            print("> ", rgt.t, ':', r)
            return

def do_parse_log():
    if (not is_valid_log(sys.argv[1])) or (len(sys.argv) < 3):
        print_usage()
        return
    pl = parse_log(sys.argv[1], sys.argv[2])
    pl.print_user()

def do_cmp_log():
    if ((len(sys.argv) < 6)
        or (not is_valid_log(sys.argv[2]))
        or (not is_valid_log(sys.argv[4]))):
        print_usage()
        return
    lft = parse_log(sys.argv[2], sys.argv[3])
    rgt = parse_log(sys.argv[4], sys.argv[5])
    compare_logs(lft, rgt)

if __name__ == "__main__":
    if len(sys.argv) == 1:      print_usage()
    elif sys.argv[1] == "-cmp": do_cmp_log()
    else:                       do_parse_log()

