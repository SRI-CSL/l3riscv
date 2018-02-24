all: mini_spike

HDRS = tv_spike.h
SRCS = tv_spike.cc mini_spike.cc
LIBS = -lfesvr -lriscv

mini_spike: $(HDRS) $(SRCS) Makefile
	g++ -Ispike -I$(RISCV)/include -o $@ $(SRCS) -L $(RISCV)/lib $(LIBS) -Wl,-rpath=$(RISCV)/lib

clean:
	-rm -f mini_spike
