########################################
# Makefile for the L3 RISCV simulator ##
########################################

TOPDIR=$(shell realpath .)

L3SRCDIR=src/l3
L3SRCBASE+=riscv-print.spec
L3SRCBASE+=riscv.spec
L3SRC=$(patsubst %, $(L3SRCDIR)/%, $(L3SRCBASE))
L3LIBDIR?=$(shell l3 --lib-path)

# sml lib sources
#######################################
SMLSRCDIR=src/sml
SMLLIBDIR=src/sml/lib
SMLLIBSRC=Elf.sig Elf.sml
SMLLIB=$(patsubst %, $(SMLLIBDIR)/%, $(SMLLIBSRC))

# generating the sml source list
#######################################
SMLSRCBASE+=riscv.sig riscv.sml
SMLSRCBASE+=model.sml mlton_run.sml poly_run.sml oracle.sig poly_spike.sml poly_model.sml
SMLSRCBASE+=l3riscv.mlb
MLBFILE=l3riscv.mlb
SMLSRC=$(patsubst %, $(SMLSRCDIR)/%, $(SMLSRCBASE))

# generating the IL source
#######################################
ILSRCDIR=src/il

# generating the HOL source
#######################################
HOLSRCDIR=src/hol

# MLton compiler options
#######################################
MLTON          = mlton
MLTON_OPTS     = -inline 1000 -default-type intinf -verbose 1
MLTON_OPTS    += -default-ann 'allowFFI true' -export-header ${SMLSRCDIR}/riscv_ffi.h
MLTON_LIB_OPTS = -mlb-path-var 'L3LIBDIR '$(L3LIBDIR)

# PolyML compiler options
#######################################
POLYC = polyc

# Spike-based Tandem Verification library
#######################################
# Set ENABLE_TVSPIKE to 1 to enable verification against Spike.
# Ensure that you have the RISCV environment variable correctly
# set as when working with riscv-tools.
ENABLE_TVSPIKE  = 1
CSRCDIR=src/cpp
TVSPIKE_SRCBASE = tv_spike_intf.h tv_spike_intf.c tv_spike.h tv_spike.cc
TVSPIKE_SRC     = $(patsubst %, $(CSRCDIR)/%, $(TVSPIKE_SRCBASE))
TVSPIKE_INC     = -I $(CSRCDIR)
TVSPIKE_INC    += -I $(RISCV)/include
TVSPIKE_LIBS    = -L $(RISCV)/lib -lfesvr -lriscv -Wl,-rpath=$(RISCV)/lib
CSPIKE_INC      = -I $(CSRCDIR)
CSPIKE_LIBS     = -L $(TOPDIR) -ltv_spike -Wl,-rpath=$(TOPDIR)
CFLAGS          = -g -Wall -Wextra -Wno-unused-parameter
# One of the below options may be needed on some platforms
#CCOPTS         =
CCOPTS          = -std=c++11
#CCOPTS         = -std=gnu++11

# make targets
#######################################

all: l3riscv.poly ilspec holspec c
c:   libtv_spike.so mini_spike mini_c_spike

.PHONY: c c_clean clean

ifeq ($(ENABLE_TVSPIKE),1)
all: libtv_spike.so mini_spike mini_c_spike
endif

${SMLSRCDIR}/riscv.sig ${SMLSRCDIR}/riscv.sml: ${L3SRC}
	echo 'SMLExport.spec ("${L3SRC}", "${SMLSRCDIR}/riscv intinf")' | l3

ifeq ($(ENABLE_TVSPIKE),1)
l3riscv.poly: libtv_spike.so
endif
l3riscv.poly: ${SMLLIB} ${SMLSRC} Makefile
	$(POLYC) -o $@ ${SMLSRCDIR}/poly_run.sml

# l3riscv.mlton: ${SMLLIB} ${SMLSRC} Makefile
# 	$(MLTON) $(MLTON_OPTS) \
#               $(MLTON_LIB_OPTS) \
#               -output $@ ${SMLSRCDIR}/$(MLBFILE) $(L3LIBDIR)/sse_float.c $(L3LIBDIR)/mlton_sse_float.c

libtv_spike.so: ${TVSPIKE_SRC} Makefile
	g++ ${CFLAGS} ${CCOPTS} -o $@ -shared -fPIC ${TVSPIKE_INC} ${TVSPIKE_LIBS} ${TVSPIKE_SRC}

mini_spike: ${CSRCDIR}/mini_spike.cc ${TVSPIKE_SRC} Makefile
	g++ ${CFLAGS} ${CCOPTS} ${TVSPIKE_INC} -o $@ $< ${TVSPIKE_SRC} ${TVSPIKE_LIBS}

mini_c_spike: ${CSRCDIR}/mini_c_spike.c ${TVSPIKE_SRC} libtv_spike.so Makefile
	gcc ${CFLAGS} -o $@ $< ${CSPIKE_INC} ${CSPIKE_LIBS} ${TVSPIKE_LIBS}

#libl3riscv.so: ${SMLLIB} ${SMLSRC} Makefile
#	$(MLTON) $(MLTON_OPTS) \
#              $(MLTON_LIB_OPTS) \
#              -format library \
#              -output $@ ${SMLSRCDIR}/$(MLBFILE) ${SMLSRCDIR}/riscv_cissr.c ${SMLSRCDIR}/riscv_oracle.c $(L3LIBDIR)/sse_float.c $(L3LIBDIR)/mlton_sse_float.c

ilspec: ${L3SRC}
	mkdir -p $(ILSRCDIR)
	echo 'ILExport.spec ("${L3SRC}", "${ILSRCDIR}/riscv")' | l3

holspec: ${L3SRC}
	mkdir -p $(HOLSRCDIR)
	echo 'HolExport.spec ("${L3SRC}", "${HOLSRCDIR}/riscv")' | l3

c_clean:
	rm -f libtv_spike.so mini_spike mini_c_spike libl3riscv.so

clean: c_clean
	rm -f l3riscv.poly l3riscv.mlton
	rm -f ${SMLSRCDIR}/riscv.sig ${SMLSRCDIR}/riscv.sml
	rm -f ${ILSRCDIR}/riscv.l3
	rm -f ${HOLSRCDIR}/riscvLib.sig ${HOLSRCDIR}/riscvLib.sml ${HOLSRCDIR}/riscvScript.sml
