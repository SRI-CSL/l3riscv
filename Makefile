########################################
# Makefile for the L3 RISCV simulator ##
########################################

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
SMLSRCBASE+=riscv.sig
SMLSRCBASE+=riscv.sml
SMLSRCBASE+=run.sml
SMLSRCBASE+=l3riscv.mlb
SMLSRCBASE+=riscv_oracle.c
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
MLTON_OPTS     = -inline 1000 -default-type intinf -verbose 1
MLTON_OPTS    += -default-ann 'allowFFI true' -export-header ${SMLSRCDIR}/riscv_ffi.h
MLTON_LIB_OPTS = -mlb-path-var 'L3LIBDIR '$(L3LIBDIR)

# use Cissr as a verifier
#######################################
USE_CISSR ?= 0
# If set, point to the location of Bluespec_RISCV
CISSR_BASE=$(HOME)/proj/Bluespec_RISCV
# Set the directory containing libcissr
CISSR_LIB_DIR=$(CISSR_BASE)/build_libcissr

ifeq ($(USE_CISSR),1)
  MLTON_LIB_OPTS+= -cc-opt "-DUSE_CISSR -DRV64 -I $(CISSR_BASE)"
  MLTON_LIB_OPTS+= -link-opt "-L $(CISSR_LIB_DIR) -lcissr -Wl,-rpath,$(CISSR_LIB_DIR)"
endif

# make targets
#######################################

all: l3riscv libl3riscv.so ilspec holspec

${SMLSRCDIR}/riscv.sig ${SMLSRCDIR}/riscv.sml: ${L3SRC}
	echo 'SMLExport.spec ("${L3SRC}", "${SMLSRCDIR}/riscv")' | l3

l3riscv: ${SMLLIB} ${SMLSRC} Makefile
	mlton $(MLTON_OPTS) \
              $(MLTON_LIB_OPTS) \
              -output $@ ${SMLSRCDIR}/$(MLBFILE) ${SMLSRCDIR}/riscv_cissr.c $(L3LIBDIR)/sse_float.c $(L3LIBDIR)/mlton_sse_float.c

libl3riscv.so: ${SMLLIB} ${SMLSRC} Makefile
	mlton $(MLTON_OPTS) \
              $(MLTON_LIB_OPTS) \
              -format library \
              -output $@ ${SMLSRCDIR}/$(MLBFILE) ${SMLSRCDIR}/riscv_cissr.c ${SMLSRCDIR}/riscv_oracle.c $(L3LIBDIR)/sse_float.c $(L3LIBDIR)/mlton_sse_float.c

ilspec: ${L3SRC}
	mkdir -p $(ILSRCDIR)
	echo 'ILExport.spec ("${L3SRC}", "${ILSRCDIR}/riscv")' | l3

holspec: ${L3SRC}
	mkdir -p $(HOLSRCDIR)
	echo 'HolExport.spec ("${L3SRC}", "${HOLSRCDIR}/riscv")' | l3

clean:
	rm -f l3riscv libl3riscv.so
	rm -f ${SMLSRCDIR}/riscv.sig ${SMLSRCDIR}/riscv.sml
	rm -f ${ILSRCDIR}/riscv.l3
	rm -f ${HOLSRCDIR}/riscvLib.sig ${HOLSRCDIR}/riscvLib.sml ${HOLSRCDIR}/riscvScript.sml
