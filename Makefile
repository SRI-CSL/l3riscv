########################################
# Makefile for the L3 RISCV simulator ##
########################################

L3SRCDIR=src/l3
L3SRCBASE+=riscv-print.spec
L3SRCBASE+=riscv.spec
L3SRC=$(patsubst %, $(L3SRCDIR)/%, $(L3SRCBASE))

# sml lib sources
#######################################
SMLSRCDIR=src/sml
SMLLIBDIR=src/sml/lib
SMLLIBSRC=Runtime.sig Runtime.sml\
          IntExtra.sig IntExtra.sml\
          Nat.sig Nat.sml\
          L3.sig L3.sml\
          Bitstring.sig Bitstring.sml\
          BitsN.sig BitsN.sml\
          FP64.sig FP64.sml\
          Ptree.sig Ptree.sml\
          MutableMap.sig MutableMap.sml
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

# Verifier
CISSR_BASE=$(HOME)/proj/Bluespec_RISCV
CISSR_INC_DIR=$(CISSR_BASE)
CISSR_LIB_DIR=$(CISSR_BASE)/build_libcissr
MLTON_LIB_OPTS=  -cc-opt "-DRV64 -I $(CISSR_BASE)"
MLTON_LIB_OPTS+= -link-opt "-L $(CISSR_LIB_DIR) -lcissr -Wl,-rpath,$(CISSR_LIB_DIR)"

# make targets
#######################################

all: l3riscv

${SMLSRCDIR}/riscv.sig ${SMLSRCDIR}/riscv.sml: ${L3SRC}
	echo 'SMLExport.spec ("${L3SRC}", "${SMLSRCDIR}/riscv")' | l3

l3riscv: ${SMLLIB} ${SMLSRC} Makefile
	mlton -inline 1000 -default-type intinf -verbose 1 \
              -default-ann 'allowFFI true' -export-header ${SMLSRCDIR}/riscv_oracle.h \
              $(MLTON_LIB_OPTS) \
              -output ./l3riscv ${SMLSRCDIR}/$(MLBFILE) ${SMLSRCDIR}/riscv_oracle.c

clean:
	rm -f l3riscv
	rm -f ${SMLSRCDIR}/riscv.sig ${SMLSRCDIR}/riscv.sml
