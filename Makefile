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
MLBFILE=l3riscv.mlb
SMLSRC=$(patsubst %, $(SMLSRCDIR)/%, $(SMLSRCBASE))

# make targets
#######################################

all: l3riscv

${SMLSRCDIR}/riscv.sig ${SMLSRCDIR}/riscv.sml: ${L3SRC}
	echo 'SMLExport.spec ("${L3SRC}", "${SMLSRCDIR}/riscv")' | l3

l3riscv: ${SMLLIB} ${SMLSRC} ${SMLSRCDIR}/riscv.sig ${SMLSRCDIR}/riscv.sml Makefile
	mlton -inline 1000 -default-type intinf -verbose 1 -output ./l3riscv ${SMLSRCDIR}/$(MLBFILE)

clean:
	rm -f l3riscv
	rm -f ${SMLSRCDIR}/riscv.sig ${SMLSRCDIR}/riscv.sml
