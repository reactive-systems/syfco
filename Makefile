NAME=syfco
MAIN=src/Main.hs
BLDDIR=bld
LDSTATIC=bldstatic
GHCFLAGS= -O3

GHCV1=$(shell ghc -V | sed 's/.*version \(.*\)\..*\..*/\1/')
GHCV2=$(shell ghc -V | sed 's/.*version .*\.\(.*\)\..*/\1/')
GHCV3=$(shell ghc -V | sed 's/.*version .*\..*\.\(.*\)/\1/')

V_GT_7_6_1=$(shell [ $(GHCV1) -gt 7 -o \( $(GHCV1) -eq 7 -a \( $(GHCV2) -gt 6 -o \( $(GHCV2) -eq 6 -a $(GHCV3) -ge 1 \) \) \) ] && echo true)

ifeq ($(V_GT_7_6_1),true)
GHCFLAGS += -global-package-db
endif


default: ${MAIN.hs}
	@mkdir -p ${BLDDIR}
	@ghc -idir1:src ${GHCFLAGS} ${MAIN} -odir ${BLDDIR} -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}

debug: ${MAIN.hs}
	@mkdir -p ${BLDDIR}
	@ghc -idir1:src ${GHCFLAGS} -O0 ${MAIN} -odir ${BLDDIR} -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}

run: default
	@./${NAME}

haddock:
	@cabal --executable --hyperlink-source haddock

profile: ${MAIN.hs}
	@mkdir -p ${BLDDIR}
	@ghc -prof -fprof-auto -idir1:src ${GHCFLAGS} ${MAIN} -odir ${BLDDIR} -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}
	@echo -e '#!/bin/bash\n\n./${NAME} +RTS -p -RTS $${*}' > ${NAME}_profile
	@chmod +x ${NAME}_profile

static:
	@mkdir -p ${BLDSTATIC}
	@ghc -static -optc-static -optl-static -optl-pthread -idir1:src ${GHCFLAGS} ${MAIN} -odir ${BLDSTATIC} -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}	

clean:
	@rm -fR ${BLDDIR}
	@rm -fR ${BLDSTATIC}	
	@rm -fR ${NAME}
	@rm -fR ${NAME}_profile
	@rm -fR dist

.PHONY: clean
.SILENT:
