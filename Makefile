NAME=syfco
MAIN=src/Main.hs
BLDDIR=bld
BLDSTATIC=bldstatic

default: ${MAIN.hs}
	@mkdir -p ${BLDDIR}
	@ghc -O3 -idir1:src -global-package-db ${MAIN} -odir ${BLDDIR} -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}

run: default
	@./${NAME}

haddock:
	@cabal --executable --hyperlink-source haddock

profile: ${MAIN.hs}
	@mkdir -p ${BLDDIR}
	@ghc -O3 -prof -fprof-auto -idir1:src -global-package-db ${MAIN} -odir ${BLDDIR} -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}
	@echo -e '#!/bin/bash\n\n./syfco +RTS -p -RTS $${*}' > ${NAME}_profile
	@chmod +x ${NAME}_profile

static:
	@mkdir -p ${BLDSTATIC}
	@ghc -static -O3 -optc-static -optl-static -optl-pthread -idir1:src -global-package-db ${MAIN} -odir ${BLDSTATIC} -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}	

clean:
	@rm -fR ${BLDDIR}
	@rm -fR ${BLDSTATIC}	
	@rm -fR ${NAME}
	@rm -fR ${NAME}_profile
	@rm -fR dist

.PHONY: clean
.SILENT:
