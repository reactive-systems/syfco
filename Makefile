NAME=syfco
MAIN=src/Main.hs
BLDDIR=bld

default: ${MAIN.hs}
	@mkdir -p bld
	@ghc -O3 -idir1:src -global-package-db ${MAIN} -odir ${BLDDIR} -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}

run: default
	@./${NAME}

haddock:
	@cabal --executable --hyperlink-source haddock

profile: ${MAIN.hs}
	@mkdir -p bld
	@ghc -O3 -prof -fprof-auto -idir1:src -global-package-db ${MAIN} -odir ${BLDDIR} -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}
	@echo -e '#!/bin/bash\n\n./syfco +RTS -p -RTS $${*}' > ${NAME}_profile
	@chmod +x ${NAME}_profile

static:
	@mkdir -p bldstatic
	@ghc -static -O3 -optc-static -optl-static -optl-pthread -idir1:src -global-package-db ${MAIN} -odir bldstatic -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}	

clean:
	@rm -fR bld
	@rm -fR ${NAME}
	@rm -fR ${NAME}_profile
	@rm -fR ${NAME}.prof

.PHONY: clean
.SILENT:
