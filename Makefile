NAME=syfco
MAIN=src/Main.hs
BLDDIR=bld

default: ${MAIN.hs}
	@mkdir -p bld
	@ghc -idir1:src -O3 -global-package-db ${MAIN} -odir ${BLDDIR} -hidir ${BLDDIR} -o ${NAME}
	@strip ${NAME}

run: default
	@./${NAME}

haddock:
	@cabal --executable --hyperlink-source haddock 

clean:
	@rm -fR bld
	@rm -fR ${NAME}

.PHONY: clean
.SILENT:
