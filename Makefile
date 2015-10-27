NAME=syfco
MAIN=Main.hs
BLDDIR=bld

default: ${MAIN}
	@mkdir -p bld
	@ghc -global-package-db ${MAIN} -odir ${BLDDIR} -hidir ${BLDDIR} -o ${NAME}

run: default
	@./${NAME}

clean:
	@rm -fR bld
	@rm -fR ${NAME}

.PHONY: clean
.SILENT:
