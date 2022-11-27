NAME=syfco
BLDTOOL=`if [ -d "dist" ]; then echo "cabal"; else echo "stack"; fi`

default:
	${BLDTOOL} build
	@if [ -d "dist" ]; then cp ./dist/build/${NAME}/${NAME} ${NAME}; else cp `stack path | grep local-install-root | sed 's/local-install-root: //'`/bin/${NAME} ${NAME}; fi

ghci:
	${BLDTOOL} repl

install:
	${BLDTOOL} install

haddock:
	${BLDTOOL} haddock

clean:
	${BLDTOOL} clean
	@rm -fR ${NAME}

testfin:
	@if [ "$(./syfco test/amba_finite.tlsf --format ltlxba-fin)" = "$(cat test/amba_finite.res)" ]; then echo "Finite semantics work as expected"; else echo "Something went wrong"; fi

.PHONY: clean
.SILENT:
