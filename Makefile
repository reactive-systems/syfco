NAME=syfco

default:
	@cabal update
	@cabal configure
	@cabal build exe:${NAME}
	@cp ./dist/build/${NAME}/${NAME} ${NAME}
	@strip ${NAME}

ghci:
	@cabal update
	@cabal configure
	@cabal build
	@cabal repl

install:
	@cabal update
	@cabal configure
	@cabal build exe:${NAME}
	@cabal install


haddock:
	@cabal --executable --hyperlink-source haddock

clean:
	@rm -fR ${NAME}
	@rm -fR dist

.PHONY: clean
.SILENT:
