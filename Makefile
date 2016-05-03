.PHONY: all init doc test

all: doc test

init:
	#cabal sandbox init
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

doc:
	cabal haddock

test:
	cabal build
	./dist/build/test/test
