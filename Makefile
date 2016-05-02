.PHONY: all doc test

all: doc test

doc:
	cabal haddock

test:
	cabal build
	./dist/build/test/test
