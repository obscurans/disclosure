.PHONY: test

test:
	cabal build
	./dist/build/test/test
