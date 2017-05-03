.PHONY: all lint run install configure test

all: install configure test lint run

install:
	cabal install --only-dependencies

configure:
	cabal configure

lint:
	.cabal-sandbox/bin/hlint .

run:
	cabal run

test:
	cabal exec -- runhaskell -isrc -itest test/Spec.hs
