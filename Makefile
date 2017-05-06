.PHONY: all lint install configure test triangle cube

triangle: install configure test lint
	cabal run -- triangle

triangle_camera: install configure test lint
	cabal run -- triangle_camera

cube: install configure test lint
	cabal run -- cube

install:
	cabal install --only-dependencies

configure:
	cabal configure

lint:
	.cabal-sandbox/bin/hlint .

test:
	cabal exec -- runhaskell -isrc -itest test/Spec.hs
