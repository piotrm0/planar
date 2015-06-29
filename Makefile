#DEBUG := --ghc-options="-ddump-splices"

build:
	cabal build -j8 -v0 $(DEBUG)

run:
	make build
	./dist/build/planar/planar