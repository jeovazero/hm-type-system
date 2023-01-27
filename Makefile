build:
	nix-build default.nix
run:
	runhaskell -ilib examples/Main.hs
format:
	fourmolu -i $$(find . -name '*.hs')
