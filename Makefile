build:
	nix build
run:
	runhaskell -ilib examples/Main.hs
format:
	fourmolu -i $$(find . -name '*.hs')
