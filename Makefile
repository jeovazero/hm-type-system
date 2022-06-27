build:
	nix-build default.nix
run:
	runhaskell -ilib app/Main.hs
format:
	fourmolu -i $$(find . -name '*.hs')
