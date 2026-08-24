DOTSLASH := dotslash
FOURMOLU := tools/fourmolu
HASKELL_SOURCE_DIRS := src test examples

.PHONY: tools format format-check build test repl check

tools:
	@command -v $(DOTSLASH) >/dev/null || { \
		echo "DotSlash is required: https://dotslash-cli.com/docs/installation/"; \
		exit 1; \
	}
	$(DOTSLASH) $(FOURMOLU) --version

format: tools
	$(DOTSLASH) $(FOURMOLU) --mode inplace $(HASKELL_SOURCE_DIRS)

format-check: tools
	$(DOTSLASH) $(FOURMOLU) --mode check $(HASKELL_SOURCE_DIRS)

build:
	cabal build all

test:
	cabal test all

repl:
	cabal repl

check: format-check build test
