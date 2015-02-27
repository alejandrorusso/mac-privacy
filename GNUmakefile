
PKG = $(basename $(wildcard *.cabal))
HSCS := $(patsubst %.hsc,%.hs,$(shell find . -name '*.hsc' -print))
HSCCLEAN = $(patsubst %.hs,%_hsc.[ch],$(HSCS))

all: $(HSCS)

.PHONY: all clean build dist doc browse install hsc

GHC = ghc $(WALL)
WALL = -Wall -Werror
LIBS = -lz

Setup: Setup.hs
	$(GHC) --make Setup.hs

dist/setup-config: Setup $(PKG).cabal
	./Setup configure --user

build: dist/setup-config
	./Setup build

doc: dist/setup-config
	./Setup haddock --hyperlink-source

dist: dist/setup-config
	./Setup sdist

INDEXDOC = cd $(HOME)/.cabal/share/doc \
    && find . -name '*.haddock' -print \
	| sed -e 's/\.\/\(.*\)\/[^\/]*\.haddock/--read-interface=\1,&/' \
	| xargs -t haddock --gen-contents --gen-index --odir=.

install: build doc
	./Setup install
	$(INDEXDOC)

uninstall: dist/setup-config
	./Setup unregister --user
	rm -rf $(HOME)/.cabal/lib/$(PKG)-[0-9]*
	rm -rf $(HOME)/.cabal/share/doc/$(PKG)-[0-9]*
	$(INDEXDOC)

browse: doc
	xdg-open dist/doc/html/$(PKG)/index.html

clean:
	rm -rf dist
	rm -f Setup $(HSCS) $(HSCCLEAN)
	find . \( -name '*~' -o -name '*.hi' -o -name '*.o' \) -print0 \
		| xargs -0 rm -f --
