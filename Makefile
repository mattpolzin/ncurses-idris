IDRIS := idris2

NCURSES_VERSION ?=      # system libncurses version
INDEXED_VERSION = 0.0.8 # indexed Idris package

PACKAGE_INSTALLDIR = `${IDRIS} --libdir`

ifeq (${NCURSES_VERSION},)
	NCURSES_WORKAROUND = echo 'Using versionless NCurses lib.'
else
	NCURSES_WORKAROUND = cat Core.idr | sed 's/libncurses"/libncurses $(NCURSES_VERSION)"/' > tmp.idr && \
	                     rm Core.idr && mv tmp.idr Core.idr
endif

all: package

./depends/indexed-${INDEXED_VERSION}: 
	mkdir -p ./depends/indexed-${INDEXED_VERSION}
	mkdir -p ./build/deps
	cd ./build/deps && \
		git clone https://github.com/mattpolzin/idris-indexed.git && \
		cd idris-indexed && \
		git checkout ${INDEXED_VERSION} && \
		make && \
		cp -R ./build/ttc/* ../../../depends/indexed-${INDEXED_VERSION}

.PHONY: package

package: ./depends/indexed-${INDEXED_VERSION}
	cd src/NCurses && \
		$(NCURSES_WORKAROUND) && \
		cd -
	$(IDRIS) --build ncurses-idris.ipkg

.PHONY: clean

clean:
	$(IDRIS) --clean ncurses-idris.ipkg
	rm -rf ./build
	rm -rf ./depends

.PHONY: install

install:
	$(IDRIS) --install ncurses-idris.ipkg
	cp -R ./depends/* ${PACKAGE_INSTALLDIR}/

install-with-src:
	$(IDRIS) --install-with-src ncurses-idris.ipkg
	cp -R ./depends/* ${PACKAGE_INSTALLDIR}/

