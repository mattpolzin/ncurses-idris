IDRIS2 ?= idris2

# system libncurses version:
NCURSES_VERSION ?=
# indexed Idris package:
INDEXED_VERSION = 0.0.9
# install in the local depends directory by default:
INDEXED_INSTALL_LOCATION ?= ./depends

PACKAGE_INSTALLDIR = `${IDRIS2} --libdir`

ifeq (${NCURSES_VERSION},)
	NCURSES_WORKAROUND = echo 'Using versionless NCurses lib.'
else
	NCURSES_WORKAROUND = echo "Using NCurses lib version ${NCURSES_VERSION}." && \
			     cat Core.idr | sed 's/libncurses,ncurses.h"/libncurses $(NCURSES_VERSION),ncurses.h"/' > tmp.idr && \
	                     rm Core.idr && mv tmp.idr Core.idr
endif

all: package

${INDEXED_INSTALL_LOCATION}/indexed-${INDEXED_VERSION}: 
	mkdir -p "${INDEXED_INSTALL_LOCATION}/indexed-${INDEXED_VERSION}"
	mkdir -p ./build/deps
	cd ./build/deps && \
		git clone https://github.com/mattpolzin/idris-indexed.git && \
		cd idris-indexed && \
		git checkout ${INDEXED_VERSION} && \
		make && \
		cp -R ./build/ttc/* ../../../${INDEXED_INSTALL_LOCATION}/indexed-${INDEXED_VERSION}

.PHONY: package

package: ${INDEXED_INSTALL_LOCATION}/indexed-${INDEXED_VERSION}
	cd src/NCurses && \
		$(NCURSES_WORKAROUND) && \
		cd -
	$(IDRIS2) --build ncurses-idris.ipkg

.PHONY: clean

clean:
	$(IDRIS2) --clean ncurses-idris.ipkg
	rm -rf ./build
	rm -rf ./depends

.PHONY: install

install:
	$(IDRIS2) --install ncurses-idris.ipkg
	if [ -d ./depends ]; then \
	  cp -R ./depends/* ${PACKAGE_INSTALLDIR}/; \
	fi

install-with-src:
	$(IDRIS2) --install-with-src ncurses-idris.ipkg
	if [ -d ./depends ]; then \
	  cp -R ./depends/* ${PACKAGE_INSTALLDIR}/; \
	fi

