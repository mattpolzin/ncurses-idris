IDRIS := idris2

NCURSES_VERSION ?=      # system libncurses version
INDEXED_VERSION = 0.0.7 # indexed Idris package

LD_OVERRIDE ?= 

TARGET = libncurses-idris
TARGET_VERSION ?= 0.0.3

PACKAGE_INSTALLDIR = `${IDRIS} --libdir`
SHAREDLIB_INSTALLDIR = ${PACKAGE_INSTALLDIR}/ncurses-idris-${TARGET_VERSION}/lib

LDFLAGS = $(LD_OVERRIDE) -lncurses

CC_VERSION = $(shell $(CC) --version)

ifeq ($(findstring clang,$(CC_VERSION)),clang)
 DYLIB_WORKAROUND = cp "${SHAREDLIB_INSTALLDIR}/${TARGET}" "${SHAREDLIB_INSTALLDIR}/${TARGET}.dylib"
else
 DYLIB_WORKAROUND = cp "${SHAREDLIB_INSTALLDIR}/${TARGET}" "${SHAREDLIB_INSTALLDIR}/${TARGET}.so"
 CFLAGS += -fPIC
 LDFLAGS += -fuse-ld=gold
endif

ifeq (${NCURSES_VERSION},)
	NCURSES_WORKAROUND = echo 'Using versionless NCurses lib.'
else
	NCURSES_WORKAROUND = cat Core.idr | sed 's/libncurses"/libncurses $(NCURSES_VERSION)"/' > tmp.idr && \
	                     rm Core.idr && mv tmp.idr Core.idr
endif

SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)
DEPS = $(OBJS:.o=.d)

all: package $(TARGET)$(SHLIB_SUFFIX)

$(TARGET)$(SHLIB_SUFFIX): $(OBJS)
	$(CC) -shared $(LDFLAGS) -o $@ $^

-include $(DEPS)

%.d: %.c
	@$(CPP) $(CFLAGS) $< -MM -MT $(@:.d=.o) >$@

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
	idris2 --build ncurses-idris.ipkg

.PHONY: clean

clean:
	rm -f $(OBJS) $(TARGET)$(SHLIB_SUFFIX)
	rm -rf ./build
	rm -rf ./depends

cleandep: clean
	rm -f $(DEPS)

.PHONY: install

install:
	idris2 --install ncurses-idris.ipkg
	@if ! [ -d $(SHAREDLIB_INSTALLDIR) ]; then mkdir -p $(SHAREDLIB_INSTALLDIR); fi
	install $(TARGET)$(SHLIB_SUFFIX) $(wildcard *.h) $(SHAREDLIB_INSTALLDIR)
	$(DYLIB_WORKAROUND)
	cp -R ./depends/* ${PACKAGE_INSTALLDIR}/

install-with-src:
	idris2 --install-with-src ncurses-idris.ipkg
	@if ! [ -d $(SHAREDLIB_INSTALLDIR) ]; then mkdir -p $(SHAREDLIB_INSTALLDIR); fi
	install $(TARGET)$(SHLIB_SUFFIX) $(wildcard *.h) $(SHAREDLIB_INSTALLDIR)
	$(DYLIB_WORKAROUND)
	cp -R ./depends/* ${PACKAGE_INSTALLDIR}/

