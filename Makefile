IDRIS := idris2

NCURSES_VERSION ?=

LD_OVERRIDE ?= 

TARGET = libncurses-idris
TARGET_VERSION ?= 0

SHAREDLIB_INSTALLDIR = `${IDRIS} --libdir`/ncurses-idris-${TARGET_VERSION}/lib

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

.PHONY: package

package:
	cd src/NCurses && \
		$(NCURSES_WORKAROUND) && \
		cd -
	idris2 --build ncurses-idris.ipkg

.PHONY: clean

clean:
	rm -f $(OBJS) $(TARGET)$(SHLIB_SUFFIX)
	rm -rf ./build

cleandep: clean
	rm -f $(DEPS)

.PHONY: install

install:
	idris2 --install ncurses-idris.ipkg
	@if ! [ -d $(SHAREDLIB_INSTALLDIR) ]; then mkdir -p $(SHAREDLIB_INSTALLDIR); fi
	install $(TARGET)$(SHLIB_SUFFIX) $(wildcard *.h) $(SHAREDLIB_INSTALLDIR)
	$(DYLIB_WORKAROUND)

install-with-src:
	idris2 --install-with-src ncurses-idris.ipkg
	@if ! [ -d $(SHAREDLIB_INSTALLDIR) ]; then mkdir -p $(SHAREDLIB_INSTALLDIR); fi
	install $(TARGET)$(SHLIB_SUFFIX) $(wildcard *.h) $(SHAREDLIB_INSTALLDIR)
	$(DYLIB_WORKAROUND)

