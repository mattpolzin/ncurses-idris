IDRIS := idris2

TARGET = libncurses-idris
TARGET_VERSION ?= 0.2.0

LD_OVERRIDE ?= 
LDFLAGS = $(LD_OVERRIDE) -lncurses

PACKAGE_INSTALLDIR = `${IDRIS} --libdir`
INSTALLDIR = ${PACKAGE_INSTALLDIR}/ncurses-idris-${TARGET_VERSION}/lib

CC_VERSION = $(shell $(CC) --version)
MACHINE := $(shell $(CC) -dumpmachine)
 ifneq (,$(findstring cygwin, $(MACHINE)))
 	OS := windows
 	SHLIB_SUFFIX := .dll
 else ifneq (,$(findstring mingw, $(MACHINE)))
 	OS := windows
 	SHLIB_SUFFIX := .dll
 else ifneq (,$(findstring windows, $(MACHINE)))
 	OS := windows
 	SHLIB_SUFFIX := .dll
 else ifneq (,$(findstring darwin, $(MACHINE)))
 	OS := darwin
 	SHLIB_SUFFIX := .dylib
 else ifneq (, $(findstring bsd, $(MACHINE)))
 	OS := bsd
 	SHLIB_SUFFIX := .so
 else
 	OS := linux
 	SHLIB_SUFFIX := .so
 endif

ifeq ($(findstring clang,$(CC_VERSION)),clang)
 CFLAGS += ''
 LDFLAGS += ''
else
 CFLAGS += -fPIC
 LDFLAGS += -fuse-ld=gold
endif

SRCS = $(wildcard *.c)
OBJS = $(SRCS:.c=.o)
DEPS = $(OBJS:.o=.d)

all: $(TARGET)$(SHLIB_SUFFIX)

$(TARGET)$(SHLIB_SUFFIX): $(OBJS)
	$(CC) -shared $(LDFLAGS) -o $@ $^

-include $(DEPS)

%.d: %.c
	@$(CPP) $(CFLAGS) $< -MM -MT $(@:.d=.o) >$@

.PHONY: clean

clean:
	rm -f $(OBJS) $(TARGET)$(SHLIB_SUFFIX)

.PHONY: cleandep

cleandep: clean
	rm -f $(DEPS)

.PHONY: install

install:
	@if ! [ -d $(INSTALLDIR) ]; then mkdir -p $(INSTALLDIR); fi
	install $(TARGET)$(SHLIB_SUFFIX) $(wildcard *.h) $(INSTALLDIR)

