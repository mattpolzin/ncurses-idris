
IDRIS ?= "idris2"

INSTALL_DIR=./depends/ncurses-idris-0

CC_VERSION = $(shell $(CC) --version)

ifeq ($(findstring clang,$(CC_VERSION)),clang)
 CFLAGS += ''
 LDFLAGS += ''
else
 CFLAGS += -fPIC
 LDFLAGS += -fuse-ld=gold
endif

.PHONY : deps app all

all : deps app

deps:
	mkdir -p depends/ncurses-idris-0/lib
	cd ../.. && \
	make && \
	cd - && \
	cp -R ../../depends/* ./depends/ && \
	cp -R ../../build/ttc/* ./depends/ncurses-idris-0 && \
	cp ../../support/libncurses* ./depends/ncurses-idris-0/lib/

./build/exe/control_curses_ticker:
	$(IDRIS) --build control_curses_ticker.ipkg

app: ./build/exe/control_curses_ticker

clean :
	rm -rf ./build
	rm -rf ./depends
