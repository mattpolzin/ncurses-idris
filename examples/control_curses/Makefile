
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

.PHONY : all

all :
	mkdir -p depends/ncurses-idris-0/lib
	cd ../.. && \
	make && \
	cd - && \
	cp -R ../../depends/* ./depends/ && \
	cp -R ../../build/ttc/* ./depends/ncurses-idris-0 && \
	(cp ../../support/*.dylib ./depends/ncurses-idris-0/lib/ || \
	cp ../../support/*.so ./depends/ncurses-idris-0/lib/) && \
	$(IDRIS) --build control_curses.ipkg

clean :
	rm -rf ./build
	rm -rf ./depends
