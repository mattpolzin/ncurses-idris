
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
	cp -R ../../build/ttc/* ./depends/ncurses-idris-0 && \
	$(IDRIS) --build key_logger.ipkg

.PHONY : clean

clean :
	rm -rf ./build
	rm -rf ./depends
