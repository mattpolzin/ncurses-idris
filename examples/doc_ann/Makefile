
IDRIS ?= "idris2"

INSTALL_DIR=./depends/ncurses-idris-0

CC_VERSION = $(shell $(CC) --version)

ifeq ($(findstring clang,$(CC_VERSION)),clang)
 DYLIB_WORKAROUND = cp "../../libncurses-idris" "${INSTALL_DIR}/lib/libncurses-idris.dylib"
else
 DYLIB_WORKAROUND = cp "../../libncurses-idris" "${INSTALL_DIR}/lib/libncurses-idirs.so"
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
	$(DYLIB_WORKAROUND)
	$(IDRIS) --build doc_ann.ipkg

.PHONY : clean

clean :
	rm -rf ./build
	rm -rf ./depends
