export CC := clang
export CXX := clang++

ARCH ?= riscv64
DBG ?= 0
include target.$(ARCH)

INCLUDES := -I $(shell pwd)/std -I $(shell pwd)/wrapper
ifeq ($(DBG), 0)
	OPT := -O2
	DEFS := -DNDEBUG
else 
	OPT := -O0 -g
	DEFS := 
endif
LINKER := -fuse-ld=lld
OPTIONS := -ffunction-sections -fdata-sections -fPIC -gdwarf-4 -fno-omit-frame-pointer -Wall -Wextra -fno-rtti -fno-exceptions
AR := ar

export CXXFLAGS := $(TARGET) $(INCLUDES) $(OPT) $(DEFS) $(LINER) $(OPTIONS)

.PHONY: clean
all:
	$(MAKE) -C wrapper all
	$(MAKE) -C std all
	$(AR) rcs libserge_runtime.a wrapper/*.o std/*.o 

clean:
	$(MAKE) -C wrapper clean
	$(MAKE) -C std clean
	$(shell rm -rf *.a 2> /dev/null)

