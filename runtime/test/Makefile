CURRENT_DIR := $(shell pwd)
INCLUDES := -I${CURRENT_DIR}/../std -I${CURRENT_DIR}/../wrapper
LIBS := $(CURRENT_DIR)/../../libsergeruntime_s.a
# currently runtime depends on libstdc++
CC := g++


all: Int

Int-test: Int.cpp
	$(CC) $^ -o $@ $(INCLUDES) $(LIBS)

Array-test: Array.cpp
	$(CC) $^ -o $@ $(INCLUDES) $(LIBS)

.PHONY: clean
clean:
	rm -rf *-test