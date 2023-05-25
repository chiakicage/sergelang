#include "GCObject.h"
#include "Utility.h"

extern "C"
void __serge_gc_init(void);

extern "C"
SergeUnit *__serge_user_main(void);

extern "C"
int main(int argc, char **argv) {
    __serge_user_main();
    return 0;
}
