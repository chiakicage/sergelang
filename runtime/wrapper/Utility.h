#pragma once


#define unlikely(x) __builtin_expect(!!(x), 0)
#define likely(x)   __builtin_expect(!!(x), 1)
// See https://blog.reverberate.org/2021/04/21/musttail-efficient-interpreters.html

#ifdef __clang__
#define tailcall    __attribute__((musttail))
#endif

#define alias(name) __attribute__((alias(name)))

void __serge_panic(const char *msg);
