#pragma once


#define unlikely(x) __builtin_expect(!!(x), 0)
#define likely(x)   __builtin_expect(!!(x), 1)
// See https://blog.reverberate.org/2021/04/21/musttail-efficient-interpreters.html

#ifdef __clang__
#define tailcall    __atttribute__((musttail))
#endif