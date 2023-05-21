#ifndef LIBSERGE_IO_H_
#define LIBSERGE_IO_H_

// SysY runtime library.
// Reference: https://bit.ly/3tzTFks
// Modified by MaxXing.

#ifdef __cplusplus
extern "C" {
#endif

// Input & output functions
int getint(), getch(), getarray(int a[]);
void putint(int num), putch(int ch), putarray(int n, int a[]);

// Timing functions
void starttime();
void stoptime();

#ifdef __cplusplus
}
#endif
#endif  // LIBSERGE_IO_H_