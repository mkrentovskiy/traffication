#ifndef __MISC_H
#define __MISC_H

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <malloc.h>

#define _PACK __attribute__((__packed__))

#define OP(OP, NAME) \
    do { \
        if (OP) { \
            fprintf(stderr, "%s failed: %s.\n", NAME, strerror(errno)); \
            exit(1); \
        } \
    } while (0)

#ifdef __GNUC__
    #define likely(x)       __builtin_expect(!!(x), 1)
    #define unlikely(x)     __builtin_expect(!!(x), 0)
#else
    #define likely(x)       (x)
    #define unlikely(x)     (x)
#endif

#endif
