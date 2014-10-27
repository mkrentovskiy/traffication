#ifndef __BASE_H_
#define __BASE_H_

#include <stdint.h>

#define likely(cond) __builtin_expect((cond), 1)
#define unlikely(cond) __builtin_expect((cond), 0)

#define ALIGN(p, align) do { (p) += -(long)(p) & ((align) - 1); } while (0)
#define VERIFY(VAL, WHAT) \
    do { \
        long long __val = (VAL); \
        if (__val < 0) tmc_task_die("Failure in '%s': %lld: %s.", (WHAT), __val, gxio_strerror(__val)); \
    } while (0)

#define _PACK __attribute__((__packed__))

typedef uint64_t counter_t;
typedef uint8_t byte_t;

typedef struct {
    counter_t len;
    byte_t *p;
} bs_t;

#endif