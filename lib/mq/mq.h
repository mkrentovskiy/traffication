#ifndef __MQ_H_
#define __MQ_H_

#include <stdint.h>
#include <pthread.h>

#include "config.h"

#define MQ_STATE_UND 0
#define MQ_STATE_INIT 1
#define MQ_STATE_WORK 2

typedef struct {
    uint16_t len; // only payload
    uint16_t type;
    uint32_t ts;
} msg_header_t;

typedef struct {
    msg_header_t h;
    uint8_t *p;
} msg_t;

typedef void (*mq_recv_callback_t)(msg_t *);

typedef struct {
    uint8_t state;
    char *dev;
    int fd;
    pthread_t w;
    mq_recv_callback_t cb;
} mq_context_t;

int mq_init(mq_context_t *);
int mq_send(mq_context_t *, msg_t *);

static inline int mq_send_rtc(mq_context_t *c, msg_t *m)
{
    int r, i = 0;

    do { r = mq_send(c, m); } while(r != 0 && i++ < SEND_COUNT_MAX);
    return r;
}

#endif