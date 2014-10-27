#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>
#include <fcntl.h>

#include <sys/time.h>

#include "base.h"
#include "config_mq.h"
#include "mq.h"
#include "dbg.h"


void * mq_recv_worker(void * arg)
{
    mq_context_t *c = (mq_context_t *) arg;
    size_t r, p;
    msg_t m = { .p = malloc(MAX_MSG_SIZE) };

    while(1) {
        /* open device */
        log_info("Try to open MQ device.");
        c->fd = open(c->dev, O_RDWR);
        if(c->fd < 0) {
            log_err("Can't open MQ device %d, waiting.", c->fd);
            c->state = MQ_STATE_UND;
            sleep(READ_TIMEOUT);
            continue;
        } else {
            log_info("Start quering");
            c->state = MQ_STATE_WORK;
            
            while(1) {
                if(likely(c->state == MQ_STATE_WORK)) {
                    m.h.len = 0;
                    p = 0;

                    r = read(c->fd, &m.h, sizeof(msg_header_t));
                    if(likely(r == sizeof(msg_header_t))) {
                        log_info("Got message (type = %d, len = %d).", m.h.type, m.h.len);
                        do {
                            r = read(c->fd, m.p + p, m.h.len - p);
                            if(likely(r >= 0)) p += r;
                            else if(errno != EINTR) break;
                        } while (p < m.h.len);   

                        if(c->cb) {
                            log_info("Apply callback to message.");
                            c->cb(&m);
                        }
                    } else break;
                } else {
                    sleep(READ_TIMEOUT);
                }
            } 
        }

    }
    if(c->fd) close(c->fd);
    free(m.p);

    return (void *)NULL;
}


int mq_init(mq_context_t *c)
{
    int r;

    if(c->state != MQ_STATE_UND) {
        log_warn("MQ already init or working.");
        return -EBUSY;
    } 

    c->state = MQ_STATE_INIT;
    log_info("Create message thread.");
    r = pthread_create(&c->w, NULL, mq_recv_worker, (void*) c);
    if(r != 0) {
        log_err("Can't create reading thread - %d.", r);
        c->state = MQ_STATE_UND;
        return -r;
    }

    return 0;
}

int mq_send(mq_context_t *c, msg_t *m)
{
    size_t r, p = 0;
    struct timeval now;

    /* check context and reset it if undefined */
    if(unlikely(m->h.len <= 0)) return 0;
    if(unlikely(c->state != MQ_STATE_WORK)) {
        if(likely(c->state == MQ_STATE_UND)) {
            log_warn("Undefined state, reinit.");
            int ir = mq_init(c);
            
            if(ir < 0) return ir; 
        } else {
            log_err("Can't send message - MQ's state are incorrect.");
            return -ENOENT;
        }
    }

    /* setup correct timestamp */
    gettimeofday(&now, NULL);
    m->h.ts = (uint32_t) now.tv_sec;

    log_info("Try to send message(type = %d, len = %d).", m->h.type, m->h.len);
    
    /* write header */
    r = write(c->fd, &m->h, sizeof(msg_header_t));
    if(unlikely(r != sizeof(msg_header_t))) {
        log_err("Can't send message - send only %d bytes of header.", r);
        close(c->fd);
        c->state = MQ_STATE_UND;
        return -EIO;
    }

    /* send payload; */
    do {
        r = write(c->fd, m->p + p, m->h.len - p);
        if(likely(r >= 0)) p += r;
        else if(errno != EINTR) {
            log_err("Can't send message - %d.", errno);
            close(c->fd);
            c->state = MQ_STATE_UND;
            return -errno;
        }
    } while(p < m->h.len); 

    return 0;
}
