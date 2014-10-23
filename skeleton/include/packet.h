#ifndef __PACKET_H
#define __PACKET_H

#include <stdint.h>
#include <stdbool.h>

#include <sys/time.h>

/*
    level 2
*/

typedef union {
    uint8_t o[6];
    struct {
        uint32_t group:1,
                 local:1,
                 ieee_reg:22,
                 iface:24;
    } _PACK f;
} mac_t;

typedef uint16_t vlan_id_t;

typedef struct {
    mac_t src_mac;
    mac_t dst_mac;
    vlan_id_t vlan_id;
} _PACK l2_t;

/*
    level 3-4
*/

typedef union {
    uint32_t a;
    uint8_t o[4];
} ipv4_t;

typedef uint8_t ipv4mask_t;
typedef uint8_t proto_t;
typedef uint16_t port_t;

typedef struct {
    ipv4_t ip;
    ipv4mask_t mask;
    uint8_t m;
} _PACK ipv4net_t;

typedef struct {
    proto_t proto;
    ipv4_t src;
    port_t src_p;
    ipv4_t dst;
    port_t dst_p;
} _PACK l3_ipv4_t;

/*
    packet
*/

typedef uint16_t packet_len_t;

typedef struct _packet packet_t;

typedef void (*packet_done_t)(packet_t *);

struct _packet {
    uint8_t *header;
    uint8_t *l3_header;
    uint8_t *l4_header;
    uint8_t *payload;
    
    packet_len_t len;

    l2_t l2;
    l3_ipv4_t l3;

    packet_done_t done;
};

/*
    statistics
*/

typedef uint64_t counter_t;

typedef struct {
    counter_t in_pkt;
    counter_t in_bytes;
    counter_t out_pkt;
    counter_t out_bytes;  
    counter_t active_flows;
    counter_t total_flows;
} flowstat_t;

/*
    packet's operations
*/

static inline void packet_construct(packet_t *p, uint8_t *data, uint16_t len) {
    p->len = len;
    p->done = NULL;

    if(len > 0) {
        p->header = data;
        p->payload = p->header;
    } else {
        p->header = NULL;
        p->payload = NULL;
    }
}

static inline void default_packet_done(packet_t *p) {
    if(p->len > 0 && p->header) free(p->header);
    p->len = 0;
    p->header = NULL;
    p->payload = NULL;
}

static inline void packet_init(packet_t *p, uint16_t len) {
    packet_construct(p, malloc(len), len);
    p->done = default_packet_done;
}

static inline bool packet_valid(packet_t *p) {
    return p->len && (p->payload >= p->header) && (p->payload <= p->header + p->len);
}

#endif