#ifndef __INPUT_H
#define __INPUT_H

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#include <pcap.h>

typedef struct _input input_t;

typedef void (*input_init_t)(input_t *);
typedef bool (*input_process_t)(input_t *, packet_t *);

struct _input {
    void *state;

    input_init_t init;
    input_init_t done;

    input_process_t next;
    input_process_t process;
};

/*
    PCAP file context
*/

typedef struct {
    uint32_t magic_number;
    uint16_t version_major;
    uint16_t version_minor;
    int32_t this_zone;
    uint32_t sig_figs;
    uint32_t snap_len;
    uint32_t network;
} pcap_file_header_t;

typedef struct {
    uint32_t ts_sec;
    uint32_t ts_usec;
    uint32_t incl_len;
    uint32_t orig_len;
} pcap_packet_header_t;

typedef struct {
    FILE *f;
    pcap_file_header_t h;
} pcap_input_state_t;

extern input_t pcap_input;

/*
    libpcap context
*/

typedef struct {
    pcap_t *handle;
} libpcap_input_state_t;

extern input_t libpcap_input;


/*
    Generic ops
*/

void preprocess(packet_t *, bool);     

#endif