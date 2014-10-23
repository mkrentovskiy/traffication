#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "misc.h"
#include "packet.h"
#include "input.h"

#include "dbg.h"
#include "config.h"

void got_packet(u_char *args, const struct pcap_pkthdr *header, const u_char *packet)
{
    packet_t p;

    packet_construct(&p, (uint8_t *) packet, header->len);
    libpcap_input.next(&libpcap_input, &p);
    if(p.done) p.done(&p);
}

void libpcap_input_init(input_t *input) 
{
    char errbuf[PCAP_ERRBUF_SIZE];

    input->state = malloc(sizeof(libpcap_input_state_t));
    libpcap_input_state_t *s = (libpcap_input_state_t *)input->state;

    s->handle = pcap_open_live(LIBPCAP_DEV, LIBPCAP_BUFSIZE, LIBPCAP_PROMISC, LIBPCAP_TOMS, errbuf);
    if(s->handle == NULL) {
        log_err("Couldn't open device %s: %s", LIBPCAP_DEV, errbuf);
        return;
    } else {
        pcap_loop(s->handle, 0, got_packet, NULL);
    }
}

void libpcap_input_done(input_t *input) 
{
    libpcap_input_state_t *s = (libpcap_input_state_t *)input->state;

    pcap_close(s->handle);
    free(s);
}

bool libpcap_input_next(input_t *input, packet_t *p) 
{
    if(input->process) {
        preprocess(p, true);
        return input->process(input, p); 
    } 

    return false;
}


input_t libpcap_input = {
    .state = NULL
    ,.init = libpcap_input_init
    ,.done = libpcap_input_done
    ,.next = libpcap_input_next
    ,.process = NULL
};
