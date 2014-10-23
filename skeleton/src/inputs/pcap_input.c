#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "misc.h"
#include "packet.h"
#include "input.h"

#include "dbg.h"
#include "config.h"


void pcap_input_init(input_t *input) 
{
    input->state = malloc(sizeof(pcap_input_state_t));
    pcap_input_state_t *s = (pcap_input_state_t *)input->state;
    packet_t p;
    
    s->f = fopen(PCAP_FILENAME, "rb");
    OP(!s->f, "Open PCAP file");
    OP(fread(&s->h, 1, sizeof(pcap_file_header_t), s->f) < sizeof(pcap_file_header_t), "Read header from PCAP file");

    while(input->next(input, &p));
}

bool pcap_input_next(input_t *input, packet_t *p) 
{
    pcap_input_state_t *s = (pcap_input_state_t *) input->state;
    bool r = false;
    pcap_packet_header_t ph;

    if(!input->state || !s->f) return r;    
    if(fread(&ph, 1, sizeof(pcap_packet_header_t), s->f) == sizeof(pcap_packet_header_t)) {
        log_info("%d %d", ph.incl_len, ph.orig_len);
        
        packet_init(p, ph.orig_len);
        if(fread((void *) p->header, 1, ph.orig_len, s->f) == ph.orig_len) {
            if(input->process) {
                preprocess(p, true);
                input->process(input, p);
                r = true;
            }
        } 
        if(p->done) p->done(p);
    } 
    return r;
} 

void pcap_input_done(input_t *input) 
{
    pcap_input_state_t *s = (pcap_input_state_t * )input->state;
    
    if(s->f) fclose(s->f);
    free(input->state);
}


input_t pcap_input = {
    .state = NULL
    ,.init = pcap_input_init
    ,.done = pcap_input_done
    ,.next = pcap_input_next
    ,.process = NULL
};