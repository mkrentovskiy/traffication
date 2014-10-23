#include <stdio.h>
#include <stdint.h>

#include "misc.h"
#include "packet.h"
#include "input.h"

#include "dbg.h"
#include "config.h"


bool process(input_t *input, packet_t *p) 
{
    log_info("Got packet for processing.");
    return true;
}

int main(void)
{
    #ifdef INPUT_PCAP
        log_info("Use pcap input");
        input_t * input = &pcap_input; 
    #endif
    #ifdef INPUT_LIBPCAP
        log_info("Use libpcap input");
        input_t * input = &libpcap_input; 
    #endif

    input->process = process;
    input->init(input);
    
    while(true);

    return 0;
}