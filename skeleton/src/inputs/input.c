#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <netinet/ether.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <netinet/udp.h>

#include "misc.h"
#include "packet.h"
#include "input.h"

#include "dbg.h"
#include "config.h"


void fill_from_raw(packet_t *p) 
{
    uint8_t *ptr = p->header;

    /* extract l2 */
    struct ether_header *l2 = (struct ether_header *) ptr;
    uint16_t etype = ntohs(l2->ether_type);
    ptr += sizeof(struct ether_header);
    
    p->l2.vlan_id = 0;
    memcpy(p->l2.src_mac.o, l2->ether_shost, ETH_ALEN);
    memcpy(p->l2.dst_mac.o, l2->ether_dhost, ETH_ALEN);

    debug("Ether type %x", etype);

    // TODO: implement MPLS decaps

    if(etype == ETHERTYPE_VLAN) {
        p->l2.vlan_id = ntohs(*((vlan_id_t *) ptr)) & 0x0fff;
        ptr += sizeof(uint16_t);

        etype = ntohs(*((uint16_t *) ptr));
        ptr += sizeof(uint16_t);

        debug("VLAN id %x", p->l2.vlan_id);
    }; 

    // TODO: implement GRE/PPP decaps
                
    if(likely(etype == ETHERTYPE_IP)) {
        /* extract l3 */
        struct ip *iphdr = (struct ip *) ptr;
        if(likely(iphdr->ip_v == IPVERSION && iphdr->ip_src.s_addr > 0 && iphdr->ip_dst.s_addr > 0)) {
            p->l3_header = ptr;
            p->l3.proto = iphdr->ip_p;
            p->l3.src.a = ntohl(iphdr->ip_src.s_addr);
            p->l3.dst.a = ntohl(iphdr->ip_dst.s_addr);

            debug("Proto %d, src %d.%d.%d.%d -> dst %d.%d.%d.%d",
                p->l3.proto,
                p->l3.src.o[3], p->l3.src.o[2], p->l3.src.o[1], p->l3.src.o[0],
                p->l3.dst.o[3], p->l3.dst.o[2], p->l3.dst.o[1], p->l3.dst.o[0]);

            ptr += (iphdr->ip_hl * sizeof(uint32_t)) + iphdr->ip_off;

            /* extract l4 */
            p->l4_header = ptr;
            if(p->l3.proto == IPPROTO_TCP) {
                struct tcphdr *tcphdr = (struct tcphdr *) ptr;
                
                p->l3.src_p = ntohs(tcphdr->th_sport);
                p->l3.dst_p = ntohs(tcphdr->th_dport);

                ptr += tcphdr->th_off * sizeof(uint32_t);
            } else if(p->l3.proto == IPPROTO_UDP) {
                struct udphdr *udphdr = (struct udphdr *) ptr;

                p->l3.src_p = ntohs(udphdr->uh_sport);
                p->l3.dst_p = ntohs(udphdr->uh_dport);

                ptr += sizeof(udphdr);
                // TODO: implement L2TP decaps
            } else {
                printf("Not TCP or UDP, skipping.\n");
            };

            debug("Proto - %d, src port %d, dst port %d", p->l3.proto, p->l3.src_p, p->l3.dst_p);
        } else {
            printf("Possible error in IP-header.\n");
        }
    } else if(etype == ETHERTYPE_IPV6) {
        // TODO: implement IPv6 support
    }

    p->payload = ptr;
}

void preprocess(packet_t *p, bool is_raw)
{
    if(is_raw) fill_from_raw(p);
}
