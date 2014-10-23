#ifndef __CONFIG_H
#define __CONFIG_H

#define INPUT_PCAP
#define PCAP_FILENAME "sample.pcap"

// #define INPUT_LIBPCAP
#define LIBPCAP_DEV "wlan0"
#define LIBPCAP_BUFSIZE BUFSIZ
#define LIBPCAP_PROMISC 1
#define LIBPCAP_TOMS 0

#endif