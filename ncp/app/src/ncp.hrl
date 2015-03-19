-define(PROTOCOLS, ["ukn","ftp","pop","smtp","imap","dns","ipp","http","mdns","ntp","netbios",
    "nfs","ssdp","bgp","snmp","xdmcp","smb","syslog","dhcp","postgres","mysql",
    "tds","ddl","i23v5","applejuice","directconnect","socrates","winmx","manolito","pando","filetopia",
    "iMESH","kontiki","openft","fasttrack","gnutella","edonkey","bittorrent","off","avi","flash",
    "ogg","mpeg","quicktime","realmedia","windowsmedia","mms","xbox","qq","move","rtsp",
    "feidian","icecast","pplive","ppstream","zattoo","shoutcast","sopcast","tvants","tvuplayer","veohtv",
    "qqlive","thunder","soulseek","gadugadu","irc","popo","jabber","msn","oscar","yahoo",
    "battlefield","quake","secondlife","steam","hl2","worldofwarcraft","telnet","stun","ipsec","gre",
    "icmp","igmp","egp","sctp","ospf","ipip","rtp","rdp","vnc","pcanywhere",
    "ssl","ssh","usenet","mgcp","iax","tftp","afp","stealthnet","aimini","sip",
    "truphone","icmpv6","dhcpv6","armagetron","crossfire","dofus","fiesta","florensia","guildwars","httpactivesync",
    "kerberos","ldap","maplestory","mssql","pptp","warcraft3","wokf","meebo","facebook","twitter",
    "dropbox","gmail","gmaps","youtube","skype","google","dcerpc","netflow","sflow","httpconnect",
    "httpproxy","citrix","netflix","lastfm","grooveshark","skyfileprepaid","skyfilerudics","skyfilepostpaid","citrixonline","apple",
    "webex","wgatsapp","appleicloud","viber","appleitunes","radius","windowsupdate","teamviewer","tuenti","lotusnotes",
    "sap","gtp","upnp","llmnr","remotescan","spotify","h323","openvpn","noe","ciscovpn","teamspeak",
    "tor","tor","rtcp","rsync","oracle","corba","ubuntuone"]).


-define(AFTER(Timeout, Event), {ok, _} = timer:send_after(Timeout, Event)).

%% p0f -f /etc/p0f/p0f.fp -i br-lan -s /tmp/p0f.sock -d
-define(PULL_PERIOD, 1000).
-define(P0F_COMMAND, "/root/p0f-client /tmp/p0f.sock ").

%% iptables
-define(PROTO_PULL_PERIOD, 2000).
-define(LOCALNET, "192.168.202.0/24").
-define(IPTABLES_FLUSH, "iptables -t mangle -F").
-define(IPTABLES_DST,  "iptables -t mangle -d ~p -~p POSTROUTING -m ndpi --~p -j ACCEPT").
-define(IPTABLES_SRC,  "iptables -t mangle -s ~p -~p PREROUTING -m ndpi --~p -j ACCEPT").
-define(IPTABLES_STAT, "iptables -t mangle -L -v -n --exact").

