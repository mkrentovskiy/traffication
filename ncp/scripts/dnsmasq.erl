#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa /root/ncp/app/ebin -sname dnsmasq@localhost -setcookie D4aL7F9K9n1ldDFZ7FW2Lvj

% Sample input: 
% add c4:85:08:97:43:11 192.168.222.153 mkrentovskiy-UX32VD
% del c4:85:08:97:43:11 192.168.222.153 mkrentovskiy-UX32VD
% add c4:85:08:97:43:11 192.168.222.153 mkrentovskiy-UX32VD
% old c4:85:08:97:43:11 192.168.222.153 mkrentovskiy-UX32VD

main([Op, MAC, IP, Hostname]) ->
    true = net_kernel:connect_node(ncp@localhost),
    ok = global:sync(),
    Operation = case Op of
        "add" -> add;
        "old" -> add;
        "del" -> del
    end,
    ok = data_source:dhcp(Operation, [{"ip", IP}, {"mac", MAC}, {"hostname", Hostname}]);

main(Any) ->
    io:format("Undefined input - ~p~n", [Any]). 
