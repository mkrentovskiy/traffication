(function($) {
    var d = new Date();
    var sockjs = null;
    var con_state = function(s) { $("#con_state").text(s) };

    $.app = {}
    $.app.init = function() {
        con_state("connecting...");
            
        sockjs = new SockJS('http://' + window.location.hostname + ":" +  window.location.port + '/a');
        sockjs.onopen = connect;
        sockjs.onclose = disconnect;
        sockjs.onmessage = message;
    }

    /*
        SockJS ops
    */
    function connect() {
        con_state("connected");
    }
    
    function reconnect() {
        setTimeout(function() { $.app.init(); }, 3000);
    }
    
    function disconnect(e) {
        con_state("disonnected");
        reconnect();
    }
    
    function send(val) {
        if(sockjs && sockjs.readyState == SockJS.OPEN) {
            var e = btoa(Bert.encode(val));
            sockjs.send(e);
        } else setTimeout(function() { send(val); }, 1000);
    }

    function message(m) {
        a = Bert.decode(atob(m.data)); 
        if(a && a[0] && a[0].value == "host") {
            switch(a[1].value) {
                case("show"): ui_show_host(a[2], make_obj(a[3])); break;
                case("update"): ui_update_host(a[2], make_obj(a[3])); break;
                case("hide"): ui_hide_host(a[2]); break;
            } 
        } else if(a && a[0] && a[0].value == "hosts") {
            for(i = 0; i < a[2].length; i++) {
                switch(a[1].value) {
                    case("show"): ui_update_host(a[2][i][0], make_obj(a[2][i][1])); break;
                    case("stat"): ui_show_stat(a[2][i][0], make_stat(a[2][i][1])); break;
                } 
            } 
        } else if(a && a[0] && a[0].value == "time") {
            uptime(a[2]);
        }
    }

    function uptime(s) {
        $("#uptime").text(moment.unix(s).fromNow());
        setTimeout(function() { uptime(s); }, 1000);
    }

    function make_obj(a) {
        if(a) {
            var obj = {};
            
            for(i = 0; i < a.length; i++) obj[(a[i][0])] = a[i][1];
            return obj;
        } else return false;
    }

    function ip2key(ip) {
        return ip.replace(/[\.\/]/g, "-");
    }

    function make_stat(a) {
        if(a) {
            var obj = {'headers': [], 'in_pkt': [], 'in_b': [], 'out_pkt': [], 'out_b': []};
            var j = 0;
            for(i = 0; i < a.length; i++) {
                if(a[i].length == 4) {
                    var k = obj['headers'].indexOf(a[i][0]);
                    if(k == -1) { 
                        obj['headers'][j] = a[i][0]; 
                        obj['in_pkt'][j] = 0; 
                        obj['in_b'][j] = 0; 
                        obj['out_pkt'][j] = 0;
                        obj['out_b'][j] = 0;  
                        k = j; 
                        j++; 
                    }
                    if(a[i][1].value == 'in') {
                        obj['in_pkt'][k] = a[i][2]; 
                        obj['in_b'][k] = a[i][3]; 
                    } else if(a[i][1].value == 'out') {                
                        obj['out_pkt'][k] = a[i][2]; 
                        obj['out_b'][k] = a[i][3]; 
                    }     
                }
            }
            return obj;
        } else return false;
    }

    /*
        UI
    */

    function ui_show_host(key, obj) {
        if(!obj) return;

        ui_hide_host(key);

        var dnode = $("#_t_i_host_list").clone();
        dnode.attr("id", key);
        $(".ip", dnode).text(obj["ip"]);
        $(".mac", dnode).text(obj["mac"]);
        $(".hostname", dnode).text(obj["hostname"]);
        $("#host_list").append(dnode);
    } 

    function ui_update_host(key, obj) {
        if(!obj) return;
        
        var dnode = $("#" + key);
        if(!dnode || dnode.length == 0) {
            ui_show_host(key, obj);
            ui_update_host(key, obj);
        } else {
            $("tr.host_param", dnode).detach();
            $.map(obj, function(v, k) {
                if(k != "ip" && k != "mac" && k != "hostname") {
                    var row = $("._t_host_param", dnode).clone();
                    $(".key", row).text(k);
                    $(".value", row).text(v); 
                    row.addClass("host_param").removeClass("_t_host_param").removeClass("hide");
                    $("table.host_params > tbody", dnode).append(row);
                }
            });
        }
    } 

    function ui_hide_host(key) {
        $("#" + key).detach();
    } 

    function ui_show_stat(ip, obj) {
        if(!obj || !obj['headers'] || obj['headers'].length == 0) return;
        
        var key = ip2key(ip);
        $("#proto_list > #" + key).detach();
    
        var dnode = $("#_t_i_proto_list").clone();
        dnode.attr("id", key);
        $(".ip", dnode).text(ip);
        
        var props = ['in_pkt', 'in_b', 'out_pkt', 'out_b'];
        $.map(obj['headers'], function(v, i) {
            var row = $("._t_proto_param", dnode).clone();
            $(".proto_name", row).text(v);
            $.map(props, function(k, j) { $("." + k, row).text(('' + obj[k][i]).replace(/(\d)(?=(\d\d\d)+([^\d]|$))/g, '$1 ')); })
            row.removeClass("_t_proto_param").removeClass("hide");
            $("table.proto_params > tbody", dnode).append(row);
        });
        
        $.map(props, function(k, j) { 
                chart($('.g_' + k, dnode).toArray(), obj['headers'], obj[k]); 
            });
        
        $("#proto_list").append(dnode);
    }

    /*
        Graph
    */

    function chart(place, headers, data) {
        var w = 300;
        var h = 300;
        var outerRadius = w / 2.4;
        var innerRadius = w / 32;

        var color = d3.scale.category20();
        var pie = d3.layout.pie();
        var m = data.reduce(function(pv, cv) { return pv + cv; }, 0);
        var arc = d3.svg.arc()
                .innerRadius(innerRadius)
                .outerRadius(outerRadius);
        var svg = d3.selectAll(place)
                .append("svg")
                .attr("width", w)
                .attr("height", h);

        var arcs = svg.selectAll("g.arc")
                .data(pie(data))
                .enter()
                .append("g")
                .attr("class", "arc")
                .attr("transform", "translate(" + outerRadius + ", " + outerRadius + ")");

        arcs.append("path")
                .attr("fill", function(d, c) {
                        return color(c);
                    })
                .attr("d", arc);
        
        arcs.append("text")
                .attr("transform", function(d) {
                        return "translate(" + arc.centroid(d) + ")";
                    })
                .attr("text-anchor", "middle")
                .text(function(d) {
                        if((d.value * 100 / m) > 15)
                            return headers[(data.indexOf(d.value))];
                        else 
                            return "";
                    });
    }

})(window.jQuery);

$(document).ready(function() { $.app.init(); });