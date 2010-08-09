var dgram = require("dgram");
var func = require("./telehash-func");

var server = dgram.createSocket("udp4");
var own_ip_port, own_ip, own_port, own_end = null;
var kbuckets = func.create_kbuckets();
var switch_masterlist = [];

server.on("message", function (msg, rinfo) {
	var timestamp = new Date().getTime();
	var telex = JSON.parse(msg);
	if (!own_ip_port && telex["_to"]) {
	    var _to = telex["_to"];
	    var _to_ip_port = func.parse_ip_port(_to);
	    own_ip_port = _to;
	    own_ip = _to_ip_port.ip;
	    own_port = _to_ip_port.port;
	    own_end = func.sha1(own_ip_port);
	}
	var switch_end = func.sha1(rinfo.address + ":" + rinfo.port);
	var switch_distance = func.bit_diff(own_end, switch_end);
	if (switch_distance < 0) return; // own IP:PORT
	var _switch = switch_masterlist[switch_end];
	if (!_switch) {
	    _switch = {
		ip: rinfo.address,
		port: rinfo.port,
		br_received: 0,
		br_sent: 0,
		my_ring: func.rand_int(),
		other_ring: (telex["_ring"] ? telex["_ring"] : 0)
	    };
	    switch_masterlist[switch_end] = _switch;
	}
	if (telex["_ring"] && _switch.other_ring == 0) {
	    _switch.other_ring = func.to_int(telex["_ring"]);
	}
	if (telex["_br"]) _switch.br_reported = func.to_int(telex["_br"]);
	_switch.last_message_received_timestamp = timestamp;
	_switch.br_received += msg.length;
	if (telex["+end"]) {
	    var plus_end = telex["+end"];
	    var end_distance = func.bit_diff(own_end, plus_end);
	    if (func.to_int(telex["_hop"]) == 0) {
		
	    }
	    func.send_line(server, {"_see": []}, _switch); // debug
	}
    });

server.bind(42424);