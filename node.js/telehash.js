var dgram = require("dgram");
var func = require("./telehash-func");

var server = dgram.createSocket("udp4");
var own_ip_port, own_ip, own_port, own_end = null;
var kbuckets = func.create_kbuckets();
var switch_masterlist = [];

server.on("message", function (msg, rinfo) {
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
	if (_distance < 0) return; // own IP:PORT
	if (telex["+end"]) {
	    var plus_end = telex["+end"];
	    var end_distance = func.bit_diff(own_end, plus_end);
	    
	}
    });

server.bind(42424);