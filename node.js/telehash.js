var dgram = require("dgram");
var func = require("./telehash-func");

var server = dgram.createSocket("udp4");
var own_ip_port, own_ip, own_port, own_end = null;
var kbuckets = func.create_kbuckets();
var switch_masterlist = [];
var taps_brute = [];

server.on("message", function (msg, rinfo) {
	console.log("RECV\t"+rinfo.address+":"+rinfo.port+"\t"+msg);
	var timestamp = new Date().getTime();
	var telex = JSON.parse(msg);
	if (!own_ip_port && telex._to) {
	    var toipp = func.parse_ip_port(telex._to);
	    own_ip_port = telex._to;
	    own_ip = toipp.ip;
	    own_port = toipp.port;
	    own_end = func.sha1(own_ip_port);
	}
	var switch_end = func.sha1(rinfo.address + ":" + rinfo.port);
	var switch_distance = func.bit_diff(own_end, switch_end);
	if (switch_distance < 0) return; // own IP:PORT
	var _switch = switch_masterlist[switch_end];
	if (!_switch) {
	    _switch = func.new_switch(rinfo.address+":"+rinfo.port,telex["_ring"]);
	    switch_masterlist[switch_end] = _switch;
	}
	if (telex["_ring"] && _switch.other_ring == 0) {
	    _switch.other_ring = func.to_int(telex["_ring"]);
	}
	if (telex["_line"] && _switch.other_ring == 0) {
	    _switch.other_ring = func.to_int(telex["_line"])/_switch.my_ring;
	}
	// TODO need to verify _line
	if (telex["_br"]) _switch.br_reported = func.to_int(telex["_br"]);
	_switch.last_message_received_timestamp = timestamp;
	_switch.br_received += msg.length;
	
	// process commands first

	if(telex._line)
	{
		// .see
		for(var i=0;i<telex[".see"].length;i++)
		{
			var seesw=telex[".see"][i];
			if(!switch_masterlist[seesw]) switch_masterlist[seesw] = func.new_switch(seesw);
			var sw = switch_masterlist[seesw];
			if(sw.seen) continue;
			sw.seen=true;
			func.send_line(server,{"+end":own_end},sw);
			func.send_line(server,{"+end":sw.end, "+pop":"th:"+own_ip_port, "_hop":1},_switch);
		}
		// .tap
		if(telex[".tap"] && telex[".tap"].length > 0)
		{
			taps_brute[_switch] = telex[".tap"];
		}
	}
	
	// process signals now

	// any direct +end is responded to with a .see
	if (telex["+end"] && func.to_int(telex._hop) == 0) {
	    var plus_end = telex["+end"];
	    var end_distance = func.bit_diff(own_end, plus_end);
	    func.send_line(server, {".see": [own_ip_port]}, _switch); // debug
	}
	
	// look for our own .tap for +pop
	if(telex["+end"] == own_ip_port && telex["+pop"])
	{
		var pop = telex["+pop"].match(/^th\:([\d\:\.]+)$/);
		if(!pop || switch_masterlist[pop[1]]) continue; // for now just skip if we've already seen this ipp
		var sw = switch_masterlist[pop[1]] = func.new_switch(pop[1]);
		func.send_line(server,{},sw); // smallest hello possible to ping open port
	}

	// TODO any signals are checked against taps
	if(func.to_int(telex._hop) < 4)
	{
		// brute force :(
		for(var i=0;i<taps_brute.length;i++)
		{
			var pass=0;
			for(var j=0;j<taps_brute[i].length;j++)
			{
				var rule = taps_brute[i][j];
				// todo
			}
		}
	}

    });

//server.bind(42424);
var seedipp = "208.68.163.247:42424"; // telehash.org
var seedend = func.sha1(seedipp);
var seedswitch = func.new_switch(seedipp);
switch_masterlist[seedend] = seedswitch;
func.send_line(server,{"+end":seedend},seedswitch);
