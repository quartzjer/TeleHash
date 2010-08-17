var assert = require("assert");
var crypto = require("crypto");
//var dgram = require("dgram");

var sha1 = function(data) {
    return crypto.createHash("sha1").update(data).digest("hex");
};

assert.equal("a9993e364706816aba3e25717850c26c9cd0d89d", sha1("abc"));

exports.sha1 = sha1;

var parse_ip_port = function(ip_port) {
    var result = ip_port.split(":");
    return {ip: result[0], port: parseInt(result[1])};
};

var parse_ip_port_1 = parse_ip_port("123.123.123.123:12345");
assert.equal("123.123.123.123", parse_ip_port_1.ip);
assert.equal(12345, parse_ip_port_1.port);

exports.parse_ip_port = parse_ip_port;

var create_kbuckets = function () {
    var result = [];
    for (i=0;i<160;i++) {
	result.push([]);
    }
    return result;
};

assert.equal(160, create_kbuckets().length);

exports.create_kbuckets = create_kbuckets;

var bits = function (n, l) {
    var result = [];
    var input = n;
    for (i=0;i<l;i++) {
	result.push(input & 1);
	input = input >> 1;
    }
    return result;
};

assert.deepEqual([0,0,0], bits(0, 3));
assert.deepEqual([1,0,0], bits(1, 3));
assert.deepEqual([0,1,0], bits(2, 3));
assert.deepEqual([1,1,0], bits(3, 3));
assert.deepEqual([0,0,1], bits(4, 3));
assert.deepEqual([1,0,1], bits(5, 3));
assert.deepEqual([0,1,1], bits(6, 3));
assert.deepEqual([1,1,1], bits(7, 3));
assert.deepEqual([0,0,0,1], bits(8, 4));

var hex_dec = {'0': 0, '1': 1 , '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9, 'A': 10, 'B': 11, 'C': 12, 'D': 13, 'E': 14, 'F': 15};

var bit_diff = function (a, b) {
    var _a = a.toUpperCase();
    var _b = b.toUpperCase();
    var len = _a.length;
    if (_b.length < len) len = _b.length;
    if (_a == _b) return -1;
    var result = 0;

    for (i=0;i<len; i++) {
	var a_char = _a.charAt(i);
	var b_char = _b.charAt(i);
	if (a_char == b_char) {
	    result += 4;
	} else {
	    var a_num = hex_dec[a_char];
	    var b_num = hex_dec[b_char];
	    var a_bits = bits(a_num, 4);
	    var b_bits = bits(b_num, 4);
	    var diff = 0;
	    for (x=0;x<4;x++) {
		if (a_bits[x] == b_bits[x]) {
		    diff++;
		} else {
		    return result + diff;
		}
	    }
	}
    }
};

assert.equal(0, bit_diff('0', '1'));
assert.equal(1, bit_diff('1', '3'));
assert.equal(2, bit_diff('2', '6'));
assert.equal(3, bit_diff('7', 'F'));
assert.equal(4, bit_diff('F0', 'F1'));
assert.equal(5, bit_diff('F1', 'F3'));
assert.equal(6, bit_diff('F2', 'F6'));
assert.equal(7, bit_diff('F7', 'FF'));
assert.equal(16, bit_diff('FFFF0', 'FFFF1'));

exports.bit_diff = bit_diff;

var to_int = function (n) {
    if (!n) return 0;
    if (typeof(n) == "string") return parseInt(n);
    return n;
};

assert.equal(0, to_int(null));
assert.equal(1, to_int(1));
assert.equal(2, to_int("2"));

exports.to_int = to_int;

var send_line = function(client, telex, _switch) {
    var ip = _switch.ip;
    var port = _switch.port;
    telex["_to"] = ip + ":" + port;
    var _br_received = _switch.br_received;
    telex["_br"] = _br_received;
    var my_ring = _switch.my_ring;
    var other_ring = _switch.other_ring;
    if (other_ring == 0) {
		telex["_ring"] = my_ring;
    } else {
		telex["_line"] = my_ring * other_ring;
    }
    var msg = JSON.stringify(telex);
    client.send(new Buffer(msg), 0, msg.length, port, ip, function (err) {
		console.log("SEND\t"+msg);
	    if (err) throw err;
	    _switch.br_sent += msg.length;
	});
};

exports.send_line = send_line;

var new_switch = function(ip_port,ring) {
    var ipp = parse_ip_port(ip_port);
 	return {
		ipp: ipp,
		ip: ipp.ip,
		port: ipp.port,
		end: sha1(ipp),
		br_received: 0,
		br_sent: 0,
		my_ring: rand_int(),
		see: {},
		other_ring: (ring ? ring : 0)
	    };
};

exports.new_switch = new_switch;

var near_to = function(end, sw) {
	see = sw.see;

	// if(see.keys.length < 5) need to see.push(near_to(sw.end,seedswitch)) but how to get seedswitch?
	
	sorted = see.sort(); // need to sort by distance from end
	
	// if the given switch is the closest we can return this result
	if(sorted[0] == sw.ipp)
	{
		// if this end *is* for this switch, update it's cache
		if(end == sw.end)
		{
			sw.see = sorted; // should we slice out just top 5?
			// TODO seed the caches, loop through each and insert sw.ipp into their .see
		}
		return sorted;
	}
	
	// recurse to closest switch
	return near_to(end,sorted[0]);
}

exports.near_to = near_to;

var rand_int = function() {
    var result = Math.floor(Math.random()*32768);
    while (result <= 1000) {
	result = Math.floor(Math.random()*32768);
    }
    return result;
};

var rand_int_1 = rand_int();
assert.ok(rand_int_1 > 1000 && rand_int_1 < 32768);

exports.rand_int = rand_int;