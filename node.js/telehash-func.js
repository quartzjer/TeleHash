var assert = require("assert");
var crypto = require("crypto");

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