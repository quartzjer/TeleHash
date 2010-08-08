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

var bits2 = function (n, l) {
    var result = [];
    var input = n;
    for (i=0;i<l;i++) {
	result.push(input & 1);
	input = input >> 1;
    }
    return result;
};

assert.deepEqual([0,0,0], bits2(0, 3));
assert.deepEqual([1,0,0], bits2(1, 3));
assert.deepEqual([0,1,0], bits2(2, 3));
assert.deepEqual([1,1,0], bits2(3, 3));
assert.deepEqual([0,0,1], bits2(4, 3));
assert.deepEqual([1,0,1], bits2(5, 3));
assert.deepEqual([0,1,1], bits2(6, 3));
assert.deepEqual([1,1,1], bits2(7, 3));


