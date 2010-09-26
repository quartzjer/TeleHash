var assert = require
var telehash = require("./telehash");

var abcHash = new telehash.Hash("abc");
console.log("abc -> " + abcHash.toString());
assert.equal("a9993e364706816aba3e25717850c26c9cd0d89d", abcHash.toString());

