var telehash = require("./telehash");

var s = new telehash.createSwitch(process.argv[2], process.argv[3], process.argv[4]);
s.start()

