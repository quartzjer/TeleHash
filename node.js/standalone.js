var sw = require("./switch");

var _switch = new sw.createSwitch(process.argv[2], process.argv[3], process.argv[4]);
_switch.start()

