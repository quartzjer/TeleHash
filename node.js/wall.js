var sys = require("sys");
var sw = require("./switch");

var _switch = new sw.createSwitch(undefined, process.argv[2], process.argv[3]);
_switch.on("+wall", function(remoteipp, telex, line) {
    console.log(new Date() + " <" + remoteipp + "> " + telex["+wall"]);
});

var stdin = process.openStdin();
stdin.setEncoding("UTF-8");

stdin.on('data', function(chunk){
    sys.print("local: " + chunk);
    for (var hash in _switch.master) {
        var telexOut = new sw.Telex(_switch.master[hash].ipp);
        telexOut["+wall"] = chunk;
        telexOut["+end"] = new sw.Hash(chunk).toString();
        _switch.send(telexOut);
    }
});

process.on('SIGINT', function() {
    console.log("Use Control-D to exit.");
});

stdin.on('end', function () {
    _switch.stop();
    process.exit(0);
});

_switch.start()

