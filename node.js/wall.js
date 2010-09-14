var sys = require("sys");
var sw = require("./switch");

var _switch = new sw.createSwitch(undefined, process.argv[2], process.argv[3]);
_switch.on("+wall", function(remoteipp, telex, line) {
    console.log(new Date() + " <" + remoteipp + "> " + telex["+wall"]);
});

var endHash = new sw.Hash("42");

var tap = [];
tap.is = {};
tap.is["+end"] = endHash.toString();
tap.has = ["+wall"];

_switch.addTap(tap);

var stdin = process.openStdin();
stdin.setEncoding("UTF-8");

stdin.on('data', function(chunk){
    console.log("local: " + chunk);
    
    var ckeys = sw.keys(_switch.master)
    .filter(function(x) { return _switch.master[x].ipp != _switch.selfipp; })
    .sort(function(a,b) { return endHash.distanceTo(a) - endHash.distanceTo(b) })
    .slice(0,3).forEach(function(ckey){
        var target = _switch.master[ckey].ipp;
	    console.log("attached to " + target + " at distance " + endHash.distanceTo(target));
        var telexOut = new sw.Telex(target);
        telexOut["+wall"] = chunk;
        telexOut["+guid"] = new Date().getTime();
        telexOut["_hop"] = 1;
        telexOut["+end"] = endHash.toString();
        _switch.send(telexOut);
	});
});

process.on('SIGINT', function() {
    console.log("Use Control-D to exit.");
});

stdin.on('end', function () {
    _switch.stop();
    process.exit(0);
});

_switch.start()

