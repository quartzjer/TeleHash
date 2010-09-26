var sys = require("sys");
var telehash = require("./telehash");

var s = new telehash.createSwitch(undefined, process.argv[2], process.argv[3]);
s.on("+wall", function(remoteipp, telex, line) {
    console.log(new Date() + " <" + remoteipp + "> " + telex["+wall"]);
});

var endHash = new telehash.Hash("42");

var tap = {};
tap.is = {};
tap.is["+end"] = endHash.toString();
tap.has = ["+wall"];

s.addTap(tap);

var stdin = process.openStdin();
stdin.setEncoding("UTF-8");

stdin.on('data', function(chunk){
    console.log("local: " + chunk);
    
    var ckeys = telehash.keys(s.master)
    .filter(function(x) { return s.master[x].ipp != s.selfipp; })
    .sort(function(a,b) { return endHash.distanceTo(a) - endHash.distanceTo(b) })
    .slice(0,3).forEach(function(ckey){
        var target = s.master[ckey].ipp;
        if (!target) {
            return;
        }
        
        console.log("attached to " + target + " at distance " + endHash.distanceTo(target));
        var telexOut = new telehash.Telex(target);
        telexOut["+wall"] = chunk;
        telexOut["+guid"] = new Date().getTime();
        telexOut["_hop"] = 1;
        telexOut["+end"] = endHash.toString();
        s.send(telexOut);
    });
});

process.on('SIGINT', function() {
    console.log("Use Control-D to exit.");
});

stdin.on('end', function () {
    s.stop();
    process.exit(0);
});

s.start()

