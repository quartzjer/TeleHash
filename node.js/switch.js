var sys = require("sys");
var events = require("events");
var crypto = require("crypto");
var dgram = require("dgram");
var dns = require("dns");

/**
 * Emulate this helpful Object method if < JS 1.8.5.
 */
if (!Object.prototype.keys) {
    Object.prototype.keys = function() {
        var result = [];
        for (var key in this) {
            result[result.length] = key;
        }
        return result;
    }
}

/**
 * See time(2).
 */
function time() {
    return new Date().getTime();
}

/**
 * Return a random integer 1..n inclusive.
 */
function rand(n) {
  return ( Math.floor ( Math.random ( time() ) * n + 1 ) );
}

/**
 * Format a byte as a two digit hex string.
 */
function byte2hex(d) {
    return d < 16 ? "0" + d.toString(16) : d.toString(16);
}

/**
 * Test if an Object is an Array.
 */
function isArray(o) {
    return o.constructor == Array;
}

/**
 * Test if an Object is a String.
 */
function isString(o) {
    return o.constructor == String;
}

/**
 * Create a new Telex.
 * If the first argument is a string, it is used as the _to endpoint
 * for the new Telex. Otherwise all key-value pairs in the argument
 * are copied into the new Telex.
 *
 * Example:
 *  new Telex("somehost.com:41234")
 *  new Telex({_to: "somehost.com:41234", _ring: 8980})
 */
function Telex(arg){
    if (arg.constructor == String) {
        this._to = arg;
    }
    else {
        for (var key in arg) {
            this[key] = arg[key];
        }
    }
}

/**
 * Test if a telex has signals. Signals start with a '+'.
 */
Telex.prototype.hasSignals = function() {
    return this.keys().some(function(x){ x[0] == '+' });
}

/**
 * Get all the commands in this telex.
 * Returns an object of command names mapped to command parameter,
 * with leading '.' stripped off of command name.
 */
Telex.prototype.getCommands = function() {
    var result = {};
    this.keys().filter(function(x){ x[0] == '.' }).forEach(function(x){
        result[x] = this[x];
    });
    return result;
}

exports.Telex = Telex;

function Switch(bindPort, bootHost, bootPort){
    this.bindPort = bindPort == undefined ? 0 : bindPort;
    this.bootHost = bootHost == undefined ? "telehash.org" : bootHost;
    this.bootPort = bootPort == undefined ? 42424 : bootPort;
    this.seedipp = bootHost + ":" + bootPort;
    this.selfipp = null;
    this.selfhash = null;
    this.connected = false;
    this.commandHandlers = [];
    
    this.master = {};
    this.tap_js = undefined;
    this.taps = {};
    
    this.NBUCKETS=160; // 160 bits, since we're using SHA1
    this.buckets = [];
    
    // create the array of buckets
    for (var i = 0; i < this.NBUCKETS; i++) {
        this.buckets[i] = {};
    }
    
    var self  = this;
    
    this.server = dgram.createSocket("udp4", function(msgstr, rinfo){
        self.recv(msgstr, rinfo);
    });
    
    this.server.on("listening", function(){
        // Lookup the bootstrap and attempt to connect
        dns.resolve4(self.bootHost, function(err, addresses) {
            if (err) {
                throw err;
            }
            if (addresses.length == 0) {
                throw "Cannot resolve bootstrap host '" + self.bootHost;
            }
            var bootIP = addresses[0];
            var bootEndpoint = bootIP + ":" + self.bootPort;
            
            // Start the bootstrap process
            self.startBootstrap(bootEndpoint);
            
            // Retry the bootstrap every 10s until it completes
            var bootstrapRetryID = setInterval(function() {
                if (self.connected) {
                    clearInterval(bootstrapRetryID);
                }
                else {
                    self.scanlines();
                    self.startBootstrap(bootEndpoint);
                }
            }, 10000);
            
        });
        
        var address = self.server.address();
        console.log([
            "server listening ", address.address, ":", address.port].join(""));
    });
    
    // Register built-in command handlers
    
    this.on(".natr", function(source, telex, line) {
        self._natr(source, telex, line);
    });
    this.on(".nat", function(source, telex, line) {
        self._nat(source, telex, line);
    });
    this.on(".see", function(source, telex, line) {
        self._see(source, telex, line);
    });
    this.on(".tap", function(source, telex, line) {
        self._tap(source, telex, line);
    });
}

sys.inherits(Switch, events.EventEmitter);
exports.Switch = Switch;

exports.createSwitch = function() {
    return new Switch();
}

/**
 * Start the switch.
 * The switch will start listening on its bind port 
 * and start the bootstrap process.
 */
Switch.prototype.start = function() {
    this.server.bind(this.bindPort);
}

/**
 * Stop the switch.
 */
Switch.prototype.stop = function() {
    this.server.close();
}

/**
 * Start the bootstrap process by sending a telex to the 
 * bootstrap switch.
 */
Switch.prototype.startBootstrap = function(seed){
    var bootTelex = new Telex(seed);
    bootTelex["+end"] = new Hash(seed).far().toString();
    this.send(bootTelex);
}

/**
 * Complete the bootstrap by processing the response from
 * the bootstrap switch.
 */
Switch.prototype.completeBootstrap = function(source, telex) {
    this.connected = true;
    this.selfipp = telex._to;
    this.selfhash = new Hash(this.selfipp).toString();
    
    console.log(["\tSELF[", telex._to, "]"].join(""));
    
    var line = this.getline(selfipp);
    line.visible=1; // flag ourselves as default visible
	line.rules = this.tap_js; // if we're tap'ing anything
	
    // WE are the seed, haha, remove our own line and skip
    if (this.selfipp == source) {
        console.log("\tWe're the seed!\n");
        delete this.lines[this.selfipp];
    }
}

/**
 * Dispatch incoming raw messages.
 * This method is called automatically on incoming dgram message.
 */
Switch.prototype.recv = function(msgstr, rinfo) {
    var telex = new Telex(JSON.parse(msgstr));
    var source = rinfo.address + ":" + rinfo.port;
    console.log([
        "RECV from ", source, ": ", JSON.stringify(telex)].join(""));
    
    if (this.selfipp == null && "_to" in telex) {
        this.completeBootstrap(source, telex);
    }
    
    // if this is a switch we know, check a few things
    var line = this.getline(source, telex._ring);
    var lstat = this.checkline(line, telex, msgstr.length);
    if (!lstat) {
        console.log(["\tLINE FAIL[", JSON.stringify(line), "]"].join(""));
        return;
    }
    else {
        console.log(["\tLINE STATUS ", (telex._line ? "OPEN":"RINGING")].join(""));
    }
    
    // Process commands if the line is open
    if (telex._line) {
        for (var key in telex.getCommands()) {
            this.emit(key, source, telex, line);
        }
    }
    
    if (telex.hasSignals()) {
        var hop = parseInt(telex._hop)
        if ("+end" in telex && hop == 0) {
            var cipps = this.near(telex["+end"]);
            var telexOut = new Telex(source);
            // TODO: this is where dampening should happen to not advertise switches that might be too busy
            telexOut[".see"] = cipps;
            this.send(telexOut);
        }
        
        if (hop < 4) {
            var switches = {};
            telex.keys().filter(function(x){ this.taps[x] }).forEach(function(sig){
                this.taps[sig].forEach(function(sw) {
                    switches[sw] ? switches[sw]+1 : 1;
                });
            });
            
            switches.keys().forEach(function(sw){
                var pass = 0;
                this.lines[sw]["rules"].forEach(function(rule) {
                    console.log(["\tFWD CHECK IS ", sw, "\t", JSON.stringify(rule)].join(""));
                    // check that all the "is" are in this telex and match exactly
                    if (rule["is"].keys.filter(function(x){ telex[x] == rule["is"][x] }).length == 0) {
                        continue;
                    }
                    
                    // pass fail if any has doesn't exist
                    pass++;
                    rule["has"].forEach(function(sig){
                        if (!telex[sig]) {
                            pass = 0;
                        }
                    });
                    break;
                });
                
                // forward this switch a copy
                if (pass) {
                    var telexOut = new Telex(sw);
                    telex.keys().filter(function(x){ x && x[0] == '+' }).forEach(function(sig){
                        telexOut[sig] = telex[sig];
                    })
                    telexOut["_hop"] = hop + 1;
                    send(telexOut);
                }
                else{
                    console.log("\tCHECK MISS\n");
                }
            });
        }
        
    }
    
}

/**
 * Handle the .natr TeleHash command.
 */
Switch.prototype._natr = function(source, telex, line) {
    if (telex[".natr"] && self.lines[telex[".natr"]]) {
        var telexOut = new Telex(telex[".natr"]);
        telexOut[".nat"] = source; 
        this.send(telexOut);
    }
}

/**
 * Handle the .nat TeleHash command.
 */
Switch.prototype._nat = function(source, telex, line) {
    this.send(new Telex(telex[".nat"]));
}

/**
 * Handle the .see TeleHash command.
 */
Switch.prototype._see = function(source, telex, line) {
    // loop through and establish lines to them (just being dumb for now and trying everyone)
    var seeipps = telex[".see"];
    if (!seeipps) { return; }
    
    for (var i = 0; i < seeipps.length; i++) {
        var seeipp = seeipps[i];
        if (this.selfipp == seeipp[i]) {
            // skip ourselves :)
            continue;
        }
        
		// they're making themselves visible now, awesome
		if (seeipp == remoteipp && !line.visible) {
			console.log(["\t\tVISIBLE ", remoteip].join(""));
			line.visible=1;
			this.near_to(line.end, self.selfipp).map(function(x) { line.neighbors[x]=1; });
			this.near_to(line.end, remoteipp); // injects this switch as hints into it's neighbors, fully seeded now
		}
		
		if (this.master[new Hash(seeipp).toString()]) {
		    continue; // skip if we know them already
		}
		
		// XXX todo: if we're dialing we'd want to reach out to any of these closer to that $tap_end
		// also check to see if we want them in a bucket
		if (this.bucket_want(seeipp, this.buckets)) {
		    
			// send direct (should open our outgoing to them)
			var telexOut = new Telex(seeipp);
			telexOut["+end"] = this.selfhash;
			this.send(telexOut);
			
			// send pop signal back to the switch who .see'd us in case the new one is behind a nat
			telexOut = new Telex(remoteipp);
			telexOut["+end"] = sha1_hex($seeipp);
			telexOut["+pop"] = "th:" + self.selfipp;
			telexOut["_hop"] = 1;
			this.send(telexOut);
		}
    }
}

/**
 * Handle the .tap TeleHash command.
 */
Switch.prototype._tap = function(source, telex, line) {
    if (".tap" in telex && isArray(telex[".tap"])) {
        var tap = telex[".tap"];
        line["rules"] = [];
        this.tapwipe(source);
        for (var i = 0; i < tap.length; i++) {
            var rule = tap[i];
            
            // sanity check, we only understand "is" and "has", and signal filters
            var is = rule["is"];
            delete rule["is"];
            var has = rule["has"];
            delete rule["has"];
            
            // outright skip rules we don't strictly know
            if (rule.length > 0) { continue; }
            if (hasSignals(is) || hasSignals(has)) { continue; }
            if (is.filter(function(x){ x.length == 0 }).length > 0) { continue; }
            
            rule["is"] = is;
            rule["has"] = has;
            
            // any possible sigs are added to a sort of index to help in filtering incoming
            for (var sig in is) {
                this.tapadd(source, sig);
            }
            for (var sig in has) {
                this.tapadd(source, sig);
            }
            
            line["rules"][line["rules"].length] = rule;
        }
        
        // notify back the rules we stored
        var telexOut = new Telex(source);
        telexOut[".tapt"] = line["rules"];
        this.send(telexOut);
    }
}

/**
 * Send a telex.
 */
Switch.prototype.send = function(telex) {
    var line = this.getline(telex._to);
    
    // check br and drop if too much
    if (line.bsent - line.brin > 10000) {
        console.log("\tMAX SEND DROP\n");
        return;
    }
    
    // if a line is open use that, else send a ring
    if ("line" in line) {
        telex._line = parseInt(line["line"]);      
    }
    else {
        telex._ring = parseInt(line["ringout"]);
    }
    
    // update our bytes tracking and send current state
    telex._br = line.brout = line.br;
    var msg = new Buffer(JSON.stringify(telex), "utf8");
    
    line.bsent += msg.length;
    line.sentat = time();
    console.log(["\tSEND[", telex._to, "]\t", msg].join(""));
    
    this.server.send(msg, 0, line.bsent, line.port, line.host);
}

/**
 * Get the line for a host:port endpoint,
 * creating a new line if necessary.
 */
Switch.prototype.getline = function(endpoint) {
    if (endpoint.length < 4) {
        return undefined; // sanity check
    }
    
	var endpointHash = new Hash(endpoint);
	if (!this.master[endpointHash] || this.master[endpointHash].ipp != endpoint) {
        var endpieces = endpoint.split(":");
        var host = endpieces[0];
        var port = endpieces[1];
		var wip = gethostbyname(host);
        
        this.lines[endpoint] = {
            ipp: endpoint,
            port: port,
            host: host,
            ringout: rand(32768),
            init: time(),
            seenat: 0,
            sentat: 0,
            lintat: 0,
            br: 0,
            brout: 0,
            brin: 0,
            bsent: 0
        };
    }
    return this.lines[endpoint];
}

/**
 * Check a line's status.
 * True if open, false if ringing.
 */
Switch.prototype.checkline = function(line, t, br) {
    if (!line) {
        return false;
    }
    
    // first, if it's been more than 10 seconds after a line opened, 
    // be super strict, no more ringing allowed, _line absolutely required
    if (line.lineat > 0 && time() - line.lineat > 10) {
        if (t._line != line.line) {
            return false;
        }
    }
    
    // second, process incoming _line
    if (t._line) {
        if (line.ringout <= 0) {
            return false;
        }
        
        // be nice in what we accept, strict in what we send
        t._line = parseInt(t._line);
        
        // must match if exist
        if (line.line && t._line != line.line) {
            return false;
        }
        
        // must be a product of our sent ring!!
        if (t._line % line.ringout != 0) {
            return false;
        }
        
        // we can set up the line now if needed
        if(line.lineat == 0) {
            line.ringin = t._line / line.ringout; // will be valid if the % = 0 above
            line.line = t._line;
            line.lineat = time();
            this.see(line.ipp); // make sure they get added to a bucket too
        }
    }
    
    // last, process any incoming _ring's (remember, could be out of order, after a _line)
    if (t._ring) {
        // already had a ring and this one doesn't match, should be rare
        if (line.ringin && t._ring != line.ringin) {
            return false;
        }
        
        // make sure within valid range
        if (t._ring <= 0 || t._ring > 32768) {
            return false;
        }
        
        // we can set up the line now if needed
        if (line.lineat == 0) {
            line.ringin = t._ring;
            line.line = line.ringin * $line.ringout;
            line.lineat = time();
            this.see(line.ipp); // make sure they get added to a bucket too
        }
    }
    
    // we're valid at this point, line or otherwise, track bytes
    console.log([
        "\tBR ", line.ipp, " [", line.br, " += ",
        br, "] DIFF ", (line.bsent - t._br)].join(""));
    line.br += br;
    line.brin = t._br;
    
    // they can't send us that much more than what we've told them to, bad!
    if (line.br - line.brout > 12000) {
        return false;
    }
    
    // XXX if this is the first seenat,
    // if we were dialing we might need to re-send our telex as this could be a nat open pingback
    line.seenat = time();
    return true;
}

/**
 * Update status of all lines, removing stale ones.
 */
Switch.prototype.scanlines = function() {
    var now = time();
    for (var endpoint in this.lines) {
        if (line == this.selfipp) {
            continue;
        }
        
        var line = this.lines[endpoint];
        if ((line.seenat == 0 && now - line.init > 70)
                || (line.seenat != 0 && now - line.seenat > 70)) {
            console.log(["\tPURGE[", endpoint, "]"].join(""));
            delete this.lines[endpoint];
            continue;
        }
        
        var telex = new Telex(endpoint);
        telex["+end"] = this.selfhash;
        this.send(telex);
    }
    
    if (this.lines.length == 0 && this.selfipp != this.seedipp) {
        console.log("\tOFFLINE");
    }
}

/**
 * generate a .see for an +end, using a switch as a hint
 */
Switch.prototype.near_to = function(end, ipp){
    if (isString(end)) { end = new Hash(end); }
    
	var line = this.master[new Hash(ipp).toString()];
	if (!line) {
	    return undefined; // should always exist except in startup or offline, etc
	}
    
	// of the existing and visible cached neighbors, sort by distance to this end
	var see = line.neighbors
	    .map(function(x){ x.constructor == Hash ? x : new Hash(x) }) // make sure these are Hashes
	    .filter(function(x){ return this.master[x.toString()] && this.master[x.toString()].visible })
	    .sort(function(a,b){ return end.distanceTo(a) - end.distanceTo(b) });
    
	console.log(["\t\tNEARTO ", end, '\t', ipp, '\t', 
	    line.neighbors.keys().length, ">", see.length, '\t',
	    see[0].distanceTo(endHash), "=", see[0].distanceTo(line.end)].join(""));
    
    if (!see.length) {
        return undefined;
    }
    
	// it's either us or we're the same distance away so return these results
	if ((see[0].equals(line.end) 
    	    || (see[0].distanceTo(end) == see[0].distanceTo(line.end)) {
		// this +end == this line then replace the neighbors cache with this result 
		// and each in the result walk and insert self into their neighbors
		if (line.end == end) {
			console.log(["\t\tNEIGH for ", end, " was ", line.neighbors.keys().join(","), " ", see.length].join(""));
			var neigh = map {$_=>1} perlsucks(5,\@see);
			line.neighbors = neigh;
			console.log(["\t\tNEIGH for ", end, " is ", line.neighbors.keys().join(","), " ", see.length].join(""));
			for (var hash in line.neighbors) {
				this.master.hash.neighbors.end=1;
				console.log(["\t\tSEED ", ipp, " into ", this.master.hash.ipp].join(""));
			}
		}
		console.log(["\t\t\tSEE distance=", end.distanceTo(see[0]), " count=", see.length].join(""));
		return see;
	}

	// whomever is closer, if any, tail recurse endseeswitch them
	return this.near_to(end, this.master[see[0].toString()].ipp);
}

/**
 * 
 */
Switch.prototype.near = function(end) {
    // find active switches
    var bto = new Hash(end);
    var bme = new Hash(this.selfhash);
    var start = bto.or(bme).sbit();
    if (start < 0 || start > this.NBUCKETS) { // err, this needs to be handled better or something
        start = 0
    }
    
    var ret = [];
    
    // first check all buckets closer
    console.log(["\tNEAR[", start, " ", end, "]"].join(""));
    var pos = start + 1;
    while (--pos > 0) {
        //printf "%d/%d %s",$pos,scalar @ret,Dumper($buckets->[$pos]);
        ret[ret.length] = this.buckets[pos].keys().filter(
            function(x){ this.lines[x]["lineat"] }); // only push active switches
        
        if (ret.length >= 5) {
            break;
        }
    }
    
    // the check all buckets further
    for (pos = start + 1; pos < this.NBUCKETS; pos++) {
        //#printf "%d/%d ",$pos,scalar @ret;
        ret[ret.length] = this.buckets[pos].keys().filter(
            function(x){ this.lines[x]["lineat"] }); // only push active switches
        
        if (ret.length >= 5) {
            break;
        }
    }
    
    var hashes = {};
    var ckeys = [];
    ret.forEach(function(x){
        var h = new Hash(x); 
        hashes[h.toString()] = x;
        ckeys[ckeys.length] = h;
    });
    ckeys.sort(function(a,b){ return bto.or(a).cmp(bto.or(b)); }); // sort by closest to the end
    
    hashes[this.selfhash] = this.selfipp; // include ourselves always
    //printf("\tfrom %d switches, closest is %d\n",scalar @ckeys, bix_sbit(bix_or($bto,$ckeys[0])));
    
    ret = ckeys.slice(0,5).map(function(h){ return hashes[h.toString()]; }); // convert back to ip:ports, top 5
    return ret;
}

Switch.prototype.see = function(ipp) {
    // see if we want to try this switch or not, and maybe prune the bucket
    console.log(["SEE: ", new Hash(ipp).or(new Hash(this.selfhash))].join(""));
    
    var pos = new Hash(ipp).or(new Hash(this.selfhash)).sbit();
    console.log(["\tBUCKET[", pos, " ", ipp, "]"].join(""));
    
    if (pos < 0 || pos >= this.NBUCKETS) {
        return false;
    }
    
    if (ipp in this.buckets[pos]) {
        this.buckets[pos][ipp]++;
    }
    else {
        this.buckets[pos][this.selfipp] = 1;
    }
    
    return true; // for now we're always taking everyone, in future need to be more selective when the bucket is "full"!
}

// add a tap indicator for this switch/signal
Switch.prototype.tapadd = function(ipp, sig) {
    if (!this.taps[sig]) {
        this.taps[sig] = {};
    }
    if (!this.taps[sig][ipp]) {
        this.taps[sig][ipp] = 0;
    }
    this.taps[sig][ipp]++;
}

// remove all taps for a switch
Switch.prototype.tapwipe = function(ipp) {
	for (var sig in this.taps) {
		delete this.taps[sig][ipp];
	}
}

// see if we want to try this switch or not, and maybe prune the bucket
Switch.prototype.bucket_want = function(ipp) {
	var pos = new Hash(ipp).distanceTo(this.selfhash);
	console.log(["\tBUCKET WANT[", pos, ipp, "]"].join(""));
	if (pos < 0 || pos > 159) {
	    return undefined; // do not want
	}
	return 1; // for now we're always taking everyone, in future need to be more selective when the bucket is "full"!
}

/**
 * Hash objects represent a message digest of string content,
 * with methods useful to DHT calculations.
 * @constructor
 */
function Hash(value) {
    if (value != undefined) {
        var hashAlgorithm = crypto.createHash("SHA1");
        hashAlgorithm.update(value);
        this.digest = new Buffer(hashAlgorithm.digest("base64"), "base64");
    }
}

exports.Hash = Hash

Hash.prototype = {
    
    /**
     * Get the hash as geometrically "far" as possible from this one.
     * That would be the logical inverse, every bit flipped.
     */
    far: function() {
        var result = new Hash();
        result.digest = new Buffer(this.digest.length);
        for (var i = 0; i < this.digest.length; i++) {
            result.digest[i] = this.digest[i] ^= 0xff;
        }
        return result;
    },
    
    /**
     * Logical bitwise 'or' this hash with another.
     */
    or: function(h) {
        if (isString(h)) { h = new Hash(h); }
        
        var result = new Hash();
        result.digest = new Buffer(this.digest.length);
        for (var i = 0; i < this.digest.length; i++) {
            result.digest[i] = this.digest[i] ^ h.digest[i];
        }
        return result;
    },
    
    /**
     * Comparator for hash objects.
     */
    cmp: function(h) {
        if (isString(h)) { h = new Hash(h); }
        
        for (var i = 0; i < this.digest.length; i++) {
            var d = this.digest[i] - h.digest[i];
            if (d != 0) {
                return d;
            }
        }
        return 0;
    },
    
    /**
     * XOR distance between two sha1 hex hashes, 159 is furthest bit, 0 is closest bit, -1 is same hash
     */
    distanceTo: function(h) {
        if (isString(h)) { h = new Hash(h); }
        
        var result = new Hash();
        result.digest = new Buffer(this.digest.length);
        var sbtab = [-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3];
        var ret = 156;
        for (var i = 0; i < this.digest.length; i++) {
            var diff = this.digest[i] ^ h.digest[i];
            if (diff) {
                return ret + sbtab[diff]; 
            }
            ret -= 4;
        }
        return -1; # samehash ?!
    },
    
    /**
     * Represent the hash as a hexadecimal string.
     */
    toString: function() {
        var result = [];
        for (var i = this.digest.length - 1; i >= 0; i--) {
            result[i] = byte2hex(this.digest[i]);
        }
        return result.join("");
    }
    
    /**
     * Test if two hashes are equal.
     */
    equals: function(h) {
        var hstr = isString(h) ? h : h.digest;
        return toString() == hstr;
    }
}

var _switch = new Switch();
_switch.start()

