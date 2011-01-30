var dgram = require('dgram');
var message = new Buffer(JSON.stringify({"+end":"38666817e1b38470644e004b9356c1622368fa57"}));
var socket = dgram.createSocket("udp4");
var ezcrypto = require('../ezcrypto-js/ezcrypto.js').ezcrypto;



socket.sendData = function(message){ console.log(message.toString()); this.send(message, 0, message.length, 42424, "telehash.org"); }

socket.on("message", function(data, rinfo){
  console.log(data.toString());
  telex = JSON.parse(data.toString());
  //console.log("TELEX: " + JSON.stringify(telex));
  if (telex["_ring"]){
    ring = telex["_ring"];
    console.log("LINE: " + ring);
    var response = new Buffer(JSON.stringify({".tap":[{"has":["+key"]}],"_line":ring, "_to":"208.68.163.247:42424"}));
    this.sendData(response);
  }
  if (telex["+key"]){
    console.log("INCOMING KEY!!!!!!!!");
    var message = telex["+message"];
    var key = telex["+key"];
    //console.log(key);
    var signature = telex["+sig"];
    //var verifier = crypto.createVerify('RSA-SHA1');
    //verifier.update(message);
    var test = ezcrypto.verify(message, signature, key)
    //console.log(message);
    //var bool =verifier.verify(key, signature, signature_format='hex');
    //console.log(bool);
    if (test){
      console.log("Key validates");
    } else {
      console.log("Key doesn't validate");
    }
    
  }
});

socket.on("listening", function(){
  console.log("Now listening");
  socket.send(message, 0, message.length, 42424, "telehash.org");

});

socket.bind(12345);



