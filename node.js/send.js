var dgram = require('dgram');
var crypto = require('crypto');
var fs = require('fs');
var ezcrypto = require('../ezcrypto-js/ezcrypto.js').ezcrypto;
//var process = require('process');

//var keyPem = fs.readFileSync("mykey.pem", 'ascii');
//var pubKey = fs.readFileSync("mycert.pem", 'ascii');
//var cred = crypto.createCredentials({key:keyPem});
//var key = cred.key;
try { 
  data = fs.readFileSync('./keys','ascii')
  keys = JSON.parse(data);
} catch(e) {
  console.log(e);
  keys = ezcrypto.generateKey();
  fs.writeFileSync('./keys',JSON.stringify(keys));
}

var socket = dgram.createSocket("udp4");
//var signer = crypto.createSign('RSA-SHA1');
var msg = process.argv[2];
//signer.update(msg);
//var signature = signer.sign(keyPem, output_format='hex');
var hash = ezcrypto.hash(msg);
var signature = ezcrypto.sign(hash, keys.public, keys.private);
var json = {"+key":keys.public, "_hop":1,"+end":"8bf1cce916417d16b7554135b6b075fb16dd26ce","_to":"208.68.163.247:42424", "+sig":signature, "+message":msg};
var message = new Buffer(JSON.stringify(json));

console.log(message.toString());
socket.on("listening",function(){
  console.log("sending");
  socket.send(message, 0, message.length, 42424, "telehash.org", function(){ setTimeout(process.exit,1000); });
});

socket.bind(34344);





