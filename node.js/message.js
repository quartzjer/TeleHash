var dgram = require('dgram');
var message = new Buffer(JSON.stringify({"+end":"38666817e1b38470644e004b9356c1622368fa57"}));
var socket = dgram.createSocket("udp4");
var ezcrypto = require('../ezcrypto-js/ezcrypto.js').ezcrypto;
var Mu = require('../Mu/lib/mu');
var fs = require('fs');
var sqlite = require('../node-sqlite/sqlite');
var db = new sqlite.Database();
var http = require('http');
var parser = require('url').parse;
Mu.templateRoot = './templates';
var index;
var logic = {
    servername: "Yahoo"
};


db.open("tweets.db", function(error){
  if (error) {
      console.log("Error opening database");
      throw error;
  }
  db.getTweets = function(res){ 
    db.execute("select name,message,timestamp from tweets join names on tweets.key = names.key", function(error, rows){
      if (error && error.message.search("no such table") != -1){
        console.log(error);//throw error;
        db.execute("CREATE TABLE tweets (key, message, timestamp)",function(){});
        db.getTweets();
      }
      //res.end(JSON.stringify(rows));
      http.renderTweets(res, rows);
      console.log(rows);
    });
  };
  //db.getTweets();
  
});

Mu.render('index.html', logic, {}, function(err, output){
  if (err){
    throw err;
  }
  
  var buffer = '';
  
  output.addListener('data', function (c) {buffer += c; })
        .addListener('end', function () { index = buffer; });
});


http.renderTweets = function(res, rows){
  var tweetLogic = {
    tweets: rows
  };
 
  Mu.render('tweets.html', tweetLogic, {}, function(err, output){
    if (err){
      throw err;
    }
    
    var buffer = '';
    
    output.addListener('data', function (c) {buffer += c; })
          .addListener('end', function () { res.end(buffer) });
  });
};

http.createServer(function(req,res){
  res.writeHead(200, {'Content-Type': 'text/html'});
  var output = ("An error occured");
  var reqdict = parser(req.url, true);
  var pathname = reqdict.pathname;
  console.log(reqdict);
  if (pathname == "/"){
    output = index;
  }
  if (pathname == "/message") {
    var data = reqdict.query.data
  };
  if (pathname == "/getmessage"){};
  if (pathname == "/tweets"){
    db.getTweets(res);
    return;
  }
    
  res.end(output);
}).listen(8080, "127.0.0.01");

socket.sendData = function(message){ console.log(message.toString()); this.send(message, 0, message.length, 42424, "telehash.org"); }
//socket.formatter = function(message){ new Buffer(JSON.stringify({".tap":[{"has":["+key"]}],"_line":ring, "_to":"208.68.163.247:42424"}));
socket.on("message", function(data, rinfo){
  console.log(data.toString());
  telex = JSON.parse(data.toString());
  //console.log("TELEX: " + JSON.stringify(telex));
  if (telex["_ring"]){
    ring = telex["_ring"];
    console.log("LINE: " + ring);
    this.line = ring;
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
      var timestamp = new Date().toString();
      db.insertTweets = function(){
        db.execute("INSERT INTO tweets (key, message, timestamp) VALUES (?,?,?)", [key, message, timestamp], function(error, rows){
          if (error){
            db.execute("CREATE TABLE tweets (key, message, timestamp)",function(){});
            db.insertTweets();
          }
        });
      };
      db.insertTweets();
    } else {
      console.log("Key doesn't validate");
    }
    
  }
});

socket.on("listening", function(){
  console.log("Now listening");
  setTimeout(function(){ socket.send(message, 0, message.length, 42424, "telehash.org"); socket.ping() }, 1000);

});

socket.bind(12345);
var pingmsg = new Buffer(JSON.stringify({"_line":socket.line, "_to":"208.68.163.247:42424"}));
socket.ping = function(){ setTimeout(function(){ socket.sendData(pingmsg); socket.ping(); },30000) };
