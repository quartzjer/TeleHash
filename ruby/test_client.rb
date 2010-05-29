require 'rubygems'
require 'eventmachine'

 module EchoServer
   def post_init
     puts "sending our hello"
     send_datagram "{'+end':'0eb2ad19a7b508cc09b2d52b4a506845db39fae2'}", "telehash.org", 42424
   end

   def receive_data data
     puts data
   end

 end

 EventMachine::run {
   EventMachine::open_datagram_socket "0.0.0.0", 0, EchoServer
 }