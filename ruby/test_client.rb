require 'rubygems'
require 'eventmachine'
require 'json'

module EchoServer
  def post_init
    puts "sending our hello"
    send_datagram({'+end'=>'0eb2ad19a7b508cc09b2d52b4a506845db39fae2'}.to_json, "telehash.org", 42424)
  end
  
  def receive_data(data)
    # parse message we received
    telex=JSON.parse(data)
    puts telex.inspect
  end
  
end

EventMachine::run do
  EventMachine::open_datagram_socket "0.0.0.0", 0, EchoServer
end
