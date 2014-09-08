require 'socket'

host, port = ARGV

s = TCPSocket.new(host, port)
while line = s.gets # ソケットから行を読む
  puts line.chop
end
s.close
