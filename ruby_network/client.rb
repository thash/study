require 'socket'

server = TCPServer.open(2000)
loop {
  client = server.accept      # クライアントの接続を待つ
  client.puts(Time.now.ctime) # クライアントに時刻を送る
  client.puts("bye :)") # クライアントに時刻を送る
  client.close
}
