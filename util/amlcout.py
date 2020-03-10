# Send bytes 0-0xff to Prime emulator to verify transparent connection
# NOTE: Enable the printf at storech: in devamlc to perform the test

import socket

client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
client_socket.connect(('localhost', 9000))
data = ''
for i in xrange(256):
  data += chr(i)
data += chr(255)
client_socket.send(data)
client_socket.close()
