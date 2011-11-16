#! /usr/bin/env python
import socket, ssl, pprint, select, sys

# This code was blatantly ripped off the internet. It came from:
# http://code.activestate.com/recipes/531824-chat-server-client-using-selectselect/

# Yes, it could use some work.


class SSLClient(object):
    def __init__(self, address, port):
        self.quit = False

        self.address = address
        self.port = port

        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.ssl_sock = ssl.wrap_socket(self.socket,
                           ca_certs="../../priv/precursors.crt",
                           cert_reqs=ssl.CERT_REQUIRED)

    def connect(self):
        self.ssl_sock.connect((self.address, self.port))

    def loop(self):
        while not self.quit:
            try:
                sys.stdout.write("Chat! > ")
                sys.stdout.flush()

                # Wait for input from stdin & socket
                inputready, outputready, exceptrdy = select.select([0, self.ssl_sock], [],[])

                for i in inputready:
                    if i == 0:
                        data = sys.stdin.readline().strip()
                        if data:
                            self.ssl_sock.send(data)
                    elif i == self.ssl_sock:
                        data = self.ssl_sock.recv(1024)
                        if not data:
                            print 'Shutting down.'
                            self.quit = True
                            break
                        else:
                            sys.stdout.write(data + '\n')
                            sys.stdout.flush()

            except KeyboardInterrupt:
                print 'Interrupted.'
                self.ssl_sock.close()
                break

if __name__ == "__main__":
    import sys

    if len(sys.argv)<3:
        sys.exit('Usage: %s host portno' % sys.argv[0])

    client = SSLClient(sys.argv[1], int(sys.argv[2]))
    client.connect()
    client.loop()
