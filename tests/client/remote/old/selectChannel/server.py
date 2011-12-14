import logging
import SocketServer

from communication import SelectCommunicator
from tcp import TCPChannel


class TCPHandler(SocketServer.ThreadingMixIn, SocketServer.BaseRequestHandler):
    """The RequestHandler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.

    """
    logger = logging.getLogger("remote.selectChannel.server.TCPHandler")

    connectionChannelType = TCPChannel

    def handle(self):
        comm = SelectCommunicator()

        # self.request is the TCP socket connected to the client
        self.channel = self.connectionChannelType(name="Client!", socket=self.request)
        self.channel.incomingPacket.connect(self.onPacket, self.channel)
        comm.registerChannel(self.channel)

        # Stop the loop once the channel disconnects.
        self.channel.disconnected.connect(lambda _sender, **_: comm.exitLoop(), self.channel)

        # Loop!
        comm.loop()

    def onPacket(self, sender, **kwargs):
        addr = kwargs['remoteAddress']
        msg = kwargs['message']
        self.logger.debug("Got packet from %s:%d: %r", addr[0], addr[1], msg)
        self.channel.send("Hello, %s:%d! You said %r." % (addr[0], addr[1], msg))


if __name__ == "__main__":
    HOST, PORT = "localhost", 9999

    logging.basicConfig(level=0)

    # Create the server, binding to localhost on port 9999
    server = SocketServer.TCPServer((HOST, PORT), TCPHandler)

    # Activate the server; this will keep running until you
    # interrupt the program with Ctrl-C
    server.serve_forever()
