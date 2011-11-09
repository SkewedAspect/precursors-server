from channel import Channel, EncryptionType
import socket


class UDPChannel(Channel):
    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        super(UDPChannel, self).__init__(name, remoteHost, remotePort, **kwargs)

        logger = logging.getLogger("channels.SSLChannel")

        self.reliable =False
        self.ordered = False

        # Setup our socket
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

    def connect(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        logger.debug("Connecting using UDP.")

        # Do UDP Stuff.

        super(UDPChannel, self).connect(remoteHost, remotePort, encryption, key)

    def _send(self, data):
        logger.debug("Sending data over UDP.")

