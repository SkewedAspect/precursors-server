from channel import Channel, EncryptionType
import socket


class TCPChannel(Channel):
    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        super(TCPChannel, self).__init__(name, remoteHost, remotePort, **kwargs)

        logger = logging.getLogger("channels.TCPChannel")

        self.reliable = True
        self.ordered = True

        # Setup our socket
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    def connect(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        logger.debug("Connecting using TCP.")

        # Do TCP Stuff.

        super(TCPChannel, self).connect(remoteHost, remotePort, encryption, key)

    def _send(self, data):
        logger.debug("Sending data over TCP.")


class SSLChannel(TCPChannel):
    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        super(SSLChannel, self).__init__(name, remoteHost, remotePort, **kwargs)

        logger = logging.getLogger("channels.SSLChannel")

        self.reliable = True
        self.ordered = True

        # Setup our socket
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.ssl_socket = socket.ssl(self.socket)

    def connect(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        logger.debug("Connecting using SSL.")

        # Do SSL Stuff.

        super(SSLChannel, self).connect(remoteHost, remotePort, encryption, key)

    def _send(self, data):
        logger.debug("Sending data over SSL.")

