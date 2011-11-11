import logging
import socket

from channel import SelectChannel


class TCPChannel(SelectChannel):
    logger = logging.getLogger("remote.selectChannel.tcp.TCPChannel")

    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        super(TCPChannel, self).__init__(name, remoteHost, remotePort, **kwargs)

        self.reliable = True
        self.ordered = True

    @classmethod
    def supportsArgs(cls, **kwargs):
        return kwargs['reliable'] or kwargs['ordered']

    def connect(self, remoteHost, remotePort, **kwargs):
        self.logger.debug("Connecting using TCP.")

        # Setup our socket
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        super(TCPChannel, self).connect(remoteHost, remotePort, **kwargs)

    def _send(self, data):
        self.logger.debug("Sending data over TCP.")


class SSLChannel(TCPChannel):
    logger = logging.getLogger("remote.selectChannel.tcp.SSLChannel")

    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        super(SSLChannel, self).__init__(name, remoteHost, remotePort, **kwargs)

        self.reliable = True
        self.ordered = True

        # Setup our socket
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.ssl_socket = socket.ssl(self.socket)

    def connect(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        self.logger.debug("Connecting using SSL.")

        # Do SSL Stuff.

        super(SSLChannel, self).connect(remoteHost, remotePort, encryption, key)

    def _send(self, data):
        self.logger.debug("Sending data over SSL.")

