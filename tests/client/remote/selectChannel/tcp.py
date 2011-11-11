import logging
import socket
import ssl

from channel import SelectChannel


class TCPChannel(SelectChannel):
    logger = logging.getLogger("remote.selectChannel.tcp.TCPChannel")

    reliable = True
    ordered = True

    def __init__(self, *args, **kwargs):
        super(TCPChannel, self).__init__(*args, **kwargs)

        # Set up our socket
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.settimeout(0.0)

        self.protocolName = "TCP"

    @classmethod
    def supportsArgs(cls, **kwargs):
        return not kwargs['TLS'] and (kwargs['reliable'] or kwargs['ordered'])

    def connect(self, remoteHost, remotePort, **kwargs):
        self.logger.debug("Connecting using %s.", self.protocolName)

        # Connect.
        self.socket.connect((remoteHost, remotePort))

        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        super(TCPChannel, self).connect(remoteHost, remotePort, **kwargs)


class SSLChannel(TCPChannel):
    logger = logging.getLogger("remote.selectChannel.tcp.SSLChannel")

    def __init__(self, *args, **kwargs):
        super(SSLChannel, self).__init__(*args, **kwargs)

        self.protocolName = "TLS"

        self.originalSocket = self.socket

        # Set up TLS on our socket.
        self.socket = ssl.wrap_socket(
                self.socket,
                ca_certs="/etc/ca_certs_file",
                cert_reqs=ssl.CERT_REQUIRED
                )
        self.target = self.socket

    @classmethod
    def supportsArgs(cls, **kwargs):
        return kwargs['TLS'] and (kwargs['reliable'] or kwargs['ordered'])

    def connect(self, remoteHost, remotePort, **kwargs):
        # Connect.
        super(SSLChannel, self).connect(remoteHost, remotePort, **kwargs)

        # Print out some info about our TLS connection.
        print repr(self.socket.getpeername())
        print self.socket.cipher()
        import pprint
        print pprint.pformat(self.socket.getpeercert())
