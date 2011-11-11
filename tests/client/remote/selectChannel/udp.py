import logging
import socket

from channel import SelectChannel


class UDPChannel(SelectChannel):
    logger = logging.getLogger("remote.selectChannel.udp.UDPChannel")

    reliable = False
    ordered = False

    def __init__(self, *args, **kwargs):
        super(UDPChannel, self).__init__(*args, **kwargs)

        # Set up our socket
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.socket.settimeout(0.0)

        self.protocolName = "UDP"

    @classmethod
    def supportsArgs(cls, **kwargs):
        return not (kwargs['TLS'] or kwargs['reliable'] or kwargs['ordered'])

    def connect(self, remoteHost, remotePort, **kwargs):
        self.logger.debug("Connecting using UDP.")

        # Connect.
        self.socket.connect((remoteHost, remotePort))

        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        super(UDPChannel, self).connect(remoteHost, remotePort, **kwargs)
