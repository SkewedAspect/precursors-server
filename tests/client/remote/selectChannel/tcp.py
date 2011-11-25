import logging
import socket

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
        return kwargs['reliable'] == True or kwargs['ordered'] == True

    def connect(self, remoteHost, remotePort, **kwargs):
        self.logger.debug("Connecting using %s.", self.protocolName)

        # Connect.
        self.socket.connect((remoteHost, remotePort))

        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        super(TCPChannel, self).connect(remoteHost, remotePort, **kwargs)
