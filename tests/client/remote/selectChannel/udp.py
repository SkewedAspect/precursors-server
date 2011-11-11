import logging
import socket

from channel import SelectChannel


class UDPChannel(SelectChannel):
    logger = logging.getLogger("remote.selectChannel.udp.UDPChannel")

    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        super(UDPChannel, self).__init__(name, remoteHost, remotePort, **kwargs)

        self.reliable = False
        self.ordered = False

        # Ensure this is the right type of socket.
        assert(kwargs['reliable'] == self.reliable)
        assert(kwargs['ordered'] == self.ordered)

    @classmethod
    def supportsArgs(cls, **kwargs):
        return not (kwargs['reliable'] or kwargs['ordered'])

    def connect(self, remoteHost, remotePort, **kwargs):
        self.logger.debug("Connecting using UDP.")

        # Setup our socket
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        super(UDPChannel, self).connect(remoteHost, remotePort, **kwargs)

    def _send(self, data):
        self.logger.debug("Sending data over UDP.")

