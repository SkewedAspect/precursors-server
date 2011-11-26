import logging

from channel import SelectChannel


class UDPChannel(SelectChannel):
    logger = logging.getLogger("remote.selectChannel.udp.UDPChannel")

    reliable = False
    ordered = False

    def __init__(self, *args, **kwargs):
        super(UDPChannel, self).__init__(*args, **kwargs)

        self.protocolName = "UDP"

    @classmethod
    def supportsArgs(cls, **kwargs):
        return kwargs['reliable'] == False and kwargs['ordered'] == False

    def connect(self, remoteHost, remotePort, **kwargs):
        self.logger.debug("Connecting using UDP.")

        # Connect.
        self.socket.connect((remoteHost, remotePort))

        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        super(UDPChannel, self).connect(remoteHost, remotePort, **kwargs)

    def _readStream(self, requestedBytes=-1, **kwargs):
        assert(len(kwargs) == 0)
        self.socket.recvfrom(requestedBytes)

    def _writeStream(self, message, **kwargs):
        remoteAddress = kwargs.pop('address', self.defaultTargetAddress)
        assert(len(kwargs) == 0)
