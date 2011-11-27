import logging

from channel import SelectChannel


class UDPChannel(SelectChannel):
    logger = logging.getLogger("remote.selectChannel.udp.UDPChannel")

    reliable = False
    ordered = False

    def __init__(self, *args, **kwargs):
        super(UDPChannel, self).__init__(*args, **kwargs)

        self.protocolName = "UDP"

        self.defaultTargetAddress = None

    @classmethod
    def supportsArgs(cls, **kwargs):
        return kwargs['reliable'] == False and kwargs['ordered'] == False

    def connect(self, remoteHost, remotePort, **kwargs):
        self.logger.debug("Connecting using UDP.")

        self.defaultTargetAddress = (remoteHost, remotePort)

        # Bind to the local address and port.
        self.socket.bind()

        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        super(UDPChannel, self).connect(remoteHost, remotePort, **kwargs)

    def _readStream(self, requestedBytes=-1, **kwargs):
        # We don't support any keyword arguments.
        assert(len(kwargs) == 0)

        (data, address) = self.socket.recvfrom(requestedBytes)

        metadata = {
                'remoteAddress': address,
                }
        return data, metadata

    def _writeStream(self, message, **kwargs):
        remoteAddress = kwargs.pop('address', self.defaultTargetAddress)

        # We don't support any other keyword arguments.
        assert(len(kwargs) == 0)

        self.socket.sendto(message, remoteAddress)
