import logging

from remote.channel import withDefChannelKwargs

from channel import SelectChannel


class UDPChannel(SelectChannel):
    logger = logging.getLogger("remote.selectChannel.udp.UDPChannel")

    reliable = False
    ordered = False

    @withDefChannelKwargs
    def __init__(self, *args, **kwargs):
        self.protocolName = "UDP"

        self.defaultTargetAddress = None

        super(UDPChannel, self).__init__(*args, **kwargs)

    @classmethod
    @withDefChannelKwargs
    def supportsArgs(cls, **kwargs):
        return kwargs['reliable'] == False and kwargs['ordered'] == False

    def connect(self, remoteAddr, **kwargs):
        self.logger.debug("Connecting using UDP.")

        self.defaultTargetAddress = remoteAddr

        # Bind to the local address and port.
        self.socket.bind()

        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        return super(UDPChannel, self).connect(remoteAddr, **kwargs)

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
