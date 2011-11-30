import logging

from channel import SelectChannel


class TCPChannel(SelectChannel):
    logger = logging.getLogger("remote.selectChannel.tcp.TCPChannel")

    reliable = True
    ordered = True

    def __init__(self, *args, **kwargs):
        self.protocolName = "TCP"

        self.metadata = {}

        super(TCPChannel, self).__init__(*args, **kwargs)

    @classmethod
    def supportsArgs(cls, **kwargs):
        return kwargs['reliable'] == True or kwargs['ordered'] == True

    def connect(self, remoteAddr, **kwargs):
        self.logger.debug("Connecting using %s.", self.protocolName)

        # Connect.
        self.socket.connect(remoteAddr)

        self.metadata = {
                'remoteAddress': remoteAddr,
                }

        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        return super(TCPChannel, self).connect(remoteAddr, **kwargs)

    def _readStream(self, requestedBytes=-1, **kwargs):
        # We don't support any keyword arguments.
        assert(len(kwargs) == 0)

        return self.socket.recv(requestedBytes), self.metadata

    def _writeStream(self, message, **kwargs):
        # We don't support any keyword arguments.
        assert(len(kwargs) == 0)

        return self.socket.send(message)
