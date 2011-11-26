import logging

from channel import SelectChannel


class TCPChannel(SelectChannel):
    logger = logging.getLogger("remote.selectChannel.tcp.TCPChannel")

    reliable = True
    ordered = True

    def __init__(self, *args, **kwargs):
        super(TCPChannel, self).__init__(*args, **kwargs)

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

    def _readStream(self, requestedBytes=-1, **kwargs):
        assert(len(kwargs) == 0)
        return self.socket.recv(requestedBytes)

    def _writeStream(self, message, **kwargs):
        assert(len(kwargs) == 0)
        return self.socket.send(message)
