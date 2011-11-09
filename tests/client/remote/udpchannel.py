from channel import Channel, EncryptionType

class UDPChannel(Channel):
    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        super(UDPChannel, self).__init__(name, remoteHost, remotePort, **kwargs)

        self.reliable =False
        self.ordered = False

    def connect(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        logger.debug("Connecting using UDP.")

        # Do UDP Stuff.

        super(UDPChannel, self).connect(remoteHost, remotePort, encryption, key)

    def _send(self, data):
        logger.debug("Sending data over UDP.")


