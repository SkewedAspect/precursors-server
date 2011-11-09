from channel import Channel, EncryptionType

class TCPChannel(Channel):
    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        super(TCPChannel, self).__init__(name, remoteHost, remotePort, **kwargs)

        self.reliable = True
        self.ordered = True

    def connect(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        logger.debug("Connecting using TCP.")

        # Do TCP Stuff.

        super(TCPChannel, self).connect(remoteHost, remotePort, encryption, key)

    def _send(self, data):
        logger.debug("Sending data over TCP.")


