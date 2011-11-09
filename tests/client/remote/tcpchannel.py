from channel import Channel, EncryptionType

class TCPChannel(Channel):
    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        super(TCPChannel, self).__init__(name, remoteHost, remotePort, **kwargs)

        self.reliable = True
        self.ordered = True

    def connectTCP(self, remoteHost, remotePort, key=None):
        logger.debug("Connecting using raw TCP.")

        if key is not None:
            self.encryption = EncryptionType.AES
            self.key = key

    def _send(self, data):
        logger.debug("Sending data.")

        if self.key is not None:
            logger.debug("Encrypting data.")
        pass

