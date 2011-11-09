import logging

# Import Dispatcher
try:
    import dispatch
except:
    try:
        import django.dispatch
        globals()['dispatch'] = django.dispatch
    except:
        import channels.dispatch
        globals()['dispatch'] = channels.dispatch


logger = logging.getLogger("channels.channel")


class EncryptionType(object):
    NONE = 0
    SSL = 1
    AES = 2


class Channel(object):
    incomingPacket = dispatch.Signal(["remoteHost", "remotePort", "message"])

    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        self.name = name

        self.remoteHost = None
        self.remotePort = None
        self.key = None
        self.reliable = None
        self.ordered = None
        self.encryption = None

        # AES Keys
        self.encryptionKey = None
        self.oldEncryptionKey = None

        # Dictionary of outstanding requests
        self.requests = dict()

        if remoteHost is not None and remotePort is not None:
            if not self.connect(remoteHost=remoteHost, remotePort=remotePort, **kwargs):
                raise RuntimeError("Error auto-connecting!")

        elif remoteHost is not None or remotePort is not None:
            raise ValueError("Both remoteHost and remotePort must be specified in order to connect!")

    def connect(self, remoteHost, remotePort, reliable=False, ordered=True, encryption=EncryptionType.NONE, key=None):
        succeeded = False

        # Call the appropriate connect handler.
        handler = {
                (True, True): self._connectReliableOrdered,
                (True, False): self._connectReliableUnordered,
                (False, True): self._connectUnreliableOrdered,
                (False, False): self._connectUnreliableUnordered,
                }[reliable, ordered]
        succeeded = handler(remoteHost, remotePort, encryption, key)

        if succeeded:
            # Successfully connected. Now set variables:
            self.encryption = encryption
            self.reliable = reliable
            self.ordered = ordered
            self.key = key
            self.remoteHost = remoteHost
            self.remotePort = remotePort

            # Set our send function
            if self.key is not None:
                logger.debug("Encrypting data.")
                self._sendFunc = lambda data: self._send(encrypt(data, *self.key))
            else:
                self._sendFunc = self._send

            return True

        return False

    def _connectReliableOrdered(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        # We're doing one of the TCP connections.
        if encryption == EncryptionType.SSL:
            return self.connectSSL(remoteHost, remotePort)

        else:
            return self.connectTCP(remoteHost, remotePort, key)

    def _connectUnreliableUnordered(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        # We're doing UDP.
        return self.connectUDP(remoteHost, remotePort, key)

    def _connectReliableUnordered(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        logger.warn("Reliable/unordered channels not yet implemented; using reliable/ordered instead.")
        return self.connectReliableOrdered(remoteHost, remotePort, encryption, key)

    def _connectUnreliableOrdered(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        logger.warn("Unreliable/ordered channels not yet implemented; using reliable/ordered instead.")
        return self.connectReliableOrdered(remoteHost, remotePort, encryption, key)

    def connectSSL(self, remoteHost, remotePort):
        logger.warn("Unimplemented in base class!")
        #logger.debug("Connecting using TCP with SSL.")
        pass

    def connectTCP(self, remoteHost, remotePort, key=None):
        logger.warn("Unimplemented in base class!")
        #logger.debug("Connecting using raw TCP.")
        pass

    def connectUDP(self, remoteHost, remotePort, key=None):
        logger.warn("Unimplemented in base class!")
        #logger.debug("Connecting using raw UDP.")
        pass

    def sendRequest(self, request):
        """Send an request over the channel.

        """
        self._sendFunc(request)

    def sendEvent(self, event):
        """Send an event over the channel.

        """
        self._sendFunc(event)

    def receiveRequest(self, request):
        """Receive a request over the channel.

        """
        pass

    def receiveEvent(self, event):
        """Receive an event over the channel.

        """
        pass

    def _send(self, data):
        """Sends data over the channel, encrypting if required.

        """
        logger.warn("Unimplemented in base class!")

