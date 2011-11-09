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

    def connect(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        """Connect to the remote host, possibly using encryption.

        remoteHost - The remote host to connect to.
        remotePOrt - The port on the remote host to connect to.
        encryption - What type of encryption to use on this channel.
        key        - A tuple, consisting of a 128bit key, and a 128bit initialization vector.

        """

        # Successfully connected. Now set variables:
        self.key = key
        self.encryption = encryption
        self.remoteHost = remoteHost
        self.remotePort = remotePort

        # Set our send function
        if self.key is not None:
            logger.debug("Encrypting data.")
            self._sendFunc = lambda data: self._send(encrypt(data, *self.key))
        else:
            self._sendFunc = self._send

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

