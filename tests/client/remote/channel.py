from abc import ABCMeta, abstractmethod
import logging

# Import Dispatcher
try:
    #FIXME: This will __always__ import the 'dispatch' package in 'remote',
    # since it's in the same package as this module; we will never import one
    # from the system.
    import dispatch
except:
    try:
        import django.dispatch
        globals()['dispatch'] = django.dispatch
    except:
        import remote.dispatch
        globals()['dispatch'] = remote.dispatch


logger = logging.getLogger("remote.channel")


class EncryptionType(object):
    NONE = 0
    SSL = 1
    AES = 2


defaultChannelKwargs = {
        'reliable': False,
        'ordered': False,
        'encryption': EncryptionType.NONE,
        }


def createChannel(channelTypes, **kwargs):
    for key, val in defaultChannelKwargs.iteritems():
        kwargs.setdefault(key, val)

    for chanType in channelTypes:
        chan = chanType.tryCreate(**kwargs)
        if chan is not None and chan is not NotImplemented:
            return chan


class Channel(object):
    __metaclass__ = ABCMeta

    incomingPacket = dispatch.Signal(["remoteHost", "remotePort", "message"])

    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        """Create a Channel, optionally connecting to the given remote host and
        port immediately.

        Additional keyword arguments are passed on to connect; see its
        documentation for more options.

        """
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
            if not self.connect(
                    remoteHost=remoteHost,
                    remotePort=remotePort,
                    **kwargs
                    ):
                raise RuntimeError("Error auto-connecting!")

        elif remoteHost is not None or remotePort is not None:
            raise ValueError("Both remoteHost and remotePort must be specified in order to connect!")

    @abstractmethod
    @classmethod
    def supportsArgs(cls, **kwargs):
        return NotImplemented

    @classmethod
    def tryCreate(cls, **kwargs):
        if cls.supportsArgs(**kwargs):
            return cls(**kwargs)

    @abstractmethod
    def connect(self, remoteHost, remotePort, encryption=EncryptionType.NONE, key=None):
        #TODO: Remove 'encryption' and 'key' in favor of cipher objects, which
        # conform to the stream protocol (providing read() and write()) and
        # provide setSocket(sock) to set the underlying socket object to work
        # with. This allows us to pass in whatever data is needed by the
        # encryption algorithm, without Channel caring what data is needed.
        """Connect to the remote host, possibly using encryption.

        remoteHost: The remote host to connect to.
        remotePort: The port on the remote host to connect to.
        encryption: What type of encryption to use on this channel.
        key: A tuple containing a 128-bit key and a 128-bit initialization vector.

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

class NotImplementedChannel(channel):
    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        super(NotImplementedChannel, self).__init__(name, remoteHost, remotePort, **kwargs)

        logger.warn("Channel type not yet implemented.")
        raise NotImplemented("Channel type not yet implemented.")
