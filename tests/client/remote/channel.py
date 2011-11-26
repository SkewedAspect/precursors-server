from abc import ABCMeta, abstractmethod
import logging

import dispatch
from stream import IOQueuedStream


logger = logging.getLogger("remote.channel")


defaultChannelKwargs = {
        'reliable': False,
        'ordered': False,
        }


def createChannel(channelTypes, **kwargs):
    for key, val in defaultChannelKwargs.iteritems():
        kwargs.setdefault(key, val)

    for chanType in channelTypes:
        chan = chanType.tryCreate(**kwargs)
        if chan is not None and chan is not NotImplemented:
            return chan


class ChannelStream(object):
    def __init__(self, channel):
        self.channel = channel

    def read(self, requestedBytes=-1, **kwargs):
        return self.channel._readStream(requestedBytes, **kwargs)

    def write(self, message, **kwargs):
        return self.channel._writeStream(message, **kwargs)


class Channel(object):
    __metaclass__ = ABCMeta

    disconnected = dispatch.Signal(["remoteAddress"])
    incomingPacket = dispatch.Signal(["remoteAddress", "message", "metadata"])

    def __init__(self, name, remoteHost=None, remotePort=None, **kwargs):
        """Create a Channel, optionally connecting to the given remote host and
        port immediately.

        Additional keyword arguments are passed on to connect; see its
        documentation for more options.

        """
        self.name = name

        self.remoteHost = None
        self.remotePort = None
        self.reliable = None
        self.ordered = None

        # Build the StreamWrapper stack.
        self.buildStreamWrapperStack()

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

    def buildStreamWrapperStack(self):
        # The base of the StreamWrapper stack.
        self.target = ChannelStream(self)

    @classmethod
    def tryCreate(cls, **kwargs):
        if cls.supportsArgs(**kwargs):
            return cls(**kwargs)

    def send(self, data):
        """Sends data over the channel, encrypting if required.

        """
        self.logger.debug("Sending data over %s.", self.protocolName)
        self.target.write(data)

    def receive(self):
        """Checks for incoming data, and raises an incomingPacket signal if a message is received.

        If this isn't an asynchronous Channel of some sort, this method will block until data is received.

        """
        try:
            incoming, metadata = self.target.read()
        except TypeError:
            # Ignore TypeError; if we got a TypeError, that probably means we got None back, which just means there's
            # nothing to send yet.
            pass
        else:
            self.logger.debug("Received message from %s.", self.protocolName)

            kwargs = {
                    'sender': self,
                    'message': incoming,
                    }

            if 'remoteAddress' in metadata:
                kwargs['remoteAddress'] = metadata.pop('remoteAddress')

            kwargs['metadata'] = metadata

            self.incomingPacket.send_robust(**kwargs)

    ## Channel Implementation Methods ##
    # Implement all of these in each subclass.
    @classmethod
    @abstractmethod
    def supportsArgs(cls, **kwargs):
        """Test whether this Channel class supports the given keyword arguments.

        """

    @abstractmethod
    def connect(self, remoteHost, remotePort):
        """Connect to the remote host, possibly using encryption.

        """

    @abstractmethod
    def _readStream(self, requestedBytes=-1, **kwargs):
        """Low-level read - this is called by the innermost stream in the self.target hierarchy.

        @param requestedBytes the number of bytes to attempt to read from the network

        @return the bytes read from the network

        Implement this by reading from the underlying socket or network stream.

        """

    @abstractmethod
    def _writeStream(self, message, **kwargs):
        """Low-level write - this is called by the innermost stream in the self.target hierarchy.

        @param message the bytes to write to the network

        @return the number of bytes successfully written

        Implement this by writing to the underlying socket or network stream.

        """


class QueuedChannel(Channel):
    queuedStreamWrapperType = IOQueuedStream

    def buildStreamWrapperStack(self):
        super(QueuedChannel, self).buildStreamWrapperStack()

        # Add our queued StreamWrapper to the stack, and keep track of it separately so we can call handle*.
        self.queuedStreamWrapper = self.queuedStreamWrapperType(self.target)
        self.target = self.queuedStreamWrapper

    def handleRead(self):
        # Read all currently-available data into the incoming queue.
        self.queuedStreamWrapper.handleRead()

        # Now, see if we got a full message in the queue, and raise incomingPacket if we did.
        self.receive()

    def handleWrite(self):
        self.queuedStreamWrapper.handleWrite()
