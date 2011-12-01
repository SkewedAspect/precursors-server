from abc import ABCMeta, abstractmethod
from functools import wraps
import logging

import dispatch
from stream import IOQueuedStream


logger = logging.getLogger("remote.channel")


defaultChannelKwargs = {
        'reliable': True,
        'ordered': True,
        }


def withDefChannelKwargs(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        for key, val in defaultChannelKwargs.iteritems():
            kwargs.setdefault(key, val)

        return func(*args, **kwargs)
    return wrapper


def createChannel(channelTypes, *args, **kwargs):
    for chanType in channelTypes:
        chan = chanType.tryCreate(*args, **kwargs)
        if chan is not None and chan is not NotImplemented:
            return chan


class ChannelStream(object):
    def __init__(self, channel):
        self.channel = channel

    def readable(self):
        return self.channel._readable()

    def read(self, requestedBytes=-1, **kwargs):
        return self.channel._readStream(requestedBytes, **kwargs)

    def write(self, message, **kwargs):
        return self.channel._writeStream(message, **kwargs)


class Channel(object):
    __metaclass__ = ABCMeta

    disconnected = dispatch.Signal(["remoteAddress"])
    incomingPacket = dispatch.Signal(["remoteAddress", "message", "metadata"])

    @withDefChannelKwargs
    def __init__(self, name, remoteAddr=None, **kwargs):
        """Create a Channel, optionally connecting to the given remote host and
        port immediately.

        Additional keyword arguments are passed on to connect; see its
        documentation for more options.

        """
        self.name = name

        self.remoteAddr = None
        self.reliable = None
        self.ordered = None

        # Build the StreamWrapper stack.
        self.buildStreamWrapperStack()

        # Dictionary of outstanding requests
        self.requests = dict()

        if remoteAddr is not None:
            if not self.connect(remoteAddr=remoteAddr, **kwargs):
                raise RuntimeError("Error auto-connecting!")

    def buildStreamWrapperStack(self):
        # The base of the StreamWrapper stack.
        self.target = ChannelStream(self)

    @classmethod
    @withDefChannelKwargs
    def tryCreate(cls, *args, **kwargs):
        if cls.supportsArgs(**kwargs):
            return cls(*args, **kwargs)

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
            # Ignore TypeError; if we got this, it probably means we got None back, which just means there's nothing to
            # send yet.
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

            self.incomingPacket.send_robust(self, **kwargs)

    def _readable(self):
        """Check whether this channel's stream is currently readable.

        """
        return True

    ## Channel Implementation Methods ##
    # Implement all of these in each subclass.
    @classmethod
    @abstractmethod
    def supportsArgs(cls, **kwargs):
        """Test whether this Channel class supports the given keyword arguments.

        """

    @abstractmethod
    def connect(self, remoteAddr, **kwargs):
        """Connect to the remote host, possibly using encryption.

        """
        return True

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
