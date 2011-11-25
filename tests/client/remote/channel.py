from abc import ABCMeta, abstractmethod
import logging

import dispatch


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
        self.reliable = None
        self.ordered = None

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

    @classmethod
    def tryCreate(cls, **kwargs):
        if cls.supportsArgs(**kwargs):
            return cls(**kwargs)

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
    def send(self, data):
        """Sends data over the channel.

        """
