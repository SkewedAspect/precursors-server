from abc import ABCMeta, abstractmethod
import logging
import socket

from remote.netstring import QueuedNetstringChannel


logger = logging.getLogger("remote.selectChannel.channel")


class SelectChannel(QueuedNetstringChannel):
    """A Channel implementation using select to perform asynchronous communication.

    """
    __metaclass__ = ABCMeta

    def __init__(self, *args, **kwargs):
        assert(hasattr(self, 'protocolName'))

        # Ensure this is the right type of socket.
        assert(self.supportsArgs(**kwargs))

        # If an existing socket was specified, use it.
        if 'socket' in kwargs:
            self.socket = kwargs['socket']
        else:
            # Set up a new socket
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(0.0)

        super(SelectChannel, self).__init__(*args, **kwargs)

    @abstractmethod
    def connect(self, remoteAddr, **kwargs):
        """Connect to the remote host, possibly using encryption.

        remoteAddr: The address of the remote host to connect to.

        Additional keyword arguments are passed on to Channel.connect; see its documentation for more options.

        """
        self.logger.debug("Successfully connected to %s:%d using %s.", remoteAddr[0], remoteAddr[1], self.protocolName)

        # Now call super to set variables and enable encryption if requested.
        return super(SelectChannel, self).connect(remoteAddr, **kwargs)

    def get_socket(self):
        return self._socket

    def set_socket(self, sock):
        self._socket = sock
        if self.socket is not None:
            self.fileno = sock.fileno
        else:
            self.fileno = None

    def del_socket(self):
        self._socket = None
        self.fileno = None

    socket = property(get_socket, set_socket, del_socket, doc="The underlying socket object for this Channel.")
