from abc import ABCMeta, abstractmethod
import logging
import socket

from remote.channel import Channel


logger = logging.getLogger("remote.selectChannel.channel")


class SelectChannel(Channel):
    """A Channel implementation using select to perform asynchronous communication.

    """
    __metaclass__ = ABCMeta

    def __init__(self, *args, **kwargs):
        assert(hasattr(self, 'protocolName'))

        # Ensure this is the right type of socket.
        assert(self.supportsArgs(**kwargs))

        # If an existing socket was specified, use it.
        self.socket = kwargs.get('socket', None)

        if self.socket is None:
            # Set up a new socket
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(0.0)

        super(SelectChannel, self).__init__(*args, **kwargs)

    @abstractmethod
    def connect(self, remoteHost, remotePort, **kwargs):
        """Connect to the remote host, possibly using encryption.

        remoteHost: The remote host to connect to.
        remotePort: The port on the remote host to connect to.

        Additional keyword arguments are passed on to Channel.connect; see its documentation for more options.

        """
        self.logger.debug("Successfully connected to %s:%d using %s.", remoteHost, remotePort, self.protocolName)

        # Now call super to set variables and enable encryption if requested.
        super(SelectChannel, self).connect(remoteHost, remotePort, **kwargs)

    def get_socket(self):
        return self._socket

    def set_socket(self, sock):
        self._socket = sock
        self.fileno = sock.fileno

    socket = property(get_socket, set_socket, doc="The underlying socket object for this Channel.")
