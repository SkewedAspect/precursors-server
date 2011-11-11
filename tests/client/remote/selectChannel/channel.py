import logging

from remote.channel import Channel


logger = logging.getLogger("remote.selectChannel.channel")


class SelectChannel(Channel):
    def __init__(self, *args, **kwargs):
        if not hasattr(self, 'target'):
            self.target = None

        assert(hasattr(self, 'protocolName'))

        # Ensure this is the right type of socket.
        assert(kwargs['reliable'] == self.reliable)
        assert(kwargs['ordered'] == self.ordered)

        super(SelectChannel, self).__init__(*args, **kwargs)

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
        self.target = sock
        self.fileno = sock.fileno

    socket = property(get_socket, set_socket, doc="The underlying socket object for this Channel.")

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
        raise NotImplementedError("receiveRequest not yet implemented!")

    def receiveEvent(self, event):
        """Receive an event over the channel.

        """
        raise NotImplementedError("receiveEvent not yet implemented!")

    def _send(self, data):
        """Sends data over the channel, encrypting if required.

        """
        self.logger.debug("Sending data over %s.", self.protocolName)
        self.target.write(data)
