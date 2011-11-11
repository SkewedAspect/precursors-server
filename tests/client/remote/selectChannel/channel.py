import logging

from remote.channel import Channel


logger = logging.getLogger("remote.channel")


class SelectChannel(Channel):
    def connect(self, remoteHost, remotePort, **kwargs):
        """Connect to the remote host, possibly using encryption.

        remoteHost - The remote host to connect to.
        remotePort - The port on the remote host to connect to.

        Additional keyword arguments are passed on to Channel.connect; see its
        documentation for more options.

        """
        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        super(SelectChannel, self).connect(remoteHost, remotePort, **kwargs)

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
