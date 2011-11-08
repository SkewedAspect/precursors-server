import logging

import channel


logger = logging.getLogger("channels.client")


class Client(object):
    def __init__(self, remoteHost=None, remotePort=2695):
        self.remoteHost = remoteHost

        # Channel vars
        self.channels = dict()
        self.controlChannel = None

        if remoteHost is not None:
            if not self.connect(remoteHost=remoteHost, remotePort=remotePort):
                raise RuntimeError("Error auto-connecting controlChannel channel!")

    def connect(self, remoteHost, remotePort):
        """Connects to the server, and creates our SSL controlChannel channel.

        """
        try:
            controlChannel = self.createChannel('Control', reliable=True, ordered=True,
                    encryption=channel.EncryptionType.SSL)

        except Exception, ex:
            logger.exception("Error connecting controlChannel channel!", ex)
            return False

        # We've successfully connected
        self.controlChannel = controlChannel

        return True

    def sendControlRequest(self, request):
        """Sends the request over the controlChannel channel.

        """
        self.controlChannel.sendRequest(request)

    def sendControlEvent(self, event):
        """Sends the event over the controlChannel channel.

        """
        self.controlChannel.sendEvent(event)

    def removeChannel(self, name):
        try:
            del self.channels[name]

        except KeyError:
            pass

    def createChannel(self, name, reliable=False, ordered=False, encryption=channel.EncryptionType.NONE,
            remotePort=None):
        """Creates a channel from the given parameters, and stores it with the given name.

        """
        #TODO: Negotiate with the server first to get the port and cookie we should use, and then create the channel!
        self.channels[name] = channel.Channel('Control', self.remoteHost, remotePort, True, channel.EncryptionType.SSL)
        return self.channels[name]
