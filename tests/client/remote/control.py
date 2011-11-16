import logging

import channel
import selectChannel


logger = logging.getLogger("remote.control")


class Control(object):
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

    def createChannel(self, name, remotePort=None, reliable=False, ordered=False, cryptor=None):
        """Creates a channel from the given parameters, and stores it with the given name.

        """
        assert name not in self.channels

        channelType = channel.createChannel(selectChannel.channelTypes, reliable=reliable, ordered=ordered)

        #TODO: Negotiate with the server first to get the port and cookie we should use, and then create the channel!

        # TODO: Dummy Channel idea:
        # We have a class that just queues requests. Control then gets the result of the
        # auto-negotiation with the server, and creates the _actual_ channel, replacing
        # it's internal reference, as well as notifying the dummy channel. The dummy channel
        # then passes it's queue to the channel (calling send in a loop, basically) and
        # deletes it's queue. It then forever acts as a passthrough to the channel, as long
        # as its reference lasts.

        self.channels[name] = channelType(self.remoteHost, remotePort)

        return self.channels[name]
