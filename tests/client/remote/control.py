import logging

import channel
import selectChannel


logger = logging.getLogger("remote.control")


class Control(object):
    def __init__(self, remoteAddr=(None, 2695)):
        self.remoteAddr = remoteAddr

        # Channel vars
        self.channels = dict()
        self.controlChannel = None

        if remoteAddr is not None:
            if not self.connect(remoteAddr=remoteAddr):
                raise RuntimeError("Error auto-connecting controlChannel channel!")

    def connect(self, remoteAddr):
        """Connects to the server, and creates our SSL controlChannel channel.

        """
        try:
            controlChannel = self.createChannel('Control', remoteAddr, reliable=True, ordered=True)

        except:
            logger.exception("Error connecting controlChannel channel!")
            return False

        # We've successfully connected
        self.controlChannel = controlChannel

        return True

    def removeChannel(self, name):
        try:
            del self.channels[name]

        except KeyError:
            pass

    def createChannel(self, name, remoteAddr=(None, 2695), reliable=False, ordered=False, cryptor=None):
        """Creates a channel from the given parameters, and stores it with the given name.

        """
        assert name not in self.channels

        #TODO: Negotiate with the server first to get the port and cookie we should use, and then create the channel!

        #TODO: Dummy Channel idea:
        # We have a class that just queues requests. Control then gets the result of the auto-negotiation with the
        # server, and creates the _actual_ channel, replacing its internal reference, as well as notifying the dummy
        # channel. The dummy channel then passes its queue to the channel (calling send in a loop, basically) and
        # deletes its queue. It then forever acts as a passthrough to the channel, as long as its reference lasts.

        self.channels[name] = channel.createChannel(selectChannel.channelTypes,
                name, remoteAddr,
                reliable=reliable, ordered=ordered
                )

        return self.channels[name]
