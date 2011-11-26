import logging
import select

from remote.communication import QueuedCommunicator


class Select(QueuedCommunicator):
    logger = logging.getLogger("remote.selectChannel.communication.Select")

    def __init__(self, *args, **kwargs):
        super(Select, self).__init__(*args, **kwargs)

    def checkIO(self, timeout=None):
        """Checks all registered Channels for ones which are ready for I/O, and calls their handlers.

        If no Channels are ready, and timeout is:
        - 0: return immediately
        - None or not specified: block indefinitely until a Channel becomes ready, then return
        - Any positive number: wait `timeout` seconds or until a Channel becomes ready, then return

        """
        if len(self.awaitingRead) == 0 and len(self.awaitingWrite) == 0:
            return

        readReady, writeReady, _ = select.select(
                rlist=self.channels,
                wlist=self.awaitingWrite,
                xlist=[],
                timeout=0
                )

        for chan in writeReady:
            chan.handleWrite()

        for chan in readReady:
            chan.handleRead()


class EPoll(QueuedCommunicator):
    logger = logging.getLogger("remote.selectChannel.communication.EPoll")

    def __init__(self, *args, **kwargs):
        raise NotImplementedError("EPoll support not yet implemented!")
        self.epoll = select.epoll()
        super(EPoll, self).__init__(*args, **kwargs)


class KQueue(QueuedCommunicator):
    logger = logging.getLogger("remote.selectChannel.communication.KQueue")

    def __init__(self, *args, **kwargs):
        raise NotImplementedError("KQueue support not yet implemented!")
        super(KQueue, self).__init__(*args, **kwargs)


class KEvent(QueuedCommunicator):
    logger = logging.getLogger("remote.selectChannel.communication.KEvent")

    def __init__(self, *args, **kwargs):
        raise NotImplementedError("KEvent support not yet implemented!")
        super(KEvent, self).__init__(*args, **kwargs)


class Poll(QueuedCommunicator):
    logger = logging.getLogger("remote.selectChannel.communication.Poll")

    def __init__(self, *args, **kwargs):
        raise NotImplementedError("Poll support not yet implemented!")
        super(Poll, self).__init__(*args, **kwargs)
