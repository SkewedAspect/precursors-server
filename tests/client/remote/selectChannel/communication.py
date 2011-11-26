import logging
import select

from remote.communication import QueuedCommunicator


class SelectCommunicator(QueuedCommunicator):
    logger = logging.getLogger("remote.selectChannel.communication.SelectCommunicator")

    def __init__(self, *args, **kwargs):
        super(SelectCommunicator, self).__init__(*args, **kwargs)

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


class EPollCommunicator(QueuedCommunicator):
    logger = logging.getLogger("remote.selectChannel.communication.EPollCommunicator")

    def __init__(self, *args, **kwargs):
        raise NotImplementedError("EPoll support not yet implemented!")
        self.epoll = select.epoll()
        super(EPollCommunicator, self).__init__(*args, **kwargs)


class KQueueCommunicator(QueuedCommunicator):
    logger = logging.getLogger("remote.selectChannel.communication.KQueueCommunicator")

    def __init__(self, *args, **kwargs):
        raise NotImplementedError("KQueue support not yet implemented!")
        super(KQueueCommunicator, self).__init__(*args, **kwargs)


class KEventCommunicator(QueuedCommunicator):
    logger = logging.getLogger("remote.selectChannel.communication.KEventCommunicator")

    def __init__(self, *args, **kwargs):
        raise NotImplementedError("KEvent support not yet implemented!")
        super(KEventCommunicator, self).__init__(*args, **kwargs)


class PollCommunicator(QueuedCommunicator):
    logger = logging.getLogger("remote.selectChannel.communication.PollCommunicator")

    def __init__(self, *args, **kwargs):
        raise NotImplementedError("Poll support not yet implemented!")
        super(PollCommunicator, self).__init__(*args, **kwargs)
