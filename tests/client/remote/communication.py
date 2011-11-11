from abc import ABCMeta, abstractmethod
import logging

from remote.stream import OutgoingQueuedStream


logger = logging.getLogger("remote.communication")


class Communicator(object):
    __metaclass__ = ABCMeta

    autoApplyStreamWrappers = []

    def __init__(self, *args, **kwargs):
        self.looping = False
        self._continue = False

        self.channels = set()

        super(Communicator, self).__init__(*args, **kwargs)

    def registerChannel(self, chan):
        self.channels.add(chan)

        for wrapper in self.autoApplyStreamWrappers:
            chan.target = wrapper(chan.target)

    @abstractmethod
    def checkIO(self, timeout=None):
        """Checks all registered Channels for ones which are ready for I/O, and calls their handlers.

        If no Channels are ready, and timeout is:
        - 0: return immediately
        - None or not specified: block indefinitely until a Channel becomes ready, then return
        - Any positive number: wait `timeout` seconds or until a Channel becomes ready, then return

        """
        pass

    def loop(self, timeout=None):
        """Enters a run loop which checks all registered Channels for ones which are ready for I/O, and calls their
        handlers.

        Call `exitLoop` to exit the run loop.

        On a given iteration, if no Channels are ready and timeout is:
        - 0: loop again immediately (YOU PROBABLY DON'T WANT THIS!)
        - None or not specified: block indefinitely until a Channel becomes ready, then loop
        - Any positive number: wait `timeout` seconds or until a Channel becomes ready, then loop

        """
        assert self._looping == False

        self._continue = True
        self._looping = True
        try:
            while self._continue:
                self.checkIO()

        finally:
            self._looping = False

    def exitLoop(self):
        """Exits the run loop started by `loop`.

        """
        self._continue = False


class QueuedCommunicator(object):
    autoApplyStreamWrappers = [OutgoingQueuedStream]

    def __init__(self, *args, **kwargs):
        super(QueuedCommunicator, self).__init__(*args, **kwargs)

        self.awaitingWrite = set()

    def onOutgoingQueued(self, sock):
        self.awaitingWrite.add(sock)

    def onOutgoingFinished(self, sock):
        self.awaitingWrite.remove(sock)
