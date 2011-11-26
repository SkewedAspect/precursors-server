import collections
import logging
import io
import warnings


logger = logging.getLogger("remote.stream")


class Stream(io.RawIOBase):
    """Base class for stream wrappers: streams which wrap other streams, adding functionality.

    """
    def __init__(self, targetStream, *args, **kwargs):
        self.targetStream = targetStream

        self.onReadFinished = []
        self.onWriteFinished = []

        super(Stream, self).__init__(*args, **kwargs)

    def _emit(self, handlerList, eventName, **kwargs):
        logger.debug("Emitting %r event.", eventName)
        for handler in handlerList:
            try:
                handler(**kwargs)
            except:
                logger.exception("Exception encountered when calling %r handler!", eventName)

    @classmethod
    def factory(cls, **kwargs):
        def fact(*args, **kwargsOverrides):
            kwa = kwargs.copy()
            kwa.update(kwargsOverrides)
            return cls(*args, **kwa)

        return fact

    def read(self, requestedBytes=-1):
        """Read incoming data from the target stream.

        """
        dataRead = self.targetStream.read(requestedBytes)

        self._emit(self.onReadFinished, "read finished", requestedBytes=requestedBytes, dataRead=dataRead)

        return dataRead

    def write(self, data):
        """Write outgoing data to the target stream.

        """
        bytesWritten = self.targetStream.write(data)

        self._emit(self.onWriteFinished, "write finished", bytesWritten=bytesWritten, data=data)

        return bytesWritten


class OutgoingQueuedStream(Stream):
    """Stream wrapper which queues all outgoing messages instead of writing them instantly.

    """
    def __init__(self, *args, **kwargs):
        self._outgoingMessages = collections.deque()

        self.onOutgoingQueueEmpty = []
        self.onOutgoingMessageQueued = []

        super(OutgoingQueuedStream, self).__init__(*args, **kwargs)

    def write(self, message):
        """Write message to the outgoing queue.

        """
        self._outgoingMessages.append(message)

        self._emit(self.onOutgoingMessageQueued, "outgoing message queued")

        return len(message)

    def handleWrite(self):
        """Write the next outgoing message to the socket.

        This should be called by a ready-polling class like Select.

        """
        if len(self._outgoingMessages) == 0:
            self._emit(self.onOutgoingQueueEmpty, "outgoing queue empty")
            return

        outgoingMessage = self._outgoingMessages.popleft()

        bytesWritten = self.targetStream.write(outgoingMessage)

        if bytesWritten < len(outgoingMessage):
            logger.debug("Wrote first %d bytes of %r; re-queuing rest.",
                    bytesWritten, outgoingMessage)
            self._outgoingMessages.appendleft(outgoingMessage[bytesWritten:])
        else:
            logger.debug("Wrote all %d bytes of %r",
                    bytesWritten, outgoingMessage)

        if len(self._outgoingMessages) == 0:
            self._emit(self.onOutgoingQueueEmpty, "outgoing queue empty")


class IncomingQueuedStream(Stream):
    """Stream wrapper which can read incoming messages into a queue, instead of waiting for a read() call to read from
    the target stream.

    """
    def __init__(self, *args, **kwargs):
        self._incomingMessages = collections.deque()

        self.onIncomingQueueEmpty = []
        self.onIncomingMessageQueued = []

        super(IncomingQueuedStream, self).__init__(*args, **kwargs)

    def read(self, requestedBytes=-1):
        """Read message from the incoming queue.

        """
        if len(self._incomingMessages) == 0:
            self._emit(self.onIncomingQueueEmpty, "incoming queue empty")
            return None

        try:
            #TODO: Implement requestedBytes support
            if requestedBytes != -1:
                warnings.warn("requestedBytes support not currently implemented.", FutureWarning)
            incomingMessage = self._incomingMessages.popleft()

        except IndexError:
            incomingMessage = None

        if len(self._incomingMessages) == 0:
            self._emit(self.onIncomingQueueEmpty, "incoming queue empty")

        return incomingMessage

    def handleRead(self):
        """Read an incoming message from the socket, and queue it for reading.

        This should be called by a ready-polling class like Select.

        """
        incomingMessage = self.targetStream.read()
        logger.debug("Read %d bytes: %r", len(incomingMessage),
                incomingMessage)

        self._incomingMessages.append(incomingMessage)

        self._emit(self.onIncomingMessageQueued, "incoming message queued")


class IOQueuedStream(IncomingQueuedStream, OutgoingQueuedStream):
    """Stream wrapper which queues both incoming and outgoing messages.

    This allows messages to be read and written without blocking, provided handleRead() and handleWrite() are called at
    appropriate times.

    """
