import collections
import logging
import io
import re


logger = logging.getLogger("remote.stream")


class Stream(io.RawIOBase):
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

        bytesWritten = self.target.write(outgoingMessage)

        if bytesWritten < len(outgoingMessage):
            logger.debug("Wrote first %d bytes of %r; re-queuing rest.",
                    bytesWritten, outgoingMessage)
            self._outgoingMessages.appendleft()
        else:
            logger.debug("Wrote all %d bytes of %r",
                    bytesWritten, outgoingMessage)

        if len(self._outgoingMessages) == 0:
            self._emit(self.onOutgoingQueueEmpty, "outgoing queue empty")


class IncomingQueuedStream(Stream):
    def __init__(self, *args, **kwargs):
        self._incomingMessages = collections.deque()

        self.onIncomingQueueEmpty = []
        self.onIncomingMessageQueued = []

        super(IncomingQueuedStream, self).__init__(*args, **kwargs)

    def read(self, message):
        """Read message from the incoming queue.

        """
        if len(self._incomingMessages) == 0:
            self._emit(self.onIncomingQueueEmpty, "incoming queue empty")
            return None

        try:
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
        incomingMessage = self.target.read()
        logger.debug("Read %d bytes: %r", len(incomingMessage),
                incomingMessage)

        self._incomingMessages.append(incomingMessage)

        self._emit(self.onIncomingMessageQueued, "incoming message queued")


class IOQueuedStream(IncomingQueuedStream, OutgoingQueuedStream):
    pass


NETSTRING_MAX_LENGTH_BYTES = 9
NETSTRING_READ_BUFFER_SIZE = 8192


class IncomingQueuedNetstringStream(IncomingQueuedStream):
    netstringLengthPattern = re.compile(
            r'^([0-9]{1,%d}):' % (NETSTRING_MAX_LENGTH_BYTES, ))

    def __init__(self, *args, **kwargs):
        super(IncomingQueuedNetstringStream, self).__init__(*args, **kwargs)

        self._incomingData = io.BufferedReader(self.target,
                buffer_size=NETSTRING_READ_BUFFER_SIZE)

    def handleRead(self):
        """Read an incoming netstring message from the socket, and queue its
        contents for reading.

        This should be called by a ready-polling class like Select.

        """
        try:
            lenString = self._incomingData.peek(NETSTRING_MAX_LENGTH_BYTES + 1)
            match = self.netstringLengthPattern.match(lenString)
            if match is None:
                if len(lenString) >= NETSTRING_MAX_LENGTH_BYTES + 1:
                    # Incorrectly-formatted length.
                    logger.warn("Incoming message doesn't have a valid netstring "
                            "length specifier! Discarding.")
                    logger.debug("Discarding message: %r",
                            self._incomingData.read1())
                    return

                # Otherwise, we don't have enough data yet, so just wait.
                return

            msgLen = int(match.group(1))

            # Pull the length portion and trailing colon out, and discard them.
            self._incomingData.read(len(match.group(0)))

            # Get the message contents.
            incomingMessage = self._incomingData.read(msgLen)

            # Check for message end marker. (comma)
            if self._incomingData.read(1) != ',':
                logger.warn("Incoming netstring message doesn't have a trailing "
                        "comma! Discarding.")
                logger.debug("Discarding message: %r",
                        self._incomingData.read1())

        except io.BlockingIOError:
            # Not enough data yet.
            return

        logger.debug("Read %d bytes: %r", len(incomingMessage),
                incomingMessage)

        # Enqueue message.
        self._incomingMessages.append(incomingMessage)

        self._emit(self.onIncomingMessageQueued, "incoming message queued")


class OutgoingQueuedNetstringStream(OutgoingQueuedStream):
    def write(self, message):
        if len(str(len(message))) > NETSTRING_MAX_LENGTH_BYTES:
            raise ValueError("Message is too big! Length is %d, but should be "
                    "less than or equal to %d.",
                    len(message), "9" * NETSTRING_MAX_LENGTH_BYTES)

        encoded = "%d:%s," % (len(message), message)
        super(OutgoingQueuedStream, self).write(encoded)


class IOQueuedNetstringStream(IncomingQueuedStream, OutgoingQueuedStream):
    pass
