import logging
import io
import re

from remote.stream import IncomingQueuedStream, OutgoingQueuedStream
from remote.channel import QueuedChannel


logger = logging.getLogger("remote.netstring")


NETSTRING_MAX_LENGTH_BYTES = 9
NETSTRING_READ_BUFFER_SIZE = 8192


class IncomingQueuedNetstringStream(IncomingQueuedStream):
    """Stream wrapper which splits the target stream's data into messages using netstrings, and enqueues each message.

    """
    netstringLengthPattern = re.compile(
            r'^([0-9]{1,%d}):' % (NETSTRING_MAX_LENGTH_BYTES, ))

    def __init__(self, *args, **kwargs):
        super(IncomingQueuedNetstringStream, self).__init__(*args, **kwargs)

        self._incomingData = io.BufferedReader(self.targetStream, buffer_size=NETSTRING_READ_BUFFER_SIZE)

    def handleRead(self):
        """Read an incoming netstring message from the socket, and queue its
        contents for reading.

        This should be called by a ready-polling class like SelectCommunicator.

        """
        try:
            lenString = self._incomingData.peek(NETSTRING_MAX_LENGTH_BYTES + 1)
            match = self.netstringLengthPattern.match(lenString)
            if match is None:
                if len(lenString) >= NETSTRING_MAX_LENGTH_BYTES + 1:
                    # Incorrectly-formatted length.
                    logger.warn("Incoming message doesn't have a valid netstring length specifier! Discarding.")
                    logger.debug("Discarding message: %r", self._incomingData.read1())
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
                logger.warn("Incoming netstring message doesn't have a trailing comma! Discarding.")
                logger.debug("Discarding message: %r", self._incomingData.read1())

        except io.BlockingIOError:
            # Not enough data yet.
            return

        logger.debug("Read %d bytes: %r", len(incomingMessage), incomingMessage)

        # Enqueue message.
        self._incomingMessages.append(incomingMessage)

        self._emit(self.onIncomingMessageQueued, "incoming message queued")


class OutgoingQueuedNetstringStream(OutgoingQueuedStream):
    """Stream wrapper which formats outgoing binary (str) messages as netstrings.

    """
    def write(self, message):
        if len(str(len(message))) > NETSTRING_MAX_LENGTH_BYTES:
            raise ValueError("Message is too big! Length is %d, but should be less than or equal to %d.",
                    len(message), "9" * NETSTRING_MAX_LENGTH_BYTES)

        encoded = "%d:%s," % (len(message), message)
        super(OutgoingQueuedStream, self).write(encoded)


class IOQueuedNetstringStream(IncomingQueuedNetstringStream, OutgoingQueuedNetstringStream):
    pass


class QueuedNetstringChannel(QueuedChannel):
    """Queued Channel subclass which implements netstring encoding and decoding using IOQueuedNetstringStream.

    """
    queuedStreamWrapperType = IOQueuedNetstringStream
