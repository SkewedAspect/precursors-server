import collections
import logging
import warnings

import dispatch


class BaseTransport(object):
    logger = logging.getLogger('remote.transports.base.BaseTransport')

    transportType = None

    onIncomingQueueEmpty = dispatch.Signal()
    onIncomingMessageQueued = dispatch.Signal(["message", "queueLength"])

    onOutgoingQueueEmpty = dispatch.Signal()
    onOutgoingMessageQueued = dispatch.Signal(["message", "queueLength"])

    def __init__(self):
        self._outgoingMessages = collections.deque()
        self._incomingMessages = collections.deque()

    def handleExceptionalCondition(self):
        self.logger.error("Got exceptional condition!")

    def write(self, message):
        """Write message to the outgoing queue.

        """
        self._outgoingMessages.append(message)

        self._emit(self.onOutgoingMessageQueued, "outgoing message queued",
                message=message, queueLength=len(self._outgoingMessages))

        return len(message)

    def handleWrite(self):
        """Write the next outgoing message to the socket.

        This should be called by a ready-polling class like SelectCommunicator.

        """
        self.logger.debug("handleWrite called.")

        if len(self._outgoingMessages) == 0:
            # If we got here, we probably weren't properly removed from the list of streams with outgoing messages;
            # signal whatever's managing outgoing writes that we're done.
            self._emit(self.onOutgoingQueueEmpty, "outgoing queue empty")
            return

        outgoingMessage = self._outgoingMessages.popleft()

        bytesWritten = self.socket.write(outgoingMessage)

        if bytesWritten < len(outgoingMessage):
            self.logger.debug("Wrote first %d bytes of %r; re-queuing rest.",
                    bytesWritten, outgoingMessage)
            self._outgoingMessages.appendleft(outgoingMessage[bytesWritten:])
        else:
            self.logger.debug("Wrote all %d bytes of %r",
                    bytesWritten, outgoingMessage)

        if len(self._outgoingMessages) == 0:
            self._emit(self.onOutgoingQueueEmpty, "outgoing queue empty")

    def _popIncomingMessage(self):
        try:
            return self._incomingMessages.popleft()

        except IndexError:
            return None

        finally:
            if len(self._incomingMessages) == 0:
                self._emit(self.onIncomingQueueEmpty, "incoming queue empty")

    def readinto(self, into):
        """Read message from the incoming queue into the given bytearray.

        """
        incomingMessage, metadata = self._popIncomingMessage()

        messageLen = len(incomingMessage)
        lenDiff = messageLen - len(into)
        if lenDiff > 0:
            raise RuntimeError(
                    "Incoming message longer than 'into' bytearray in readinto! Discarding %d bytes of data!"
                    % (lenDiff, )
                    )

        into[:messageLen] = incomingMessage

        self._emit(self.onReadFinished, "read finished",
                requestedBytes=len(into), bytesRead=messageLen, dataRead=bytes(into[:messageLen]))

        return messageLen, metadata

    def read(self, requestedBytes=-1):
        """Read message from the incoming queue.

        """
        if len(self._incomingMessages) == 0:
            self._emit(self.onIncomingQueueEmpty, "incoming queue empty")
            return None

        #TODO: Implement requestedBytes support
        if requestedBytes != -1:
            warnings.warn("requestedBytes support not currently implemented.", FutureWarning)

        incomingMessage = self._popIncomingMessage()

        if len(self._incomingMessages) == 0:
            self._emit(self.onIncomingQueueEmpty, "incoming queue empty")

        return incomingMessage

    def handleRead(self):
        """Read an incoming message from the socket, and queue it for reading.

        This should be called by a ready-polling class like SelectCommunicator.

        """
        self.logger.debug("handleRead called.")

        message = self.targetStream.read()
        self.logger.debug("Read %d bytes: %r", len(message), message)

        # NOTE: 'message' is actually a tuple: (data, metadata); We just store the whole thing.
        self._incomingMessages.append(message)

        self._emit(self.onIncomingMessageQueued, "incoming message queued",
                message=message, queueLength=len(self._outgoingMessages))
