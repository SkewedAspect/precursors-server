import logging
import socket

from remote.transports.base import BaseTransport
from remote.protos.requests_pb2 import Channel


class UDPTransport(BaseTransport):
    logger = logging.getLogger("remote.transports.udp.UDPTransport")
    transportType = Channel.SSL

    def __init__(self, remoteAddr=None):
        self.defaultTargetAddress = None

        self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

        return super(UDPTransport, self).__init__(remoteAddr)

    def connect(self, remoteAddr):
        self.logger.debug("Connecting.")

        self.defaultTargetAddress = remoteAddr

        # Bind to the local address and port.
        self.socket.bind()

        return super(UDPTransport, self).connect(remoteAddr)

    def disconnect(self):
        self.socket.disconnect()

    def _readStream(self, requestedBytes=-1, into=None, **kwargs):
        # We don't support any keyword arguments.
        assert(len(kwargs) == 0)

        if into is not None:
            # We should use recvfrom_into, to read into the given buffer.
            if requestedBytes == -1 or (into is not None and requestedBytes > len(into)):
                requestedBytes = len(into)

            (bytesRead, address) = self.socket.recvfrom_into(into, requestedBytes)

            metadata = {
                    'remoteAddress': address,
                    }
            return bytesRead, metadata

        else:
            # We should simply use recvfrom, and return a new string.
            (data, address) = self.socket.recvfrom(requestedBytes)

            metadata = {
                    'remoteAddress': address,
                    }
            return data, metadata

    def _writeStream(self, message, **kwargs):
        remoteAddress = kwargs.pop('address', self.defaultTargetAddress)

        # We don't support any other keyword arguments.
        assert(len(kwargs) == 0)

        self.socket.sendto(message, remoteAddress)
