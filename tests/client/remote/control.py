import logging
import pprint
import select
import socket
import ssl

from protos.requests_pb2 import Channel


class Control(object):
    logger = logging.getLogger("remote.control.Control")

    connectionTypeNames = {
            Channel.SSL: 'SSL',
            Channel.TCP: 'TCP',
            Channel.UDP: 'UDP',
            }

    def __init__(self, hostName, port=2695, *args, **kwargs):
        self._looping = False
        self._continue = False

        self.hostName = hostName
        port = port

        self.transports = [None] * 3

        self.awaitingWrite = set()

        baseControlConnection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        # Set up TLS on our socket.
        sslConn = ssl.wrap_socket(
                baseControlConnection,
                ca_certs="/etc/ca_certs_file",
                cert_reqs=ssl.CERT_REQUIRED,
                ssl_version=ssl.SSLv3,

                # Allow all ciphers except ECDSA, since its implementation in OpenSSL was broken.
                # (see https://secure.wikimedia.org/wikipedia/en/wiki/ECDSA)
                #TODO: We probably should exclude a few more, but the @STRENGTH sort should help.
                ciphers='DEFAULT !ECDSA @STRENGTH',
                )

        # Set target socket to non-blocking.
        sslConn.settimeout(0.0)

        sslConn.connect((hostName, port))

        self.transports[Channel.SSL] = sslConn

        print repr(sslConn.getpeername())
        print sslConn.cipher()
        print pprint.pformat(sslConn.getpeercert())

        # Create the secondary TCP and UDP sockets so we have them when we need them.
        self.transports[Channel.TCP] = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        super(Control, self).__init__(*args, **kwargs)

    def disconnect(self):
        # Close the secondary transports first.
        self.transports[Channel.TCP].close()
        self.transports[Channel.UDP].close()

        # Note that closing the SSLSocket will also close the underlying socket.
        self.transports[Channel.SSL].close()

    def checkIO(self, timeout=None):
        """Checks all registered Channels for ones which are ready for I/O, and calls their handlers.

        If no Channels are ready, and timeout is:
        - 0: return immediately
        - None or not specified: block indefinitely until a Channel becomes ready, then return
        - Any positive number: wait `timeout` seconds or until a Channel becomes ready, then return

        """
        if len(self.channels) == 0:
            # No channels set, so nothing to check for. (otherwise, we always check for read)
            return

        #TODO: Implement "exceptional condition" handling.
        readReady, writeReady, exceptionalCondition = select.select(
                self.transports,    # check for read
                self.awaitingWrite,  # check for write
                self.transports,    # check for "exceptional condition"
                0                    # timeout
                )

        for socket in writeReady:
            socket.handleWrite()

        for socket in readReady:
            socket.handleRead()

        for socket in exceptionalCondition:
            self.logger.error("%r transport is exceptional. No, really.", transport)
            socket.handleExceptionalCondition()

    def loop(self, timeout=None):
        """Enters a run loop which checks all registered Channels for ones which are ready for I/O, and calls their
        handlers.

        Call `exitLoop` to exit the run loop.

        On a given iteration, if no Channels are ready and timeout is:
        - 0: loop again immediately (YOU PROBABLY DON'T WANT THIS! Passing None is preferable when looping, since
            othewise you end up doing a rather CPU-heavy spinlock)
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

    def setupTransport(self, transportType, transport):
        if hasattr(transport, 'onOutgoingQueueEmpty'):
            transport.onOutgoingQueueEmpty.connect(self.onOutgoingFinished, transport)

        if hasattr(transport, 'onOutgoingMessageQueued'):
            transport.onOutgoingMessageQueued.connect(self.onOutgoingQueued, transport)

        self.transports[transportType] = transport

    def onOutgoingQueued(self, transport):
        self.awaitingWrite.add(transport)

    def onOutgoingFinished(self, transport):
        self.awaitingWrite.remove(transport)
