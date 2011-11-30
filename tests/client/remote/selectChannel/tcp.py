import logging

from channel import SelectChannel


class TCPChannel(SelectChannel):
    logger = logging.getLogger("remote.selectChannel.tcp.TCPChannel")

    reliable = True
    ordered = True

    def __init__(self, *args, **kwargs):
        self.protocolName = "TCP"

        self.metadata = {}

        super(TCPChannel, self).__init__(*args, **kwargs)

    @classmethod
    def supportsArgs(cls, **kwargs):
        return kwargs['reliable'] == True or kwargs['ordered'] == True

    def connect(self, remoteAddr, **kwargs):
        self.logger.debug("Connecting using %s.", self.protocolName)

        # Connect.
        #XXX: HACK! (instead, Communicator should have a list of sockets awaiting .connect() calls)
        import socket
        while True:
            try:
                self.socket.connect(remoteAddr)
            except socket.error, e:
                if e.errno == 115:
                    # 115 == "Operation now in progress"; that just means we need to wait longer.
                    import select
                    sl = [self.socket]
                    self.logger.debug("select returned: %r", select.select(sl, sl, sl))
                else:
                    self.logger.exception("connect failed with socket error!")
                    return False
            except Exception, e:
                self.logger.exception("connect failed with %s error!", e.__class__)
                return False
            else:
                break
        #XXX: /HACK!

        self.metadata = {
                'remoteAddress': remoteAddr,
                }

        # Successfully connected. Now call super to set variables and enable
        # encryption if requested.
        return super(TCPChannel, self).connect(remoteAddr, **kwargs)

    def _readStream(self, requestedBytes=-1, **kwargs):
        # We don't support any keyword arguments.
        assert(len(kwargs) == 0)

        return self.socket.recv(requestedBytes), self.metadata

    def _writeStream(self, message, **kwargs):
        # We don't support any keyword arguments.
        assert(len(kwargs) == 0)

        return self.socket.send(message)
