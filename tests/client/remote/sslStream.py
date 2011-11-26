import logging
import ssl

from remote.stream import Stream


class SSLStream(Stream):
    """Stream wrapper which starts an SSL or TLS session on the target stream.

    The target stream probably has to actually be a socket, or this may explode in new and interesting ways. Basically,
    make sure this is the first stream wrapper applied to a Channel, and you should be good.

    """
    logger = logging.getLogger("remote.cryptors.sslStream.SSLStream")

    supportedProtocols = dict()

    for name in dir(ssl):
        if name.startswith('PROTOCOL_'):
            key = name[len('PROTOCOL_'):]
            val = getattr(ssl, name)
            supportedProtocols[key] = val
            locals()[name] = val

    connInfoFormat = """SSL connection info:
    Peer name: %s
    Cipher chosen: %s
    Peer certificate:
%s"""

    def __init__(self, *args, **kwargs):
        super(SSLStream, self).__init__(*args, **kwargs)

        self._targetStream = None

        self.protocol = kwargs.get('protocol', self.supportedProtocols["TLSv1"])

    def get_targetStream(self):
        return self._targetStream

    def set_targetStream(self, newTarget):
        assert self._targetStream is None
        assert newTarget is not None

        try:
            self._wrappedTarget = newTarget

            # Set up TLS on our socket.
            self._targetStream = ssl.wrap_socket(
                    self._wrappedTarget,
                    ca_certs="/etc/ca_certs_file",
                    cert_reqs=ssl.CERT_REQUIRED,
                    ssl_version=self.protocol,

                    # Allow all ciphers except ECDSA, since its implementation in OpenSSL was broken.
                    # (see https://secure.wikimedia.org/wikipedia/en/wiki/ECDSA)
                    #TODO: We probably should exclude a few more, but the @STRENGTH sort should help.
                    ciphers='DEFAULT !ECDSA @STRENGTH',
                    )

            # Set target socket to non-blocking.
            if hasattr(self._wrappedTarget, 'settimeout'):
                self._wrappedTarget.settimeout(0.0)

        except:
            self._wrappedTarget = None
            self._targetStream = None
            raise

    targetStream = property(get_targetStream, set_targetStream)

    def logConnectionInfo(self, remoteHost, remotePort, **kwargs):
        # Print out some info about our TLS connection.
        import pprint
        self.logger.debug(
                self.connInfoFormat,
                repr(self.socket.getpeername()),
                self.socket.cipher(),
                pprint.pformat(self.socket.getpeercert()).replace('\n', '\n        ')
                )
