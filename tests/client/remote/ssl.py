import logging
import ssl

from remote.stream import Stream


class SSLStream(Stream):
    logger = logging.getLogger("remote.cryptors.ssl.SSLStream")

    connInfoFormat = """SSL connection info:
    Peer name: %s
    Cipher chosen: %s
    Peer certificate:
%s"""

    def __init__(self, *args, **kwargs):
        super(SSLStream, self).__init__(*args, **kwargs)

        self._targetStream = None

        self.protocolName = "TLS"

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
                    ssl_version=ssl.PROTOCOL_TLSv1,

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
                pprint.pformat(
                    self.socket.getpeercert()
                    ).replace('\n', '\n        ')
                )
