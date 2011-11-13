from abc import ABCMeta, abstractmethod
from stream import Stream
import logging


logger = logging.getLogger("remote.crypt")


class Cryptor(Stream):
    """Abstract base class for cryptographic objects capable
    of performing encryption and decryption of strings.

    """
    __metaclass__ = ABCMeta

    @abstractmethod
    def encrypt(self, plaintext):
        """Encrypts string plaintext, returning a ciphertext.

        """
        pass

    @abstractmethod
    def decrypt(self, ciphertext):
        """Decrypts string ciphertext, returning a plaintext.

        """
        pass

    def read(self, requestedBytes=-1):
        """Read incoming data from the target stream and decrypt it.

        """
        ciphertext = super(Cryptor, self).read(requestedBytes)

        return self.decrypt(ciphertext)

    def write(self, data):
        """Encrypt outgoing data and then write it to the target stream.

        """
        data = self.encrypt(data)
        return super(Cryptor, self).write(data)

