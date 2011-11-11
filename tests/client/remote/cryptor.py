from abc import ABCMeta, abstractmethod
import logging


logger = logging.getLogger("remote.crypt")


class Cryptor(object):
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
