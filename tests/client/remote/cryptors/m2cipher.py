from cryptor import Cryptor
from M2Crypto.EVP import Cipher
import os
import logging


logger = logging.getLogger("remote.cryptors.m2cipher")


ENCODE = 1
DECODE = 0


class M2CipherCryptor(Cryptor):
    """Implementation of an AES based cryptor.

    """
    def __init__(self, key=None, iv=None, algorithm=None, *args, **kwargs):
        """Instantiate an M2CipherCryptor object.

        key - The 128bit random cipher key.
        iv - The random initialization vector.
        algorithm - the M2Crypto algorithm to use.

        """
        self.key = key
        self.iv = iv
        self.algorithm = algorithm

        if self.key is None:
            self.key = os.random(16)

        if self.iv is None:
            self.iv = os.random(16)

        if self.algorithm is None:
            self.algorithm = 'aes_128_cbc'

        super(M2CipherCryptor, self).__init__(*args, **kwargs)

    def encrypt(self, plaintext):
        """Encrypts plaintext with key and iv.

        Returns ciphertext as a string.

        """
        return self._cryptoOperation(plaintext, self.algorithm, self.key, self.iv, ENCODE)

    def decrypt(self, ciphertext):
        """Decrypts ciphertext with key and iv.

        Returns plaintext as a string.

        """
        return self._cryptoOperation(ciphertext, self.algorithm, self.key, self.iv, DECODE)

    @staticmethod
    def _cryptoOperation(data, algorithm, key, iv, operation):
        """Encrypts or decrypts data with the given key and initialization vector.

        This function performs an encryption if operation is ENCODE, or a
        decryption if operation is DECODE.

        """
        # Use M2Crypto's Cipher object. Odd work flow, but whatever.
        cipher = Cipher(alg=algorithm, key=key, iv=iv, op=operation)
        result = cipher.update(data)
        result = result + cipher.final()
        del cipher

        return result


