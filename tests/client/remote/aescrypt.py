from M2Crypto.EVP import Cipher

__all__ = ['encrypt', 'decrypt']

ENCODE = 1
DECODE = 0


def _cryptoOperation(data, algorithm, key, iv, operation):
    """Encrypts or decrypts data with the given key and initialization vector.

    This function performs an encryption if operation is ENCODE, or a
    decryption if operation is DECODE.
    """
    # If a iv is None, then we simply use a null IV. This is insecure,
    # however, since it is trivial to gain insight into the data stored
    # in each message through statistical analysis. However, it is still
    # valid, technically.
    if iv is None:
        iv = '\0' * 16

    if algorithm is None:
        algorithm = 'aes_128_cbc'

    # Use M2Crypto's Cipher object. Odd work flow, but whatever.
    cipher = Cipher(alg=algorithm, key=key, iv=iv, op=operation)
    result = cipher.update(data)
    result = result + cipher.final()
    del cipher

    return result


def encrypt(plaintext, key, iv=None, algorithm=None):
    """Encrypts plaintext with key and iv.

    """
    return _cryptoOperation(plaintext, algorithm, key, iv, ENCODE)


def decrypt(ciphertext, key, iv=None, algorithm=None):
    """Decrypts ciphertext with key and iv.

    """
    return _cryptoOperation(ciphertext, algorithm, key, iv, DECODE)
