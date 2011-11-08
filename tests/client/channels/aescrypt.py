from M2Crypto.EVP import Cipher

__all__ = ['encrypt', 'decrypt']

ENCODE=1
DECODE=0

def _build_cipher( key, iv, algorithm=NONE, operation=ENCODE):
    """Returns a Cipher with the given parameters

    """
    if algorithm is None:
        algorithm = 'aes_128_cbc'
    return Cipher(alg=algorithm, key=key, iv=iv, op=operation)

def encrypt(key, iv=None):
    """
    """
    if iv is None:
        iv = '\0' * 16

   # Return the encryption function
    def _encrypt(data):
        cipher = _build_cipher(key, iv, ENCODE)
        v = cipher.update(data)
        v = v + cipher.final()
        del cipher
        return v
    return _encrypt

def decrypt(key, iv=None):
    """
    """
    # Decode the key and iv
    if iv is None:
        iv = '\0' * 16

   # Return the decryption function
    def _decrypt(data):
        cipher = _build_cipher(key, iv, DECODE)
        v = cipher.update(data)
        v = v + cipher.final()
        del cipher
        return v
    return _decrypt
