#!/usr/bin/env python

import sys

sys.path += ['..']

import os, datetime
from aescrypt import encrypt, decrypt

key = os.urandom(16)
iv = os.urandom(16)
text = os.urandom(150000) # 150K bytes of plaintext

totalTime = datetime.timedelta()

print 'Starting benchmark...'

for i in range(0, 10000):
    starttime = datetime.datetime.now()
    text = encrypt(text, key, iv)
    endtime = datetime.datetime.now()

    totalTime += endtime - starttime

print 'Average time:', totalTime / 10000

