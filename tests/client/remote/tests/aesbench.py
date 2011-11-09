#!/usr/bin/env python

import sys
import os


# Add the parent directory of the tests dir to the path.
sys.path += [os.path.dirname(os.path.dirname(os.path.abspath(sys.argv[0])))]


import os
import datetime
from aescrypt import encrypt


plaintextBytes = 150000  # 150K bytes of plaintext
iterations = 10000

key = os.urandom(16)
iv = os.urandom(16)
text = os.urandom(plaintextBytes)


totalTime = datetime.timedelta()

print 'Starting benchmark...'

for i in range(iterations):
    starttime = datetime.datetime.now()
    text = encrypt(text, key, iv)
    endtime = datetime.datetime.now()

    totalTime += endtime - starttime

print 'Total time:', totalTime
print 'Average time: %0.6f ms over %d iterations.' % (
        totalTime.total_seconds() * 1000 / iterations,
        iterations
        )
