#!/usr/bin/env python

# Set up the path so we can import `remote`.
from _path import setPath
setPath(__file__)

from decimal import Decimal

from numScale import prefixNum


bytes_ = lambda num: prefixNum(num, 'B', precision=1, binary=True)
seconds = lambda num: prefixNum(num, 'sec', precision=3)


def doTimings(plaintextBytes, iterations=1000, repetitions=5, useTimeit=True,
        indent=''):
    if useTimeit:
        # Use timeit.
        setup = '''
import os
from remote.cryptors.m2cipher import M2CipherCryptor
key = os.urandom(16)
iv = os.urandom(16)
cryptor = M2CipherCryptor(key=key, iv=iv)
text = os.urandom(%(plaintextBytes)d)
''' % {
            'plaintextBytes': plaintextBytes,
            }

        test = '''
text = cryptor.encrypt(text)
'''

        from timeit import Timer
        t = Timer(test, setup)
        times = [Decimal(time) for time in
                t.repeat(repeat=repetitions, number=iterations)]
        print indent + "%d sets of %d iterations:" % (
                repetitions,
                iterations,
                ),
        print ", ".join(
                seconds(time) for time in times
                )
        print indent + "    Average time per set:", \
                seconds(sum(times) / repetitions)
        print indent + "    Average time per iteration:", \
                seconds(sum(times) / repetitions / iterations)
        print indent + "    Total iterations: %d" % (
                len(times),
                )
        print indent + "    Total time:", \
                seconds(sum(times))

    else:
        import os
        import datetime

        from remote.cryptors.m2cipher import M2CipherCryptor

        key = os.urandom(16)
        iv = os.urandom(16)
        cryptor = M2CipherCryptor(key=key, iv=iv)
        text = os.urandom(plaintextBytes)

        print indent + 'Starting benchmark...'

        for r in range(repetitions):
            totalTime = datetime.timedelta()

            for i in range(iterations):
                starttime = datetime.datetime.now()
                text = cryptor.encrypt(text)
                endtime = datetime.datetime.now()

                totalTime += endtime - starttime

            print
            print indent + 'Repetition %d:' % (r, )
            print indent + '    Total time:', totalTime
            print indent + '    Average time: %0.6f ms over %d iterations.' % (
                    totalTime.total_seconds() * 1000 / iterations,
                    iterations
                    )


plaintextBytes = [
        1.5 * 1024,         # 1.5 KiB of plaintext
        15 * 1024,          # 15 KiB of plaintext
        150 * 1024,         # 150 KiB of plaintext
        1.5 * 1024 * 1024,  # 1.5 MiB of plaintext
        ]


for ptb in plaintextBytes:
    print "Encrypting %s of plaintext:" % (bytes_(ptb), )
    doTimings(ptb, indent='    ')
