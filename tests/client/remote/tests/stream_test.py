#!/usr/bin/env python

import sys
import os
from StringIO import StringIO

# Add the parent directory of the tests dir to the path.
sys.path += [os.path.dirname(os.path.dirname(os.path.abspath(sys.argv[0])))]

# Get unittest from our wrapper that imports the right version.
from unittestwrapper import unittest

from stream import Stream, OutgoingQueuedStream, IncomingQueuedStream, IOQueuedStream


class TestStream(unittest.TestCase):
    def setUp(self):
        self.text = "This is my text. Isn't it beautiful?"

    def test_stream_read(self):
        target = StringIO(self.text)

        text = Stream(target).read()

        self.assertEqual(text, self.text)

    def test_stream_write(self):
        target = StringIO()

        stream = Stream(target)
        stream.write(self.text)

        self.assertEqual(target.getvalue(), self.text)

    def test_OutgoingQueuedStream_write(self):
        target = StringIO()

        stream = OutgoingQueuedStream(target)
        stream.write(self.text)

        stream.handleWrite()

        self.assertEqual(target.getvalue(), self.text)

    def test_IncomingQueuedStream_read(self):
        target = StringIO(self.text)

        stream = IncomingQueuedStream(target)

        stream.handleRead()

        text = stream.read()

        self.assertEqual(text, self.text)

    def test_IOQueuedStream_roundtrip(self):
        target = StringIO()

        stream = IOQueuedStream(target)
        stream.write(self.text)

        stream.handleWrite()

        target.seek(0)

        stream.handleRead()

        text = stream.read()

        self.assertEqual(text, self.text)


if __name__ == '__main__':
    # Build the suite of tests
    suite = unittest.TestLoader().loadTestsFromTestCase(TestStream)

    # Run the tests
    unittest.TextTestRunner(verbosity=2).run(suite)
