#!/usr/bin/env python

# Set up the path so we can import `remote`.
from _path import setPath
setPath(__file__)


from multiprocessing import Process
import logging
import SocketServer

from unittestwrapper import unittest

from remote.selectChannel.communication import SelectCommunicator
from remote.selectChannel.server import TCPHandler
from remote.control import Control


logger = logging.getLogger("remote.tests.queuedClientServer_test")


class TestQueuedClientServer(unittest.TestCase):
    def setUp(self):
        self.serverAddr = '127.0.0.1', 2695
        self.text = "This is my text. Isn't it beautiful?"

        def runServer():
            server = SocketServer.TCPServer(self.serverAddr, TCPHandler)
            server.serve_forever()

        # Start the server.
        self.server = Process(target=runServer)
        self.server.start()

        try:
            # Connect to the server.
            self.control = Control(self.serverAddr)
            self.channel = self.control.controlChannel
            print "CHANNEL ASSIGNED:", self.channel

            #TODO: Control should be doing this!
            self.comm = SelectCommunicator()
            self.comm.registerChannel(self.channel)

        except:
            logger.exception("Failed to set up %s test!", self.__class__)
            self.server.terminate()

    def tearDown(self):
        # Shut down the server forcefully.
        self.server.terminate()

        if hasattr(self, 'channel'):
            del self.channel

        if hasattr(self, 'control'):
            del self.control

        #TODO: Control should be doing this!
        if hasattr(self, 'comm'):
            del self.comm

    def test_for_echo(self):
        def receiveResponse(sender, **kwargs):
            addr = kwargs['remoteAddress']
            self.assertEqual(addr, self.serverAddr)

            message = kwargs['message']
            logger.debug("Got response from %s:%d: %r", addr[0], addr[1], message)

            expectedMessage = "Hello, %s:%d! You said %r." % (addr[0], addr[1], self.text)
            self.assertEqual(message, expectedMessage)

            # Exit the loop, so we end the test.
            self.comm.exitLoop()

        self.channel.incomingPacket.connect(receiveResponse, self.channel)

        self.channel.send(self.text)

        # Start the Communicator loop.
        self.comm.loop()


if __name__ == '__main__':
    logging.basicConfig(level=0)

    # Build the suite of tests
    suite = unittest.TestLoader().loadTestsFromTestCase(TestQueuedClientServer)

    # Run the tests
    unittest.TextTestRunner(verbosity=2).run(suite)
