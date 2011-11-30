#!/usr/bin/env python

# Set up the path so we can import `remote`.
from _path import setPath
setPath(__file__)


from multiprocessing import Process
import logging
import SocketServer
import string
import sys

from unittestwrapper import unittest

from remote.selectChannel.communication import SelectCommunicator
from remote.selectChannel.server import TCPHandler
from remote.control import Control


logger = logging.getLogger("remote.tests.queuedClientServer_test")


class TestQueuedClientServer(unittest.TestCase):
    colorLogFmtTemplate = string.Template(
            '%(levelname)s'
            ':'
            '${location}'
            ':'
            '%(name)s'
            ':'
            '%(message)s'
            )
    consoleLogFmtTemplate = string.Template(
            '\033[38;5;16;48;5;${color}m${location}'
            '\033[0;1;38;5;59m:'
            '\033[0;1m%(levelname)s'
            '\033[38;5;59m:'
            '\033[38;5;${color}m%(name)s'
            '\033[38;5;59m: '
            '\033[0;38;5;${color}m%(message)s'
            '\033[m'
            )
    clientLogFmt = consoleLogFmtTemplate.safe_substitute(location='client')
    serverLogFmt = consoleLogFmtTemplate.safe_substitute(location='server')
    clientColorLogFmt = consoleLogFmtTemplate.safe_substitute(location='client', color=70)
    serverColorLogFmt = consoleLogFmtTemplate.safe_substitute(location='server', color=75)

    def setUp(self):
        self.serverAddr = '127.0.0.1', 2695
        self.text = "This is my text. Isn't it beautiful?"

        def runServer():
            # Configure server logging.
            for handler in logging.getLogger().handlers:
                if isinstance(handler, logging.StreamHandler) and handler.stream == sys.stderr:
                    handler.setFormatter(logging.Formatter(self.serverColorLogFmt))
                else:
                    handler.setFormatter(logging.Formatter(self.serverLogFmt))

            server = SocketServer.TCPServer(self.serverAddr, TCPHandler)
            server.serve_forever()

        # Start the server.
        self.server = Process(target=runServer)
        self.server.start()

        #XXX: HACK!
        # Wait a bit to hopefully ensure that the server has started and is listening. (we should probably replace this
        # with a queue or something, since that would also allow us to signal a graceful shutdown in the server.)
        import time
        time.sleep(0.2)
        #XXX: /HACK!

        # Configure client logging.
        for handler in logging.getLogger().handlers:
            if isinstance(handler, logging.StreamHandler) and handler.stream == sys.stderr:
                handler.setFormatter(logging.Formatter(self.clientColorLogFmt))
            else:
                handler.setFormatter(logging.Formatter(self.clientLogFmt))

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
