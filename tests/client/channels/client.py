# Global dictionary of outstanding requests
requests = dict()

class EncryptionType(object):
    SSL = 1
    AES = 2
    NONE = 3

class Client(object):
    host = None

    # AES Keys
    aeskey = None
    oldkey = None

    # Channel vars
    channels = dict()
    control = None

    def __init__(self):
        pass

    def connect(self, host, port):
        """Connects to the server, and creates our SSL control channel.

        """
        control = Channel('Control')
        if control.connect(host, port, True, EncryptionType.SSL):

            # We've successfully connected
            self.host = host

            self.channels['Control'] = control
            self.control = control

            return True

        return False

    @staticmethod
    def sendRequest(channel, request):
        """Sends the request over SSL.

        """
        pass

    @staticmethod
    def sendEvent(channel, event):
        """Sends the event over SSL.

        """
        pass

    def removeChannel(self, name):
        try:
            del self.channels[name]

        except KeyError:
            pass

    def createNewChannel(self, name, port, reliable=False, ordered=False, encryption=None):
        """Creates a new channel from the given parameters.

        """
        pass

class Channel(object):
    def __init__(self, name):
        self.name = name
        self.host = None
        self.port = None
        self.key = None
        self.reliable = None
        self.ordered = None
        self.encryption = None

    def connect(self, host, port, reliable=False, ordered=True, encryption=None, key=None):
        succeeded = False

        if reliable:
            # We're doing one of the TCP connections.
            if encryption == EncryptionType.SSL:
                succeeded = self.connectSSL(host, port)

            else:
                succeeded = self.connectTCP(host, port, key)

        elif not ordered:
            # We're doing UDP.
            succeeded = self.connectUDP(host, port, key)

        else:
            # We don't support unreliable ordered!
            print "Unsupported connection type!"

        if succeeded is not None and succeeded:
            # Successfully connected. Now set variables:
            self.encryption = encryption
            self.reliable = reliable
            self.ordered = ordered
            self.key = key
            self.host = host
            self.port = port

            return True

        return False

    def connectSSL(self, host, port):
        pass

    def connectTCP(self, host, port, key=None):
        pass

    def connectUDP(self, host, port, key=None):
        pass

    def sendRequest(self, request):
        """Send the request over the channel.

        """
        pass

    def sendEvent(self, event):
        """Send the event over the channel.

        """
        pass

    def receiveRequest(self, request):
        """Receive the request over the channel.

        """
        pass

    def receiveEvent(self, event):
        """Receive the event over the channel.

        """
        pass