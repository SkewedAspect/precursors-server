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
        if control.connect(host, port, True, 'SSL'):

            # We've successfully connected
            self.host = host

            self.channels['Control'] = control
            self.control = control

            return True

        return False

    def sendRequest(self, request):
        """Sends the request over SSL.

        """
        pass

    def sendEvent(self, event):
        """Sends the event over SSL.

        """
        pass

    def removeChannel(self, name):
        try:
            del self.channels[name]

        except KeyError:
            pass

class Channel(object):
    def __init__(self, name):
        self.name = name
        self.host = None
        self.port = None
        self.key = None
        self.reliable = None
        self.encryption = None

    def connect(self, host, port, reliable=False, encryption=None, key=None):

        # TODO: Actually connect to something

        # Successfully connected. Now set variables:
        self.encryption = encryption
        self.reliable = reliable
        self.key = key
        self.host = host
        self.port = port

        return True

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