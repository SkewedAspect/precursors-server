class Client(object):
    aeskey = None
    oldkey = None
    channels = dict()
    host = None

    def __init__(self):
        pass

    def connect(self, host, port):

        control = Channel('Control')
        if control.connect(host, port, True, 'SSL'):

            # We've successfully connected
            self.host = host

            self.channels['Control'] = control

            return True

        return False

class Channel(object):
    def __init__(self, name):
        self.encryption = None
        self.name = name
        self.host = None
        self.port = None
        self.reliable = False

    def connect(self, host, port, reliable=False, encryption=None):

        # TODO: Actually connect to something

        # Successfully connected. Now set variables:
        self.encryption = encryption
        self.reliable = reliable
        self.host = host
        self.port = port

        return True