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

        control = Channel('Control')
        if control.connect(host, port, True, 'SSL'):

            # We've successfully connected
            self.host = host

            self.channels['Control'] = control
            self.control = control

            return True

        return False

class Channel(object):
    def __init__(self, name, host=None, port=None, reliable=False):
        self.encryption = None
        self.name = name
        self.host = host
        self.port = port
        self.reliable = False

    def connect(self, host, port, reliable=False, encryption=None):

        # TODO: Actually connect to something

        # Successfully connected. Now set variables:
        self.encryption = encryption
        self.reliable = reliable
        self.host = host
        self.port = port

        return True