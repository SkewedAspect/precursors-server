from channel import SelectChannel
from communication import SelectCommunicator
from tcp import TCPChannel
from udp import UDPChannel


channelTypes = [
        TCPChannel,
        UDPChannel,
        ]

# Reference some things so PyFlakes doesn't complain.
SelectChannel
SelectCommunicator
