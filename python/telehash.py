#! /usr/bin/env python

# Released into the public domain by tav <tav@espians.com>

"""TeleHash -- A Distributed JSON Peering Protocol."""

import sys

from optparse import OptionParser

try:
    from hashlib import sha1 as sha
except ImportError:
    from sha import new as sha

try:
    from gevent import socket
except ImportError, e:
    raise RuntimeError("You need to install libevent and easy_install gevent.")

try:
    from simplejson import dumps as encode_json, loads as decode_json
except ImportError:
    try:
        from json import dumps as encode_json, loads as decode_json
    except ImportError:
        raise RuntimeError("You need to easy_install simplejson.")

# We define some constants
MAX_TELEX_BYTESIZE = 1400

SEEDS = [
    ('telehash.org', 42424)
    ]

connections = {}

commands = {}

routes = {}

class Telex:
    pass

class Switch(object):

    def __init__(self, host, port):
        self.port = port
        self.sock = sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        sock.setblocking(0)
        sock.setsockopt(
            socket.SOL_SOCKET, socket.SO_REUSEADDR,
            sock.getsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR) | 1,
            )
        sock.bind((host, port))
        if not host:
            self.who_am_i()
        else:
            self.host = host

    def who_am_i(self):
        pass

def parse_incoming_telex(telex):
    telex = decode_json(telex)
    for key in telex:
        first_char = key[:1]
        if first_char == '.':
            # Command
            pass
        elif first_char == '_':
            # Header
            pass
        elif first_char == '+':
            # Signal
            pass

def main(argv=None):

    argv = argv or sys.argv[1:]
    switch = Switch('', 42424)

if __name__ == '__main__':
    sys.exit(main())
