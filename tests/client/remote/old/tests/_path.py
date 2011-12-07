import os
import sys


def setPath(filename):
    up = os.path.dirname
    mainModule = os.path.abspath(filename)

    # Assuming sys.argv[0] is one of the test modules, up(mainModule) is the `tests` directory, up(up(mainModule)) is
    # the `remote` package's directory, and up(up(up(mainModule))) is the directory containing `remote`.
    remoteRoot = up(up(up(mainModule)))

    # Add the parent directory of the 'remote' module to the start of the path.
    sys.path.insert(0, remoteRoot)

    # Add the 'compat' directory to the end of the path.
    sys.path.append(os.path.join(remoteRoot, 'compat'))
