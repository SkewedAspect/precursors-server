#!/usr/bin/env python

from __future__ import print_function

import os
import subprocess
import sys


# This assumes this script lives in precursors-server/tests/client.
scriptDir = os.path.dirname(os.path.abspath(sys.argv[0]))

# Determine the directory with the source .proto files.
protoSourceDir = os.path.join(
        os.path.dirname(os.path.dirname(scriptDir)),
        "proto_src"
        )

# Determine the directory we should write the generated .py files to.
protoDestDir = os.path.join(scriptDir, "remote/protos")

# Find all matching source files.
sourceProtos = list(filter(
        lambda p: p.endswith(".proto"),
        os.listdir(protoSourceDir)
        ))

# Display what we've found.
print("\033[1;33mProtobuf sources in \033[0;33m%s\033[1;33m:\033[m %s"
        % (protoSourceDir, ", ".join(sourceProtos)))
print("\033[1;33mGenerated files will be written to:\033[m", protoDestDir)

# Ensure the target directory exists
if not os.path.isdir(protoDestDir):
    os.makedirs(protoDestDir)

# Generate the .py files.
command = [
        "protoc",
        "--proto_path=" + protoSourceDir,
        "--python_out=" + protoDestDir,
        ] + list(map(
            lambda f: os.path.join(protoSourceDir, f),
            sourceProtos
            ))

retval = subprocess.call(command)

if retval == 0:
    print("\033[1;32mFinished successfully.\033[m")
else:
    print("\033[1;31mFinished with error: %d\033[m" % (retval, ))
