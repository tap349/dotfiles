#!/usr/bin/env python

# Cleans up the output of "react-native log-ios" by
# removing all output not related to your app, similarly
# to how it works in the XCode debug window
#
# Usage: react-native log-ios | ./react-native-only.py "<string>"
#
# where "<string>" is the name of your app as known
# to the iOS logging system. E.g. "SimpleExampleApp"

import sys, os, re

class Transformer:
    def __init__(self):
        self.context = None

    def transform(self, only, l):
        regex = "^\S+\s+\S+\s+\S+\s+\S+\s+(\S+)\[\d+\]\s+(.*)$"

        m = re.match(regex, l)

        if m:
            self.context = m.group(1)
            msg = m.group(2)

            if self.context == only:
                return msg
        else:
            if self.context == only or self.context is None:
                return l

        return None

def main():
    only = sys.argv[1]
    transformer = Transformer()

    while True:
        line = sys.stdin.readline().rstrip()
        if not line:
            break

        result = transformer.transform(only, line)

        if result:
            sys.stdout.write(result + "\n")
            sys.stdout.flush()

try:
    main()
except KeyboardInterrupt:
    sys.exit(0)
