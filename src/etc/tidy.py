#!/bin/env python

maxlen=70

def report(msg):
    print ("%s:%d: %s" % (fileinput.filename(), fileinput.filelineno(), msg))

import fileinput
for line in fileinput.input():
    if len(line.rstrip("\n")) > maxlen:
        report("overlong line")
    if line.find("\t") != -1:
        report("line with tab")
    if line.rstrip(" \t") != line:
        report("line with trailing whitespace")
