import sys

# Check the python version for 2.7, and import appropriate unittest
# module as required.
if sys.version_info < (2, 3):
    print "WARNING: unittest2 not compatible with python < 2.3!"
    print "Attempting to continue, but tests may be broken."

if sys.version_info < (2, 7):
    try:
        import unittest2
        globals()['unittest'] = unittest2

    except ImportError:
        print "WARNING: Could not find unittest2 on python < 2.7!"
        print "Attempting to continue, but tests may be broken."

if 'unittest' not in globals():
    import unittest

