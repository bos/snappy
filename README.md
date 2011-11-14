# Welcome to snappy

snappy is a fast Haskell library for working with data compressed
using Google's Snappy format.  It is implemented as a binding to the
[Snappy library](http://code.google.com/p/snappy/).

It implements zero-copy compression and decompression of both strict
and lazy [bytestring](http://hackage.haskell.org/package/bytestring)s,
the standard Haskell types for managing binary data efficiently.

# Join in!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[github issue tracker](http://github.com/bos/snappy/issues).

Master [git repository](http://github.com/bos/snappy):

* `git clone git://github.com/bos/snappy.git`

There's also a [Mercurial mirror](http://bitbucket.org/bos/snappy):

* `hg clone http://bitbucket.org/bos/snappy`

(You can create and contribute changes using either git or Mercurial.)

Authors
-------

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>.
