securemem
=========

Securemem provides memory chunks that allow auto-scrubbing of the memory after use,
and constant time equality.

Documentation: [securemem on hackage](http://hackage.haskell.org/package/securemem)

Interacting with securemem
--------------------------

It's recommended to use the [Byteable instance](http://hackage.haskell.org/package/byteable)
when providing an interface that takes a securemem. It allow legacy code, and work in progress
code to interface with securemem more easily.

older base
----------

An older base, the memory is not scrubed: upgrade your GHC to 7.6.0 or above.

TODO
----

* add a custom memory allocator that give mlocked memory chunks.
