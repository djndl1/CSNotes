# Every Basic Type

## GQuark

Use: when both integers and string can be used interchangeably, or used for string interning.

The string-to-int mapping is maintained as a hash table `quark_ht`. The reverse is a one-based (since quarks are non-zero, 0 denotes `NULL`) string array `quarks` (`char **` of course, allocated per `QUARK_BLOCK_SIZE`), the `quark_seq_id`  is the index for the next new quark. A mutex is used for thread-safe access to the quark structures. These structures are initialized at load time of glib. Note the old quark string array is leaked every time the new one replaces the old one.

Multiple duplicated strings are stored in string blocks if small enough, rather than scattered around.
