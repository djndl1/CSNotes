# `pickle` module

The pickle module implements binary protocols for serializing and de-serializing a Python object structure. 

The data format used by pickle is Python-specific. By default, the pickle data format uses a relatively compact binary representation. If you need optimal size characteristics, you can efficiently compress pickled data. There are currently 5 different protocols which can be used for pickling.

## API

To serialize an object hierarchy, you simply call the `dumps()` function. Similarly, to de-serialize a data stream, you call the `loads()` function. However, if you want more control over serialization and de-serialization, you can create a `Pickler` or an `Unpickler` object, respectively.

- `pickle.dump()`: Write a pickled representation of obj to the open file object file.

- `pickle.dumps()`: Return the pickled representation of the object as a bytes object, instead of writing it to a file.

- `pickle.load()`: Read a pickled object representation from the open file object file and return the reconstituted object hierarchy specified therein.

- `pickle.loads()`: Read a pickled object hierarchy from a bytes object and return the reconstituted object hierarchy specified therein.

### class `pickle.Pickler`

takes a binary file for writing a pickle data stream.

TODO
'
### class `pickle.Unpickler`

 akes a binary file for reading a pickle data stream.


# `pickletools` module

This module contains various constants relating to the intimate details of the pickle module, some lengthy comments about the implementation, and a few useful functions for analyzing pickled data.

`python -m pickletools` will disassemble the contents of one or more pickle files.

# Module `json`

 If cross-language compatibility is required, one option is `json`. `json` exposes an API familiar to users of the standard library `marshal` and `pickle` modules.
