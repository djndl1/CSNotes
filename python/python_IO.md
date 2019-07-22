# IO core tools for working with streams

The `io` module provides Python's main facilities for dealing with various types of I/O. Three abstract generic types are avaialbe

- text I/O

- binary I/O

- raw I/O

A concrete object belonging to any of these categories is called a _file object_/_file-like object_/_stream_. Streams have read/write capabilities and random-access/sequntial-access distinction. All streams are type-aware.

Text I/O expects and produces `str` objects. Binary I/O (also called buffered I/O) expects bytes-like objects and produces `bytes` objects. Raw I/O (also called unbuffered I/O) is generally used as a low-level building block for binary and text streams.

# In-memory streams

It is possible to use a `str` or `bytes-like` object as a file for both reading and writing. `StringIO` for strings and `BytesIO` for binary objects. Both provide full read-write capabilities with random access.

# Class hierarchy

```python

        |- RawIOBase - FileIO
        |
        |
IOBase -|- BufferedIOBase - BufferedWriter, BufferedReader, BufferedRWPair, BytesIO, BufferedRandom
        |
        |- TextIOBase - TextIOWrapper, StringIO, 
```

## class `io.IOBase`

The abstact base class for all I/O classes, acting on streams of bytes, providing empty abstract implementations for many methods that derived classes can override selectively.

IOBase supports the iterator protocol and is also a context manager.

- `tell()`: return the current stream position.

### class `io.RawIOBase`

Raw binary I/O typically provides low-level access to an underlying OS device or API and does not try to encapsulate it in high-level primitives.

#### class `io.FileIO`

an OS-level file containing bytes data.

### class `io.BufferedIOBase`

The main difference with `RawIOBase` is that methods of read-write read as much input as requiest or to consume all given output, making perhaps more than one system call. A typical `BufferedIOBase` implementation should not inherit from a `RawIOBase` but wrap one.

- `.raw`: the underlying `RawIOBase` stream it deals with, may not be present in some implementation.

- `.detach()`: separate the underlying raw stream from the buffer and return it
 
#### class `io.BytesIO`
 
 a stream implementation using an in-memory bytes buffer.
 
#### class `io.BufferedReader`
 
 A buffer providing high-level access to a readable, sequential `RawIOBase` object.
 
 - `peek()`: return bytes from the stream without advancing the position.
 
#### class `io.BufferedWriter`
 
A buffer providing higher-level access to a writeable, sequential RawIOBase object. 

The buffer will be written out to the underlying `RawIOBase` when

1. buffer too small

2. `flush()`

3. `seek()`

4. `BufferedWriter` closed or destroyed

#### class `io.BufferedRandom`

buffered interface to random access streams. It is capable of anything `BufferedReader` and `BufferedWriter` can do.

#### class `io.BufferedRWPair`

Never pass the same object as reader and writer.
 
### class `io.TextIOBase`

Base class for text streams. Provides a character and line based interface to stream I/O.

#### class `io.TextIOWrapper`

A buffered text stream over a `BufferedIOBase` binary stream. There's a `reconfigure()` method that sets the text stream with new settings.

#### class `io.StringIO`

An in-memory stream for text I/O.

# Built-in `open` function
Python has a built-in function `open`. It accepts a path to a file or an integer file descritpor of the file. It basically has the same modes as the C standard library function `fopen` and the default mode is `r`/`rt`. The `encoding` argument defaults to the platform unless specified and is only used in text mode.

Python distinguishes binary mode, which returns `bytes` objects without any decoding, and text mode, which returns `str` after decoding.

The default buffer policy deterimines the buffer size using a heuristic to find the underlying device's block size and falling back on `io.DEFAULT_BUFFER_SIZE`, typically 4096 or 8192 bytes. For interactive text files, it uses line buffering. `0` means buffer off. `1` means line buffering. Any other positive integer means a fixed size chunk buffer.

`errors` argument is an optional string that specifies how encoding and decoding errors are to be handled, only in text mode. `newline` determines how to deal with newline characters when reading and writing. The default accepts all three newlines as a newline character when reading and writes the system default line separator. `closefd` specifies whether to close the file descriptor when the file is closed. The default  `opener` is similar to `os.open`.
