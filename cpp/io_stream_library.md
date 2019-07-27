# Overview

The intent was to create architecture independent I/O by the standard of ANSI/ISO.

Most C++ I/O classes have names starting with `basic_`. However, most of them are typedefed into other names. I/O facilities were developed using the template mechnaism alowing for easy conversions to character types other than the traditional `char` type.

Iostream objects cannot be declared using standard forward declarations, instead, the `<iosfwd>` header file should be included.

## Architecture

```
                         +----------+
                         | ios_base |
                         +-----|----+
                            +--|--+
                            | ios +----> rdbuf() +------> streambuf*
                 +----------|-----|----------+
                 |                           |
           +-----|-----+              +------|----+
           |           |  streambuf*  |           |
           |  istream  <--+filebuf*+-->  ostream  +----------+
           |           |  stringbuf*  |           |          |
     +-----|--|--------++             +---|-------+          |
     |        |         |                 |       |          |
     |        |         |                 |       |          |
+----|------+ | +-------|------+     +----|-----+ | +--------|--------+
| ifstream  | | | istringstream|     | ofstream | | | ostringstream   |
+-----------+ | +--------------+     +----------+ | +-----------------+
              |          +-------------+          |
              +----------+  iostream   +----------+
                         +-------|-----+
                            +----|----+
                            | fstream |
                            +---------+

```

- `class ios_base` defines the core of all I/O operations and offers facilities for inspecting the state of I/O streams and for output formatting.

- `ios` implements the communication with a buffer used by streams, which is a `streambuf` object, responsible for the actual I/O to/form the underlying device. `iostream` objects do not perform I/O operations themselves but leave these the buffer with which they are associated.

- It is possible read/write information into memory buffers instead of files, for which the `istreamstream`/`ostringstream` is used.

- Formating is to a great extent possible using the facilities defined in the `ios` class, but also possbile to insert formatting commands directly into streams using manipulators.

- Stream objects are the interface between the objects to be input or or output and the `streambuf` which is responsible for the actual I/O to the device. This allows to construct a new kind of `streambuf` for a new kind of device and use it in combination with `istream`- and `ostream`-class facilities. iostream objects perform formatting roles. Interfacing to new decivecs requires the construction of a new kind of `streambuf` rather than a new kind of stream object. A wrapper class around stream classes eases the access to a special device, which is how the `stringstream` classes were constructed.

# Class `ios_base`

The class `std::ios_base` forms the foundation of all I/O operations, and defines, among other things, facilities for inspecting the state of I/O streams and most output formatting facilities.

## class `ios`

The purpose of the class `ios` is to provide the facilities of the class `basic_ios`, and to add several new facilites, all related to
the `streambuf` object which is managed by objects of the class `ios`.

- `std::sterambuf *ios::rdbuf()`: returnsiop a nter the `streambuf` object forming the interface between the `ios` object and the device with which the `ios` communicates.

Operations on streams may fail for various reasons. Whenever an operation fails, further operations on the stream are suspended. It is possible to inspect, set and possbily clear the condition state of streams, allowing a program to repair the problem rather than having to abort.

- `ios::badbit`: illegal reqeust at the level of the `streambuf`

- `ios::eofbit`: end of file

- `ios::failbit`: an operation by the `streambuf` object has failed, in which case, further such operations no longer work.

- `ios::goodbit`: if none of the three above 
