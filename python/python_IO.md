# Files 

# Built-in `open` function
Python has a built-in function `open`. It accepts a path to a file or an integer file descritpor of the file. It basically has the modes as the C standard library function `fopen` and the default mode is `r`/`rt`. The `encoding` argument defaults to the platform unless specified and is only used in text mode.

Python distinguishes binary mode, which returns `bytes` objects without any decoding, and text mode, which returns `str` after decoding.

The default buffer policy deterimines the buffer size using a heuristic to find the underlying device's block size and falling back on `io.DEFAULT_BUFFER_SIZE`, typically 4096 or 8192 bytes. For interactive text files, it uses line buffering. `0` means buffer off. `1` means line buffering. Any other positive integer means a fixed size chunk buffer.

`errors` argument is an optional string that specifies how encoding and decoding errors are to be handled, only in text mode. `newline` determines how to deal with newline characters when reading and writing. The default accepts all three newlines as a newline character when reading and writes the system default line separator. `closefd` specifies whether to close the file descriptor when the file is closed. The default  `opener` is similar to `os.open`.
