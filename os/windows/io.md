# [File Naming](https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file)

Full naming or Universal Naming Convention starts with a double backslash `\\` indicating the global root. `/` also works in low-level API but better use `/`. File and directory names used as API has a length limit of 255.

> it is acceptable to specify a period as the first character of a name. For example, ".temp".

Windows may also create a short 8.3 alias form of name for a file. Windows stores the long file names on disk in Unicode.

A file name is not relative if:

1. UNC `\\`

2. starts with a disk designator `C:\`. `C:tmp.txt` is a relative path on `C:\`.

3. a single backslash

```c
#include <windows.h>

#include <stdio.h>

int main(int argc, char *argv[])
{
        printf("Long path %d\n", MAX_PATH);
        return 0;
}
// returns Long path 260, use \\?\ to specify a path up to 32767 characters long,  \\?\C:\ and \\?\UNC\, which actually turns off automatic expansion of the path string.
```

## _namespaces_


NT namespaces and Win32 namespaces. The NT namespace was designed to be the lowest level namespace on which other subsystem and namespaces could exist.

The `\\.\` prefix is used to access the Win32 Device Namespaces.

Under the NT namespace, `Global??` is the Win32 namespace.

# Opening, Reading, Writing and Closing

[CreateFile](https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew)

[CloseHandle](https://docs.microsoft.com/en-us/windows/win32/api/handleapi/nf-handleapi-closehandle): close nearly all handle objects.

[ReadFile](https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile): 

[WriteFile](https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefile):

## Generic Characters for handling Unicode

```c
TCHAR
LPTSTR
LPCTSTR
```

Define `UNICODE` and `_UNICODE_` before `<windows.h>`to get unicode support.

Use the collection of generic C library string and character I/O functions in `<tchar.h>`.
