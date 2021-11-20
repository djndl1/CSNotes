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

`GetLastError` (set by `SetLastError`) rather than `errno` ensures that system errors are unique to the threads. `FormatMessage` turns the message number into a meaning message.

A Windows process has three standard devices:

```c
HANDLE WINAPI GetStdHandle(
  _In_ DWORD nStdHandle
);

// for redirection
BOOL WINAPI SetStdHandle(
  _In_ DWORD  nStdHandle,
  _In_ HANDLE hHandle
);
```

There are two reserved pathnames `CONIN$` and `CONOUT$` for console input and output. Use `CreateFile` on them.

# File Management

```c
BOOL DeleteFile(
  LPCSTR lpFileName
);
```

# Console and unicode

http://archives.miloush.net/michkap/archive/2010/10/07/10072032.html

http://illegalargumentexception.blogspot.com/2009/04/i18n-unicode-at-windows-command-prompt.html

http://illegalargumentexception.blogspot.com/2009/04/java-unicode-on-windows-command-line.html
