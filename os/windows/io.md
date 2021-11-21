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

https://alfps.wordpress.com/2011/11/22/unicode-part-1-windows-console-io-approaches/

https://devblogs.microsoft.com/cppblog/new-options-for-managing-character-sets-in-the-microsoft-cc-compiler/

MSVC internally use UTF-8 for string literals. For unprefixed string literals, MSVC would treat them based current on the current code page.

Normal strings are output as they are in execution charset (GCC or MSVC). Execution character sets affect normal byte strings only.

For wide strings, `wprintf` would first convert them based on the set code page and then output them into stdout.

The following code should have the commented standard-conformant result.

```cpp
  setlocale(LC_ALL, "my_system_locale");
	printf("printf-s-1 %s\n", Test1); // Correct
	printf("printf-s-2 %s\n", Test2); // -N

	printf("printf-ls-1 %ls\n", Test1); // empty and no newline
	printf("printf-ls-2 %ls\n", Test2); // correct

  // For MSVC, these two lines should be the same as the last two
	wprintf(L"wprintf-s-1 %s\n", Test1); // correct output
	wprintf(L"wprintf-s-2 %s\n", Test2); // -N

	wprintf(L"wprintf-ls-1 %ls\n", Test1); // garbage output
	wprintf(L"wprintf-ls-2 %ls\n", Test2); // correct
```

GCC-MSVCRT cannot output wide strings in any meaningful way due to a [bug][https://yongweiwu.wordpress.com/2016/05/27/msvcrt-dll-console-io-bug/] in MSVCRT6, with or without `setlocale`, using `%ls` or `%s` on wide strings or normal strings.

```cpp
printf-s-1 ?D??
printf-s-2 -N?e
printf-ls-2
wprintf-s-1 ?D??
wprintf-s-2 -N?e
wprintf-ls-1
wprintf-ls-2
```

However, with some perl redirection, GCC-MSVCRT6 works somewhat better, with or without `setlocale`. Seems MSVCRT6 does some translation when outputing to the console.

```shell
printf-s-1 中文 
printf-s-2 -N噀 
printf-ls-2
wprintf-s-1 中文
wprintf-s-2 -N噀 # actually a UTF-16 "中文"
wprintf-ls-1
wprintf-ls-2
```

Even so, it still has some problems with `wprintf`

 GCC-UCRT shows similar behavior to MSVC in that with `setlocale`, wide strings are output as a converted MBCS and question marks without `setlocale`.
```shell
printf-s-1 中文
printf-s-2 -N噀
printf-ls-2 中文
wprintf-s-1 中文
wprintf-s-2 -N噀
wprintf-ls-1 ??
wprintf-ls-2 中文
```

and MSVC 19.30.30705 gives the following output

```shell
printf-s-1 中文
printf-s-2 -N噀
printf-ls-2 中文
wprintf-s-1 ??
wprintf-s-2 中文
wprintf-ls-1 ??
wprintf-ls-2 中文
```

Without setting the locale, these are the results

- GCC-MSVCRT

```shell
printf-s-1 中文
printf-s-2 -N噀
printf-ls-2
wprintf-s-1 中文
wprintf-s-2 -N噀
wprintf-ls-1
wprintf-ls-2
```

- GCC-UCRT64

```shell
printf-s-1 中文
printf-s-2 -N噀
printf-ls-2
wprintf-s-1 中文
wprintf-s-2 -N噀 
wprintf-ls-1 ??
wprintf-ls-2 ??
```

- MSVC 19.30.30705

```shell
printf-s-1 中文
printf-s-2 -N噀
printf-ls-2 wprintf-s-1 ??
wprintf-s-2 ??
wprintf-ls-1 ??
wprintf-ls-2 ??
```

`std::cout` and `std::wcout` works in a similar way to `printf("%s")` and `wprintf("%ls")`.

