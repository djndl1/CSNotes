/* Chapter 1. Basic cp file copy program. Win32 Implementation. */
/* cpW file1 file2: Copy file1 to file2. */

#include <windows.h>
#include <stdio.h>
#include <wchar.h>

#define BUF_SIZE 16384  /* Optimal in several experiments. Small values such as 256 give very bad performance */

int main(int argc, LPTSTR argv [])
{

	HANDLE hIn, hOut;
	DWORD nIn, nOut;
	CHAR buffer [BUF_SIZE];

	if (argc != 3) {
		fwprintf (stderr, L"用法: cp 文件1 文件2\n");
		return 1;
	}
	hIn = CreateFile (argv[1], GENERIC_READ, FILE_SHARE_READ, NULL,
			OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hIn == INVALID_HANDLE_VALUE) {
		fprintf (stderr, "Cannot open input file. 错误: %x\n", GetLastError ());
		return 2;
	}

	hOut = CreateFile (argv[2], GENERIC_WRITE, 0, NULL,
			CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hOut == INVALID_HANDLE_VALUE) {
		fprintf (stderr, "Cannot open output file. 错误: %x\n", GetLastError ());
		CloseHandle(hIn);
		return 3;
	}
	while (ReadFile (hIn, buffer, BUF_SIZE, &nIn, NULL) && nIn > 0) {
		WriteFile (hOut, buffer, nIn, &nOut, NULL);
		if (nIn != nOut) {
			fprintf (stderr, "Fatal write error: %x\n", GetLastError ());
			CloseHandle(hIn); CloseHandle(hOut);
			return 4;
		}
	}
	CloseHandle (hIn);
	CloseHandle (hOut);
	return 0;
}
