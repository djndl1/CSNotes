#include <windows.h>

#include <stdio.h>
#include <tchar.h>
/**
 * Obtain the error number and convert it to the system error message
 * Display this and user message to the stderr.
 */
VOID ReportError(LPCTSTR userMessage, DWORD exitCode, BOOL printErrorMessage)
{
        DWORD MsgLen, errNum = GetLastError();
        LPTSTR SysMsg;

        _ftprintf(stderr, TEXT("%s\n"), userMessage);
        if (printErrorMessage) {
                MsgLen = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                                       FORMAT_MESSAGE_FROM_SYSTEM,
                                       NULL, errNum, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR) &SysMsg, 0, NULL);

                if (MsgLen > 0) {
                        _ftprintf(stderr, TEXT("%s\n"), SysMsg);
                }
                else {
                        _ftprintf(stderr, TEXT("Last Error Number: %d\n"), errNum);
                }
                if (SysMsg != NULL)
                        LocalFree(SysMsg);
        }

        if (exitCode > 0)
                ExitProcess(exitCode);
        return;
}
