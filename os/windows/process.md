# Process Creation

```c
BOOL CreateProcessW(
  LPCWSTR               lpApplicationName,
  LPWSTR                lpCommandLine,        // non-constant
  LPSECURITY_ATTRIBUTES lpProcessAttributes,
  LPSECURITY_ATTRIBUTES lpThreadAttributes,
  BOOL                  bInheritHandles,
  DWORD                 dwCreationFlags,
  LPVOID                lpEnvironment,
  LPCWSTR               lpCurrentDirectory,
  LPSTARTUPINFOW        lpStartupInfo,
  LPPROCESS_INFORMATION lpProcessInformation
);
```
## Specifying the Executable and the Command Line

non-null `lpApplicationName` specifies the executable path, if `NULL`, the first token in `lpCommandLine` is the program name.

## Inheritable Handles

1. `bInheritHandles`: A master switch applying to all handles.

2. To make an individual handle inheritable, use a =SECURITY_ATTRIBUTES= structure at creation time of the handle or duplicate an existing handle

```c
BOOL DuplicateHandle(
  HANDLE   hSourceProcessHandle,
  HANDLE   hSourceHandle,
  HANDLE   hTargetProcessHandle,
  LPHANDLE lpTargetHandle,
  DWORD    dwDesiredAccess,
  BOOL     bInheritHandle,
  DWORD    dwOptions
);
```

The handle values are communicated to the child using an IPC mechanism or assigning the handle to stdio in the `STARTUPINFO` structure (which is the preferred way to use I/O redirection).
Non-file handles and handles that are not used to redirect stdio can be converted to text and placed in a command line argument or in an environment variable.

The inherited handles are distinct copies, a parent and child might be accessing the same file using different file pointers.

## Process Information

```c
HANDLE GetCurrentProcess(); // a constant (-1) pseudo handle that is not valid for other processes
DWORD GetCurrentProcessId();
```

To get a real process handle, get the PID and `OpenProcess` it.

```c
HANDLE OpenProcess(DWORD dwDesiredAccess, BOOL  bInheritHandle, DWORD dwProcessId);
```

A running process can determine the full pathnames of the exe used to run it with `NULL` as the module handle of these functions.

```c
DWORD GetModuleFileNameW(HMODULE hModule, LPWSTR  lpFilename, DWORD   nSize);
DWORD GetModuleFileNameExW(HANDLE  hProcess, HMODULE hModule, LPWSTR  lpFilename, DWORD   nSize);
```

# Process Termination

```c
void ExitProcess(UINT uExitCode); // DLL detach calls are made, termination handlers are not executed
BOOL GetExitCodeProcess(HANDLE  hProcess, LPDWORD lpExitCode);
BOOL TerminateProcess(HANDLE hProcess, UINT   uExitCode); // not even DllMain
```

# Process Sync

The simplest method to synchronize with another process is to wait for that process to complete. There are two general-purpose wait functions for synchronization objects to become signaled

```c
DWORD WaitForSingleObject(HANDLE hHandle, DWORD  dwMilliseconds);
DWORD WaitForMultipleObjects(DWORD nCount, const HANDLE *lpHandles, BOOL bWaitAll, DWORD dwMilliseconds);
```

# Environment Blocks and Strings

```c
DWORD GetEnvironmentVariable(LPCTSTR lpName, LPTSTR  lpBuffer, DWORD   nSize);
BOOL SetEnvironmentVariable(LPCTSTR lpName, LPCTSTR lpValue);
```

# Process Security

By default, `CreateProcess` gives `PROCESS_ALL_ACCESS` rights. Several other specific rights are available.


# Process Execution Times

```c
BOOL GetProcessTimes(HANDLE hProcess, LPFILETIME lpCreationTime, LPFILETIME lpExitTime, 
                     LPFILETIME lpKernelTime, LPFILETIME lpUserTime);
```

# Job Objects

Job objects represent a set of processes that are controlled together. Resource limits can be specified for all the job member processes and accounting information is maintained.

The typical way is:

1. Create a job (`CreateJobObject`) or open a named object (`OpenJobObject`) to get a job handle;

2. add a process to that job (`AssignProcessToJobObject`);

3. Control limits are specified `SetInformationJobObject`; This can be queried through `QueryJobInformationObject`

By default, a new child process created by a process in the job will also belong to the job unless some certain flag is specified when creating the process.
