# `CreateThread`

`CreateRemoteThread`: dangerous and not useful in normal applications. Might be useful in writing a debugger.

# `ExitThread`

The threads's stack is deallocated and all handles referring to the thread are signaled. the `DllMain` functions of each DLL will be called with `DLL_THREAD_DETACH` as the reason.

`TerminateThread` does not deallocate the thread's resources, completion handlers do not execute and there is no notification to attached DLLs.

The exit code is retrieved by `GetExitCodeThread`

# Thread Identity

- `GetCurrentThread`: noninheritable pseudohandle

- `GetCurrentThreadId`

- `GetThreadId`

- `OpenThread`

- `GetProcessIdOfThread`

- `GetThreadIOPendingFlag`

# Suspending and Resuming

Every thread has a suspend count and a thread can execute only if this count is 0. This can be manipulated by calling `ResumeThread` and `SuspendThread`

# Waiting for Threads to Terminate

`WaitForSingleObject`, `WaitForMultipleObjects` with thread handles.

# The C Library and Thread

A thread-safe C library is provided by MS `libcmt.lib` (`_MT` macro defined) and use the `/MD` option of MSVC, use `_beginthreadex` and `_endthreadex` with it.

# Thread Models

- Boss/worker: the boss thread assigns tasks for the worker threads to perform.

- work crew: the workers cooperate on a single task, each performing a small piece.

- client/server: pipeline model

# Thread Local Storage

1. Use the thread function argument as a TLS structure;

2. When the threads are created, the system allocates an array of `LPVOID` `NULL` values for TLS.

A thread use an index allocated by `TlsAlloc` (which makes an index available for all threads) function to store (`TlsSetValue`) and retrieve (`TlsSetValue`) a value that is local to the thread. The index is then deallocated (`TlsFree`). The pointed memory by the slot should be freed before the index is deallocated.
