# Signals

Signals are software interrupts. Signals provide a way of handling asynchronous events.

Read `man 7 signal`.

## Concepts

Every signal has a name beginning with `SIG`. Signal names are defined by positive integer constants. Terminal, hardware exceptions, `kill` function, `kill` command or software conditions can generate signals.

We can tell the kernel to set the _disposition_ of the signal.

- ignore the signal: `SIGKILL` and `SIGSTOP` cannot be ignored

- catch the signal: call a function when the signal occurs

- let the default action apply: every signal has a default action.

## `signal()`

The semantics of `signal` differ among implementations, we must use `sigaction` instead.

When a program is executed, the status of all signal is either default or ignore. Normally, all signals are set to their default action unless the processthat calls `exec` is ignoring the signal. When a process calls `fork`, the child inherits the parent's signal dispositions.

## Unreliable Signals

TODO

## Reentrant Functions

The SUS specifies the functions that are guaranteed to be safe to call from within a signal handler. These functions are reentrant and are called _async-signal safe_. They block any signals during operation if delivery of a signal might causes inconsistencies.

As a general rule, when calling the reentrant async-signal safe functions from a signal handler, we should save and restore `errno`.

## Reliable Signal 

A signal is _generated_ for a process (or sent to a process) when the event that causes the signal occurs. It is _delivered_ to a process when the action for a signal is taken. Between signal generation and delivery, the signal is said to be _pending_. A process has the option of _blocking_ the delivery of a signal. If a blocked signal's action is not ignore, it's pending until the process either unblocks the signal or changes the action to ignore the signal. `sigpending` determines which signals are blocked and pending. If the system delivers the signal more than once, the signals are said to be queued.

Each process has _signal mask_ that defines the set of signals currently blocked from delivery to that process.

## Sending a Signal

`kill()` sends a signal to a process or a group of processes. `raise` allows a process to send a signal to itself. `alarm` sets a timer and generates a `SIGALRM` after the timeout. The default disposition is to terminate the program. `pause` suspends the calling process until a signal is caught.

TODO
