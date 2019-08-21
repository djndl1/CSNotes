# Signals

Signals are software interrupts. Signals provide a way of handling asynchronous events.

## Concepts

Every signal has a name beginning with `SIG`. Signal names are defined by positive integer constants. Terminal, hardware exceptions, `kill` function, `kill` command or software conditions can generate signals.

We can tell the kernel to set the _disposition_ of the signal.

- ignore the signal: `SIGKILL` and `SIGSTOP` cannot be ignored

- catch the signal: call a function when the signal occurs

- let the default action apply: every signal has a default action.
