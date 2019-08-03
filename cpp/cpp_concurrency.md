In C++ the multi-threading implementation offered by the language is a high level interfeace to multithreading, using the raw pthread building blocks is hardly ever necessary.

A _thread of execution_ is a single flow of control within a program. All threads share the process's data and code. Access must be synchronized to avoid that threads read data while these data are being modified by other threads and to avoid that multiple threads modify the same data at the same time.
