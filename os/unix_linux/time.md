# Overview

The basic time service provided by UNIX counts the number of seconds that have passed since the Epoch, represented by `time_t`, called calendar time. Another kind of time is process time, represented by `clock_t`. UNIX maintains three values for a process:

- clock time, or wall clock time: the amount of time the process takes to run

- user CPU time

- System CPU time

The `time()` function returns the current calendar time.

The `clock_gettime` function can be used to get the time of the specified clock. When the clock ID is set to `CLOCK_REALTIME`, the `clock_gettime` provides similar functionality to the `time` function. `clock_getres` determine the resolution of a given system clock. Call the `clock_settime` function to set the time for a particular clock with the appropriate prviledges.

`gettimeofday` provides greater resoloution than the `time` function.

# Conversion

Once we have the number of seconds since the Epoch, we can call a function to convert it to a broken-down time structure.

```c
struct tm {
    int tm_sec;    /* Seconds (0-60) */
    int tm_min;    /* Minutes (0-59) */
    int tm_hour;   /* Hours (0-23) */
    int tm_mday;   /* Day of the month (1-31) */
    int tm_mon;    /* Month (0-11) */
    int tm_year;   /* Year - 1900 */
    int tm_wday;   /* Day of the week (0-6, Sunday = 0) */
    int tm_yday;   /* Day in the year (0-365, 1 Jan = 0) */
    int tm_isdst;  /* Daylight saving time */
};
```

Note that most of them are 0-based.

`gmtime` and `localtime` converts a calendar time to a broken-down time. `mktime()` converts a broken-down time back into a `time_t` value. 

`strftime` and `strftime_l` prints a formatted time string to a char array. `strptime` is the inverse of `strftime`. It takes a string and converts it into a broken-time time.

`localtime`, `mktime` and `strftime` are affected by the environment variable `TZ`.
