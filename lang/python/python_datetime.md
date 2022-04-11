# `datetime` module

```
object
    timedelta       # a duration expressing the difference between two `date`, `time` or `datetime` instances to microsecond resolution.
    tzinfo          # an abstract base class for time zone information.
        timezone    # a class implementing the `tzinfo` abstact base class as a fixed offset from the UTC.
    time            # an idealized time with no leap seconds.
    date            # an idealized naive date
        datetime    # a combination of time and date 
```

DateTime types are of microsecond-precision.

There are two kinds of date and time objects: 

- naive: an aware object has sufficient knowledge of applicable algorithmic and political time adjustments, such as time zone and daylight saving time information, to locate itself relative to other aware objects. 

- aware: a naive object does not contain enough information to unambiguously locate itself relative to other date/time objects. Such information is purely up to the program to interpret. 

`date​`s are always naive. `time` or `datetime` may be naive or aware depending on whether it has a proper `tzinfo` object.

`date`, `datetime`, `time` and `timezone` are 

- immutable

- hashable

- support efficient pickling

- limited support for ISO-8601

## `timedelta`

A duration, the difference between two dates or times.

- supports `+`, `-`, `*`, `/`, `//`, `%`, with `timedelta` and other date-time types

- comparisons are supported

## `date` 

Represents a date (year, month, day)

## `datetime`

microsecond-precision date-time

- No leap second

- `datetime.now(timezone.utc)`/`datetime.fromtimestamp(timestamp, timezone.utc)` is better than `datetime.utcnow()`/`datetime.utcfromtimestamp()` as the latter has no time zone.

- supports `+` (with `timedelta`), `-` (with `datetime` and `timedelta`) and comparison with `datetime`

## `time`

A time of day

## `tzinfo`

### `timezone`

Fixed offset from UTC, doesn't support daylight saving time.
