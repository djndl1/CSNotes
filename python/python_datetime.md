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


The datetime module supplies classes for manipulating dates and times in both simple and complex ways.

There are two kinds of date and time objects: “naive” and “aware”. An aware object has sufficient knowledge of applicable algorithmic and political time adjustments, such as time zone and daylight saving time information, to locate itself relative to other aware objects. A naive object does not contain enough information to unambiguously locate itself relative to other date/time objects. Such information is purely up to the program to interpret. 

Objects of `date` type are always naive. An object of type `time` or `datetime` may be naive or aware depending on whether it has a proper `tzinfo` object.

## class `date` 

Several useful factory methods:

- classmethod `.today()` return the current local date. Equivalent to `date.fromtimestamp(time.time())`

- classmethod `.fromtimestamp()`: return the local date corresponding to the POSIX timestamp

- classmethod `.fromordinal()`: the date corresponding the proleptic Gregorian ordinal

- classmethod `.fromisoformat()`: return a `date` corresponding to a date string.
