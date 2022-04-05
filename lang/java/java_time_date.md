# Background knowledge

The `History` section of [this Wikipedia article](https://en.wikipedia.org/wiki/Leap_second) has some brief introduction to time measurement. [This article](https://en.wikipedia.org/wiki/Standard_time) introduced why standard time is needed, of which, the [GMT](https://en.wikipedia.org/wiki/Greenwich_Mean_Time) is a important case.

Modern civil time is generally standard time in a time zone at a fixed offset from UTC or from GMT, possibly adjusted by daylight saving time during part of the year. Traditionally, civil time was mean solar time reckoned from midnight.

## International Atomic Time

A high-precision coordinate time standard based on the notional passage of proper time of Earth's geoid. The basis for UTC.

## Universal Time
 
Universal Time is a time standard based on Earth's rotation, a modern continuation of Greenwich Mean Time (GMT), the mean solar time on the Prime Meridian at Greenwich, England. There are several versions of Universal Time, of which the most commonly used are Coordinated Universal Time (UTC) and UT1. All these versions of UT except for UTC, are based on Earth's rotation relative to distant celestial objects, with a scaling factor and other adjustments to make them closer to solar time. UTC is based on International Atomic Time, with leap seconds added to keep within 0.9 second of UT1. 

A leap second is a one-second adjustment that is occasionally applied to civil time to keep UTC close the mean solar time at 0 Meridian, to accommodate irregularities and slowdown in the Earth's rotation. Because the Earth's rotation speed varies in response to climatic geological events, UTC leap seconds are spaced and unpredictable. Not all clocks implement leap seconds in the same manner as UTC. Leap seconds in Unix time are commonly implemented by repeating the last second of the day. Network Time Protocol freezes time during the leap second. Other experimental schemes smear time in the vicinity of a leap second.

## Main Versions

UT1: the principal form of UT, computed from observations of distant quasars.

UTC: an atomic timescale that approximates UT1.


## ISO 8601

An international standard covering the exchange of date- and time-related data.

ISO 8601 applies to representations and formats of dates in the Gregorian calendar and potentially proleptic Gregorian, of time based on the 24-hour timekeeping system with optional UTC offset, of time intervals, and combinations thereof. The standard does not assign any specific meaning to elements of the date/time to be represented; the meaning will depend on the context of its use.

### [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339) as a simplified version for us on web.

Some takeaways

- DST are hard to predict and determine, thus local offsets are used.

- To ensure interoperability, devices that has no notion of UTC or timezone should either otain the time in UTC, use another host in the same local time zone as a bridge, or prompt the user for such settings.

### Principles

- Ordered from largest to smallest, with the lexicographical order the same to chronological order except for negative years or time offset..

- fixed number of digits are padded with zeros.

- hyphen between date components and colon between time components.

- reduced precision by dropping the less significant components is allowed.

- allow a decimal fraction to the smallest time value

### Date

`YYYY-MM-DD` (extended), `YYYY-MM`, `YYYYMMDD` (basic)

- Year: `YYYY` or `+YYYYY` `-YYYYY`

- Month: `MM` 01-12

- Week `Www` e.g. `2009-W53-7` 01-53

- Ordinal date `YYYY-DDD` e.g. `1981-095`: the 95th day of 1981

### Time

- `T[hh][mm][ss]` (basic), `T[hh]:[mm]:ss` where hh between 00 and 23, mm between 00 and 59, ss between 00 and 60 (the leap second)

- `T` may be omitted in extended format.

- `[ss]`, `[mm][ss]` may be omitted for reduced precision.

### Timezone

- Local time: no UTC relation information

- UTC: `T09:30Z`

- Time offsets from UTC: `+03:00`, `-0300`, `-03`

### DateTime

- `T` is not allowed to be removed now.

### Duration

- `P[n]Y[n]M[n]DT[n]H[n]M[n]S`, `P[n]W`: where `P` denotes duration. zero components can be omitted.

- `PYYYYMMDDThhmmss`, `P[YYYY]-[MM]-[DD]T[hh]:[mm]:[ss]`

### Time Intervals

Between two time points.

- combined date

- start and duration

- duration and end

- duration only

## Time zone

A time zone is a region of the globe that observes a uniform standard time for legal, commercial, and social purposes. Time zones tend to follow boundaries of countries and subdivisions because it is convenient for areas in close commercial or other communication to keep the same time.

Some higher latitude and temperate zone countries use daylight saving time for part of the year, typically by adjusting local clock time by an hour.

`09:30 UTC` is often represented as `09:30Z` or `0930Z`.UTC time is also called "Zulu" time, after the letter "Z". Offset from UTC is also used to denote time zone (with `+` to the east, and `-` to the west). The offset from UTC changes with daylight saving time. Time zones are often represented by alphabetic abbreviations such as "EST", "WST" and "CST".

The __tz database__ (tzdata, the zoneinfo database, or IANA time zone database)is a collaborative compilation of information about the world's time zones, primarily intended for use with computer programs and operating systems. The database attempts to record historical time zones and all civil changes since the Unix time epoch. It also includes transformation such as daylight saving time and also records leap seconds. The databse is published as a set of text files which list the rules and zone transitions in a human-readable format.

## daylight saving time/daylight time/summer time

Daylight saving time is the practice of advancing clocks during summer months so that evening daylight lasts longer, while sacrificing normal sunrise times. Typically, regions that use daylight saving time adjust clocks forward one hour close to the start of spring and adjust them backward in the autumn.

# The Legacy API

## Class `java.util.Date`

- Weird representations of years (since 1900) and months (zero indexed)

- mutable, requires a clone

- a DateTime not a pure Date, nor a pure timestamp.

- Not many functionalities

the class `Date` represents a specific instant in time, with millisecond precision. The `Date` class may not conform to UTC since most computer clocks are not accurate enough to be able to reflect the leap-second distinction.

## Class `java.util.Calendar`

A bridge between time instant and human calendars but has a DateTime component

Tries to solve the issues of `java.util.Date` but has both a human calendar and a timestamp.

- Mutable

- Still zero-based representation of Months

- A civil-time calendar but also a millisecond timestamp

The most used is `GregorianCalendar` (a proleptic Julian-Gregorian calendar). The user is supposed to create a `Calendar` and `getTime()` just to get a `Date`.

# Java 8 `java.time` Package

https://londonjavacommunity.co.uk/about-jsr-310-a-new-java-datetime-api/

Mostly value-based, immutable and thread-safe.

## Java Time-scale

- 86400 seconds a day without leap second (though the definition of second is not the same as the SI second).

- Segmented along the timeline (UTC without leap seconds after 1972-11-03 and UT1 before that).

- Exactly matches the official time at noon each day.

- Closely matches it elsewhere in a precisely defined day.

The `Clock` implementation uses this timescale to provide the current time. The following classes are more or less based on this timescale and can be converted between each other.

## `Temporal`

Do not use zoned time unless really needs absolute time instances.

### `Instant`

Nanoseconds-precision Unix timestamp. The maximum/minimum years representable are 10 billions years AC/CE.

### `LocalTime`

Nanosecond-precision ISO-8601 time without a timezone. A description of the local time on a wall clock.

### `LocalDate`

A date without a time-zone in the ISO-8601 calendar system. An immutable date-time object that represents a date, often viewed as year-month-day.

### `Year`

### `YearMonth`

### `MonthDay` `TemporalAccessor` but not `Temporal`

## `TemporalAmount`

### `Period`

A date-based amount of time interval. e.g. `P2Y3M4D`


### `Duration`

nanosecond-precision time interval.

- supports basic arithmetic operations.

## Enums

Sane representations of months, years, days

### `Month`
