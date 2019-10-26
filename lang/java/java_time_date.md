# Background knowledge

The `History` section of [this Wikipedia article](https://en.wikipedia.org/wiki/Leap_second) has some brief introduction to time measurement. [This article](https://en.wikipedia.org/wiki/Standard_time) introduced why standard time is needed, of which, the [GMT](https://en.wikipedia.org/wiki/Greenwich_Mean_Time) is a important case.

Modern civil time is generally standard time in a time zone at a fixed offset from UTC or from GMT, possibly adjusted by daylight saving time during part of the year. Traditionally, civil time was mean solar time reckoned from midnight.

## International Atomic Time

A high-precision coordinate time standard based on the notional passage of proper time of Earth's geoid. The basis for UTC.

## Universal Time
 
Universal Time is a time standard based on Earth's rotation, a modern continuation of Greenwich Mean Time (GMT), the mean solar time on the Prime Meridian at Greenwich, England. There are several versions of Universal Time, of which the most commonly used are Coordinated Universal Time (UTC) and UT1. All these versions of UT except for UTC, are based on Earth's rotation relative to distant celestial objects, with a sclaing factor and other adjustments to make them closer to solar time. UTC is based on International Atomic Time, with leap seconds added to keep within 0.9 second of UT1. 

A leap second is a one-second adjustment that is occasionally applied to civil time to keep UTC close the mean solar time at 0 Meridian, to accomomodate irregularities and slowdown in the Earth's rotation. Because the Earth's rotation speed varies in response to climatic geological events, UTC leap seconds are spaced and unpredictable. Not all clocks implement leap seconds in the same manner as UTC. Leap seconds in Unix time are commonly implemented by repeating the last second of the day. Network Time Protocol freezes time during the leap second. Other experimental schemes smear time in the vicinity of a leap second.

## Main Versions

UT1: the principal form of UT, computed from observations of distant quasars.

UTC: an atomic timescale that approximates UT1.


## ISO 8601

An international standard covering the exchange of date- and time-related data.

ISO 8601 applies to representations and formats of dates in the Gregorian calendar and potentially proleptic Gregorian, of time based on the 24-hour timekeeping system with optional UTC offset, of time intervals, and combinations thereof. The standard does not assign any specific meaning to elements of the date/time to be represented; the meaning will depend on the context of its use.

[ISO 8601 on Wikipedia](https://en.wikipedia.org/wiki/ISO_8601). Read the `General Principles`, `Dates`, `Times`. The External links has links to the draft of the standard.

## Time zone

A time zone is a region of the globe that observes a uniform standard time for legal, commercial, and social purposes. Time zones tend to follow boundaries of countries and subdivisions because it is convenient for areas in close commercial or other communication to keep the same time.

Some higher latitude and temperate zone countries use daylight saving time for part of the year, typically by adjusting local clock time by an hour.

`09:30 UTC` is often represented as `09:30Z` or `0930Z`.UTC time is also called "Zulu" time, after the letter "Z". Offset from UTC is also used to denote time zone (with `+` to the east, and `-` to the west). The offset from UTC changes with daylight saving time. Time zones are often represented by alphabetic abbreviations such as "EST", "WST" and "CST".

The __tz database__ (tzdata, the zoneinfo database, or IANA time zone database)is a collaborative compilation of information about the world's time zones, primarily intended for use with computer programs and operating systems. The database attempts to record historical time zones and all civil changes since the Unix time epoch. It also includes transformation such as daylight saving time and also records leap seconds. The databse is published as a set of text files which list the rules and zone transitions in a human-readable format.

## daylight saving time/daylight time/summer time

Daylight saving time is the practice of advancing clocks during summer months so that evening daylight lasts longer, while sacrificing normal sunrise times. Typically, regions that use daylight saving time adjust clocks forward one hour close to the start of spring and adjust them backward in the autumn.

# Class `java.util.Date`

the class `Date` represents a specific isntant in time, with millisecond precision. The `Date` class may not conform to UTC since most computer clocks are not accurate enough to be able to reflect the leap-second distinction.

Most methods of the this class are deprecated. The `Date` class is not very useful for manipulating the kind of information that humans use for dates.

# Class `java.time.LocalDate`

A date without a time-zone in the ISO-8601 calendar system. An immutable date-time object that represents a date, often viewed as year-month-day. This class does not store or represent a time or time-zone. Instead, it is a description of the date. It cannot represent an instant on the time-line without additional information such as an offset or time-zone.

This class is immutable and thread-safe

A `LocalDate` class is obtained by using static _factory methods_ that call constructors on your behalf.

The following code print the present day in a month.

```java
import java.time.*;

public class CalendarTest {
  public static void main(String[] args) {
      LocalDate date = LocalDate.now();
      int month = date.getMonthValue();
      int today = date.getDayOfMonth();

      date = date.minusDays(today - 1); // set to start of month
      DayOfWeek weekday = date.getDayOfWeek();
      int value = weekday.getValue();

      System.out.println("Mon Tue Wed Thu Fri Sat Sun");
      for (int i = 1; i < value; i++)
          System.out.print("    ");                 // move to the weekday of the start of month
      while (date.getMonthValue() == month) {
          System.out.printf("%3d", date.getDayOfMonth());
          if (date.getDayOfMonth() == today)
              System.out.print("*");
          else
              System.out.print(" ");
          date = date.plusDays
              (1);
          if (date.getDayOfWeek().getValue() == 1)
              System.out.println();
      }
      if (date.getDayOfWeek().getValue() != 1)
          System.out.println();
  }
}
```
