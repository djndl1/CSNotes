# Background knowledge

UTC is based on atom clock and UT based on astronomical observations. Since the earth does not rotate uniformly, a leap second is introduced to keep UTC and UT close within 0.9 second.

## ISO 8601

An international standard covering the exchange of date- and time-related data.

ISO 8601 applies to representations and formats of dates in the Gregorian calendar and potentially proleptic Gregorian, of time based on the 24-hour timekeeping system with optional UTC offset, of time intervals, and combinations thereof. The standard does not assign any specific meaning to elements of the date/time to be represented; the meaning will depend on the context of its use.

[ISO 8601 on Wikipedia](https://en.wikipedia.org/wiki/ISO_8601). Read the `General Principles`, `Dates`, `Times`. The External links has links to the draft of the standard.

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
          // System.out.printf("%3d", date.getDayOfMonth());
          // if (date.getDayOfMonth() == today)
          //     System.out.print("*");
          // else
          //     System.out.print(" ");
          // date = date.plusDays
              (1);
          if (date.getDayOfWeek().getValue() == 1)
              System.out.println();
      }
      if (date.getDayOfWeek().getValue() != 1)
          System.out.println();
  }
}
```
