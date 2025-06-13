MODULE time_tests

  USE fortuno_interface_m, ONLY: CHECK, TEST_LIST, SUITE
  USE time_module

  IMPLICIT NONE

CONTAINS

  SUBROUTINE test_calendars()
    !*## Purpose
    !
    ! A series of tests to ensure the calendars are set correctly, and return
    ! the correct values for days in year, month etc.

    INTEGER :: ExpectedDaysYear, Year, m, d
    INTEGER, DIMENSION(:), ALLOCATABLE :: ExpectedDaysMonth, DaysMonth,&
                                          DaysInYear, MonthsFromDay,&
                                          ExpectedMonthsFromDay
    LOGICAL :: Success

    ! Some days of the year to check
    DaysInYear = [25, 60, 150, 360]

    ! Start with Gregorian non-leap year
    CALL set_calendar("gregorian")
    Year = 2001
    ExpectedDaysYear = 365
    ExpectedDaysMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    ExpectedMonthsFromDay = [1, 3, 5, 12]

    ! Run through the various tests
    Success = .not. is_leapyear(Year)
    CALL CHECK(Success)

    Success = (leap_day(Year) == 0)
    CALL CHECK(Success)

    Success = (days_in_year(Year) == ExpectedDaysYear)
    CALL CHECK(Success)

    DaysMonth = [(days_in_month(m, Year), m = 1, 12)]
    Success = (ALL(DaysMonth == ExpectedDaysMonth))
    CALL CHECK(Success)

    MonthsFromDay = [(month_from_day(DaysInYear(d), Year), d = 1, 4)]
    Success = (ALL(MonthsFromDay == ExpectedMonthsFromDay))
    CALL CHECK(Success)

    ! Now try a leap year
    Year = 2004
    ExpectedDaysYear = 366
    ExpectedDaysMonth = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    ExpectedMonthsFromDay = [1, 2, 5, 12]

    ! Run through the various tests
    Success = is_leapyear(Year)
    CALL CHECK(Success)

    Success = (leap_day(Year) == 1)
    CALL CHECK(Success)

    Success = (days_in_year(Year) == ExpectedDaysYear)
    CALL CHECK(Success)

    DaysMonth = [(days_in_month(m, Year), m = 1, 12)]
    Success = (ALL(DaysMonth == ExpectedDaysMonth))
    CALL CHECK(Success)

    MonthsFromDay = [(month_from_day(DaysInYear(d), Year), d = 1, 4)]
    Success = (ALL(MonthsFromDay == ExpectedMonthsFromDay))
    CALL CHECK(Success)

    ! Change to a no leaps calendar, and try a year which would be a gregorian
    ! leap year
    CALL set_calendar("noleaps")
    Year = 2004
    ExpectedDaysYear = 365
    ExpectedDaysMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    ExpectedMonthsFromDay = [1, 3, 5, 12]

    ! Run through the various tests
    Success = (.not. is_leapyear(Year))
    CALL CHECK(Success)

    Success = (leap_day(Year) == 0)
    CALL CHECK(Success)

    Success = (days_in_year(Year) == ExpectedDaysYear)
    CALL CHECK(Success)

    DaysMonth = [(days_in_month(m, Year), m = 1, 12)]
    Success = (ALL(DaysMonth == ExpectedDaysMonth))
    CALL CHECK(Success)

    MonthsFromDay = [(month_from_day(DaysInYear(d), Year), d = 1, 4)]
    Success = (ALL(MonthsFromDay == ExpectedMonthsFromDay))
    CALL CHECK(Success)

  END SUBROUTINE test_calendars

  SUBROUTINE test_to_from_string()
    !*## Purpose
    !
    ! Test the convenience functions that convert from and to time strings

    CHARACTER(LEN=20) :: TimeString, ExpectedString
    INTEGER :: Year, Day, Seconds, ExpectedYear, ExpectedDay, ExpectedSeconds
    INTEGER :: Month, Hours, Minutes
    LOGICAL :: Success

    ! First time reading from a string- do both just a YYYY-MM-DD format, and a
    ! YYYY-MM-DD HH-mm-SS format
    CALL set_calendar("gregorian")
    TimeString = "2003-04-20"
    ExpectedYear = 2003
    ExpectedDay = 110
    ExpectedSeconds = 0

    CALL read_time_string(TimeString, Year, Day, Seconds)
    Success = ((ExpectedYear == Year) .AND. (ExpectedDay == Day) .AND.&
      (ExpectedSeconds == Seconds))
    CALL CHECK(Success)

    ! Now try one with HH:mm:SS
    TimeString = "2003-04-20 08:30:20"
    ExpectedSeconds = 8 * 3600 + 30 * 60 + 20

    CALL read_time_string(TimeString, Year, Day, Seconds)
    Success = ((ExpectedYear == Year) .AND. (ExpectedDay == Day) .AND.&
      (ExpectedSeconds == Seconds))
    CALL CHECK(Success)

    ! Now go back the other way- start by specifying the minimum info
    Year = 2002
    Month = 7
    Day = 15
    ExpectedString = "2002-07-15 00:00:00"

    TimeString = time_as_string(Year, Month, Day)
    Success = (TRIM(TimeString) == TRIM(ExpectedString))
    CALL CHECK(Success)

    ! Now with full info (and demo keyword args)
    Hours = 11
    Minutes = 35
    Seconds = 10
    ExpectedString = "2002-07-15 11:35:10"

    TimeString = time_as_string(Year, Month, Day, Hour=Hours, Minute=Minutes,&
      Second=Seconds)
    Success = (TRIM(TimeString) == TRIM(ExpectedString))
    CALL CHECK(Success)

  END SUBROUTINE test_to_from_string

  SUBROUTINE test_intervals()
    !*## Purpose
    !
    ! Test the various methods for determining the periods between times.

    INTEGER :: NumIntervals
    LOGICAL :: Success

    CALL set_calendar("gregorian")

    NumIntervals = intervals_since(2000, 3600, 2005, 50)
    Success = (NumIntervals == (1827 * 24 + 50))
    CALL CHECK(Success)

  END SUBROUTINE test_intervals

END MODULE time_tests
