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

    INTEGER :: ExpectedDaysYear, Year, m
    INTEGER, DIMENSION(:), ALLOCATABLE :: ExpectedDaysMonth, DaysMonth
    LOGICAL :: Success

    ! Start with Gregorian non-leap year
    CALL set_calendar("gregorian")
    Year = 2001
    ExpectedDaysYear = 365
    ExpectedDaysMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

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

    ! Now try a leap year
    Year = 2004
    ExpectedDaysYear = 366
    ExpectedDaysMonth = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

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

    ! Change to a no leaps calendar, and try a year which would be a gregorian
    ! leap year
    CALL set_calendar("noleaps")
    Year = 2004
    ExpectedDaysYear = 365
    ExpectedDaysMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

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

  END SUBROUTINE test_calendars

  subroutine test_end_of_period()
    !*## Purpose
    !
    ! Test the end-of-period routines

    integer :: year, dt, step

    ! Set timestep of 4hrs
    dt = 14400

    ! Check for days- anything a multiple of 6 should be true
    CALL CHECK(is_end_of_day(6, dt))
    CALL CHECK(is_end_of_day(6000, dt))
    CALL CHECK(.NOT. is_end_of_day(5, dt))

    ! Check for months- do leap and non-leap years
    year = 2003
    CALL CHECK(is_end_of_month(year, 31 * 6, dt))
    CALL CHECK(is_end_of_month(year, 59 * 6, dt))
    CALL CHECK(is_end_of_month(year, 365 * 6, dt))
    CALL CHECK(.NOT. is_end_of_month(year, 30 * 6, dt))

    year = 2004
    CALL CHECK(is_end_of_month(year, 60 * 6, dt))
    CALL CHECK(.NOT. is_end_of_month(year, 59 * 6, dt))

  end subroutine test_end_of_period

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
