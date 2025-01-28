! Author: Lachlan Whyborn
! Last Modified: Tue 14 Jan 2025 01:07:32 PM AEDT

MODULE time_module

USE iso_fortran_env, ONLY: ERROR_UNIT

PRIVATE :: CalendarType, Gregorian, NoLeaps
ENUM, BIND(c)
  ENUMERATOR :: CalendarType = 0
  ENUMERATOR :: Gregorian = 1
  ENUMERATOR :: NoLeaps = 2
END ENUM

INTEGER(KIND(CalendarType)) :: Calendar = Gregorian

! A unit to indicate an error in the time module. Various compilers already
! occupy most integers up to 1000.
INTEGER, PARAMETER :: TimeError = 1005

CONTAINS

SUBROUTINE set_calendar(CalendarString)
  !*## Purpose
  !
  ! Set the calendar to be used for the simulation.
  !
  !## Method
  !
  ! Take a string describing the calendar and use it to set the calendar
  ! used in the rest of the time-based operations for the simulation.

  CHARACTER(LEN=*), INTENT(IN) :: CalendarString

  IF (TRIM(CalendarString) == "gregorian") THEN
    Calendar = Gregorian
  ELSEIF (TRIM(CalendarString) == "noleaps") THEN
    Calendar = NoLeaps
  ELSE
    WRITE(ERROR_UNIT,*) TRIM(CalendarString), " is not a recognised calendar."
    STOP TimeError
  END IF

END SUBROUTINE set_calendar

FUNCTION days_in_month(Month, Year)
  !*## Purpose
  !
  ! Return the numbers of days in the month for the given hear.
  !
  !## Method
  !
  ! Check the number of the month and return the correct number of days,
  ! accounting for any calendar abnormalities.

  INTEGER, INTENT(IN) :: Month, Year

  INTEGER :: days_in_month

  ! Just explicitly iterate the months for each instance
  IF ((Month == 1) .OR. (Month == 3) .OR. (Month == 5) .OR. (Month == 7) .OR.&
    (Month == 8) .OR. (Month == 10) .OR. (Month == 12)) THEN
    days_in_month = 31
  ELSEIF ((Month == 4) .OR. (Month == 6) .OR. (Month == 9) .OR. (Month == 11))&
    THEN
    days_in_month = 30
  ELSEIF (Month == 2) THEN
    days_in_month = 28 + leap_day(Year)
  ELSE
    WRITE(ERROR_UNIT, '(A)') "Invalid month passed to days_in_month."
    STOP TimeError
  END IF
END FUNCTION days_in_month

FUNCTION is_leapyear(Year)
  !*## Purpose
  !
  ! Check if the given year is a leap year based off the current calendar.
  !
  !## Method
  !
  ! Pass the year through to the checker based off the prescribed calendar.

  INTEGER, INTENT(IN) :: Year

  LOGICAL :: is_leapyear

  ! Set the default
  is_leapyear = .FALSE.

  IF (Calendar == Gregorian) THEN
    IF (((MOD(Year, 4) == 0) .AND. (MOD(Year, 100) /= 0)) .OR.&
      (MOD(Year, 400) == 0)) THEN
      is_leapyear = .TRUE.
    END IF
  END IF
END FUNCTION is_leapyear

FUNCTION leap_day(Year)
  !*## Purpose
  !
  ! Convenience function to get the length of a year/February without leap year
  ! logic check.
  !
  !## Method
  !
  ! Returns 1 if the year is a leap year based off the current calendar, 0
  ! otherwise.

  INTEGER, INTENT(IN) :: Year

  INTEGER :: leap_day

  leap_day = 0

  IF (is_leapyear(Year)) THEN
    leap_day = 1
  END IF
END FUNCTION leap_day

SUBROUTINE read_time_string(TimeString, Year, Day, Second)
  !*## Purpose
  !
  ! Read the year, day and second from a time string.
  !
  !## Method
  !
  ! Assume the time string consists of "YYYY-MM-DD" optionally followed by
  ! "HH:mm:SS". Use this pattern to extract each of the quantities from the
  ! string.

  CHARACTER(LEN=*), INTENT(IN) :: TimeString
  INTEGER, INTENT(OUT) :: Year, Day, Second

  ! The intermediate quantities used to compute the desired quantities
  INTEGER :: Month, Hour, Minute, MonthIter

  ! We want to Lshift the time of day to prevent access errors
  CHARACTER(LEN=30) :: TimeDate, TimeOfDay

  ! Initialise seconds to 0 as we may not read it
  Second = 0

  ! Cut any leading whitespace
  TimeDate = ADJUSTL(TimeString)

  ! Read the YYYY-MM-DD section
  READ(TimeDate(1:4 ),*) Year
  READ(TimeDate(6:7 ),*) Month
  READ(TimeDate(9:10),*) Day

  ! Convert months+days to just days
  CountDays: DO MonthIter = 1, Month-1
    Day = Day + days_in_month(MonthIter, Year)
  END DO CountDays

  IF (LEN(TRIM(TimeDate)) > 10) THEN

    ! We have the HH:mm:SS as well. There appear to be some datasets in which
    ! the second is defined as a floating point real, but we'll assume that we
    ! are given integer seconds
    TimeOfDay = ADJUSTL(TimeDate(11:))
    WRITE(*,*) TimeOfDay
    READ(TimeOfDay(1:2),*) Hour
    READ(TimeOfDay(4:5),*) Minute
    READ(TimeOfDay(7:8),*) Second

    Second = Second + 3600 * Hour + 60 * Minute
  END IF
END SUBROUTINE read_time_string

SUBROUTINE add_to_date(TimeIncrement, IncrementUnits, RefYear, RefDay, RefSec)
  !*## Purpose
  !
  ! Add an increment of specified units to a reference date.
  !
  !## Method
  !
  ! Convert the passed increment into years, days and seconds, then add it to
  ! the reference date. 

  INTEGER, INTENT(IN) :: TimeIncrement
  CHARACTER(LEN=*), INTENT(IN) :: IncrementUnits

  INTEGER, INTENT(INOUT) :: RefYear, RefDay, RefSec

  ! We'll need to convert the increment into our desired units
  INTEGER :: IncrementYears, IncrementDays, IncrementSec

  IncrementYears = 0
  IncrementDays = 0
  IncrementSec = 0

  ! Convert the increment into desired units
  IF (TRIM(IncrementUnits) == "seconds") THEN
    IncrementSec = MOD(TimeIncrement, 3600 * 24)
  ELSEIF (TRIM(IncrementUnits) == "minutes") THEN
    IncrementSec = MOD(TimeIncrement, 60 * 24) * 60
  ELSEIF (TRIM(IncrementUnits) == "hours") THEN
    IncrementSec = MOD(TimeIncrement, 24) * 3600
  ELSEIF (TRIM(IncrementUnits) == "days") THEN
    IncrementDays = TimeIncrement
  ELSEIF (TRIM(IncrementUnits) == "years") THEN
    IncrementYears = TimeIncrement
  ELSE
    WRITE(ERROR_UNIT, '(A)') "Invalid increment units passed to add_to_date."
    STOP TimeError
  END IF

  ! Add the increment to the reference date
  RefYear = RefYear + IncrementYears
  RefDay = RefDay + IncrementDays
  RefSec = RefSec + IncrementSec

  ! Handle any rollovers
  RefDay = RefDay + RefSec / (3600 * 24)
  RefSec = MOD(RefSec, 3600 * 24)

  DO WHILE (RefDay > (365 + leap_day(RefYear)))
    RefYear = RefYear + 1
    RefDay = RefDay - (365 + leap_day(RefYear))
  END DO

END SUBROUTINE add_to_date

FUNCTION days_since(RefYear, RefDay, NewYear, NewDay)
  !*## Purpose
  !
  ! Count the number of days between NewDay in NewYear and RefDay in RefYear.
  !
  !## Method
  !
  ! Count through the years between the two times and add the days from each
  ! year to the count.
  INTEGER, INTENT(IN) :: RefYear, RefDay, NewYear, NewDay

  ! Iterator to count through the years
  INTEGER :: YearIter

  ! Initialiser the return value
  days_since = 0

  CountDays: DO YearIter = RefYear, NewYear-1
    days_since = days_since + 365 + leap_day(YearIter)
  END DO CountDays

  days_since = days_since - RefDay + NewDay

  ! Check the result is valid
  IF (days_since < 0) THEN
    WRITE(ERROR_UNIT, '(A)') "Error in days_since, reference date is later"//&
      " than the new date."
    STOP TimeError
  END IF

END FUNCTION days_since

END MODULE time_module
