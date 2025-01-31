PROGRAM mock_cable

  USE iso_fortran_env
  USE time_module, ONLY: SecsInDay, set_calendar, days_in_year
  USE meteorology_module, ONLY: prepare_meteorology, get_meteorology, MetType

  IMPLICIT NONE

  INTEGER :: StartYear, EndYear, Year, NPoints, nmlUnit, StepsInYear, TimeStep
  REAL :: Dt
  CHARACTER(20) :: Calendar
  TYPE(MetType) :: Met

  NAMELIST /CABLENML/ StartYear, EndYear, Calendar, Dt

  OPEN(NEWUNIT=nmlUnit, FILE='cable.nml', STATUS='OLD', ACTION='READ')
  READ(nmlUnit, NML=CABLENML)
  CLOSE(nmlUnit)

  CALL set_calendar(Calendar)

  CALL prepare_meteorology(Dt, NPoints, Met)

  DO Year = StartYear, EndYear
    ! Compute number of steps in the year
    StepsInYear = (days_in_year(Year) * SecsInDay) / Dt
    DO TimeStep = 1, StepsInYear
      CALL get_meteorology(Year, TimeStep, Met)
    END DO
  END DO
END PROGRAM mock_cable
