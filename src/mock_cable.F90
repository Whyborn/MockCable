PROGRAM mock_cable

  USE iso_fortran_env
  USE mpi_module, ONLY: mpi_grp_t, mpi_mod_init
  USE time_module, ONLY: SecsInDay, set_calendar, days_in_year
  USE meteorology_module, ONLY: prepare_meteorology, get_meteorology, MetType

  IMPLICIT NONE

  INTEGER :: StartYear, EndYear, Year, NPoints, nmlUnit, StepsInYear, TimeStep
  REAL :: Dt
  CHARACTER(20) :: Calendar
  TYPE(MetType) :: Met
  TYPE(mpi_grp_t) :: mpi_grp

  NAMELIST /CABLENML/ StartYear, EndYear, Calendar, Dt

  OPEN(NEWUNIT=nmlUnit, FILE='cable.nml', STATUS='OLD', ACTION='READ')
  READ(nmlUnit, NML=CABLENML)
  CLOSE(nmlUnit)

  ! Initialise the MPI
  CALL mpi_mod_init()
  mpi_grp = mpi_grp_t()

  CALL set_calendar(Calendar)

  CALL prepare_meteorology(Dt, NPoints, Met, mpi_grp)

  DO Year = StartYear, EndYear
    ! Compute number of steps in the year
    StepsInYear = (days_in_year(Year) * SecsInDay) / Dt
    DO TimeStep = 1, StepsInYear
      CALL get_meteorology(Year, TimeStep, Met)
    END DO
  END DO
END PROGRAM mock_cable
