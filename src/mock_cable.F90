PROGRAM mock_cable

  USE iso_fortran_env
  USE mpi_module, ONLY: mpi_grp_t, mpi_mod_init, mpi_mod_end
  USE time_module, ONLY: SecsInDay, set_calendar, days_in_year
  USE meteorology_module, ONLY: prepare_meteorology, get_meteorology, MetType
  USE domain_module, ONLY: process_landmask, ProcessDomain, GlobalDomain

  IMPLICIT NONE

  INTEGER :: StartYear, EndYear, Year, NPoints, nmlUnit, StepsInYear, TimeStep
  REAL :: Dt
  CHARACTER(20) :: Calendar
  CHARACTER(200) :: LandmaskFile
  TYPE(ProcessDomain) :: ProcDomain
  TYPE(GlobalDomain) :: GlobDomain
  TYPE(MetType) :: Met
  TYPE(mpi_grp_t) :: mpi_grp

  NAMELIST /CABLENML/ StartYear, EndYear, Calendar, Dt, LandmaskFile

  OPEN(NEWUNIT=nmlUnit, FILE='cable.nml', STATUS='OLD', ACTION='READ')
  READ(nmlUnit, NML=CABLENML)
  CLOSE(nmlUnit)

  ! Initialise the MPI
  CALL mpi_mod_init()
  mpi_grp = mpi_grp_t()

  ! Initialise the domain
  CALL process_landmask(LandmaskFile, ProcDomain, GlobDomain, mpi_grp)

  ! Set the calendar for the run
  CALL set_calendar(Calendar)

  ! Prepare the meteorology module
  CALL prepare_meteorology(Dt, Met, ProcDomain, mpi_grp)

  DO Year = StartYear, EndYear
    ! Compute number of steps in the year
    StepsInYear = (days_in_year(Year) * SecsInDay) / Dt
    DO TimeStep = 1, StepsInYear
      CALL get_meteorology(Year, TimeStep, ProcDomain, Met)
    END DO
  END DO

  CALL mpi_mod_end()
  
END PROGRAM mock_cable
