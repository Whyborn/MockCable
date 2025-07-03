PROGRAM mock_cable

  USE iso_fortran_env
  USE mpi_module, ONLY: mpi_grp_t, mpi_mod_init, mpi_mod_end
  USE time_module, ONLY: secs_in_day, set_calendar, days_in_year,&
    is_end_of_day, is_end_of_month
  USE meteorology_module, ONLY: MetType, prepare_meteorology, get_meteorology,&
    write_meteorology, finalise_meteorology
  USE output_module, ONLY: initialise_output_module, mpi_info_hints_write
  USE domain_module, ONLY: process_landmask, ProcessDomain, GlobalDomain
  use partition_mod, only: partition_mod_init, partition_mod_end,&
    rectangular_partitioning
  use datasetreader_module, only: mpi_info_hints_read

  IMPLICIT NONE

  INTEGER :: StartYear, EndYear, Year, NPoints, nmlUnit, StepsInYear,&
    TimeStep, dt_int
  REAL :: rain_sum, dt
  logical :: write_output = .true.
  CHARACTER(20) :: Calendar
  CHARACTER(200) :: LandmaskFile
  TYPE(ProcessDomain) :: ProcDomain
  TYPE(GlobalDomain) :: GlobDomain
  TYPE(MetType) :: Met
  TYPE(mpi_grp_t) :: mpi_grp

  ! Set up triggers for specific actions
  LOGICAL, TARGET :: StartOfDay = .TRUE., StartOfMonth = .TRUE.
  LOGICAL, TARGET :: EndOfDay = .TRUE., EndOfMonth = .TRUE.

  ! This is example setup- these could be triggers for respective modules e.g.
  ! geophysics and CASA
  LOGICAL, POINTER :: TriggerA, TriggerB

  NAMELIST /CABLENML/ StartYear, EndYear, Calendar, Dt, LandmaskFile,&
    rectangular_partitioning, mpi_info_hints_write, mpi_info_hints_read
  NAMELIST /DEBUGNML/ write_output

  OPEN(NEWUNIT=nmlUnit, FILE='cable.nml', STATUS='OLD', ACTION='READ')
  READ(nmlUnit, NML=CABLENML)
  READ(nmlUnit, NML=DEBUGNML)
  CLOSE(nmlUnit)

  dt_int = INT(dt)
  ! Initialise the MPI
  CALL mpi_mod_init()
  mpi_grp = mpi_grp_t()

  ! Initialise the domain
  CALL process_landmask(LandmaskFile, ProcDomain, GlobDomain, mpi_grp)

  ! call partition_mod_init(GlobDomain%GlobalLandmask, mpi_grp)

  ! Initialise the output module with the MPI/process information
  CALL initialise_output_module(ProcDomain, mpi_grp)

  ! Set the calendar for the run
  CALL set_calendar(Calendar)

  ! Prepare the meteorology module
  CALL prepare_meteorology(dt, Met, ProcDomain, GlobDomain, mpi_grp, write_output)

  ! Bind the event triggers to their respective periods
  TriggerA => EndOfDay
  TriggerB => EndOfMonth

  DO Year = StartYear, EndYear
    ! Compute number of steps in the year
    StepsInYear = (days_in_year(Year) * secs_in_day) / dt_int
    DO TimeStep = 1, StepsInYear
      ! Check whether it's new day/month, based on whether prev step was the
      ! end of a day or month. This is not actually the same as having
      ! duplicate variables describing the same thing- the variables update at
      ! different times. The StartOfDay/Month variables effectively lag the
      ! EndOfDay/Month variables by 1 timestep.
      StartOfDay = EndOfDay
      StartOfMonth = EndOfMonth

      ! Check whether our timestep ends a day or month
      EndOfDay = is_end_of_day(Timestep, dt_int)
      EndOfMonth = is_end_of_month(Year, Timestep, dt_int)

      CALL get_meteorology(mpi_grp, Year, TimeStep, Met)
      rain_sum = sum(Met%Rain)
      if (write_output) CALL write_meteorology(mpi_grp, Met, TimeStep)

      if (TriggerA) then
        ! Something that happens at the end of every day
      end if

      if (TriggerB) then
        ! Something that happens at the end of a month
      end if
    END DO
  END DO

  ! Close everything down
  call partition_mod_end()
  CALL finalise_meteorology(write_output)
  CALL mpi_mod_end()
  
END PROGRAM mock_cable
