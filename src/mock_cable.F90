PROGRAM mock_cable

  USE iso_fortran_env
  USE mpi_module, ONLY: mpi_grp_t, mpi_mod_init, mpi_mod_end
  USE time_module, ONLY: SecsInDay, set_calendar, days_in_year
  USE meteorology_module, ONLY: MetType, prepare_meteorology, get_meteorology,&
    write_meteorology, finalise_meteorology
  USE output_module, ONLY: mpi_info_hints_write
  use datasetreader_module, only: mpi_info_hints_read
  use cable_netcdf_mod, only: cable_netcdf_mod_init, cable_netcdf_mod_end
  use land_decomp_mod, only: land_decomp_t, land_decomp_init

  IMPLICIT NONE

  INTEGER :: StartYear, EndYear, Year, NPoints, nmlUnit, StepsInYear, TimeStep
  REAL :: Dt, rain_sum
  logical :: write_output = .true.
  CHARACTER(20) :: Calendar
  CHARACTER(200) :: LandmaskFile
  TYPE(MetType) :: Met
  TYPE(mpi_grp_t) :: mpi_grp
  type(land_decomp_t) :: land_decomp

  NAMELIST /CABLENML/ StartYear, EndYear, Calendar, Dt, LandmaskFile, mpi_info_hints_write, mpi_info_hints_read
  NAMELIST /DEBUGNML/ write_output

  OPEN(NEWUNIT=nmlUnit, FILE='cable.nml', STATUS='OLD', ACTION='READ')
  READ(nmlUnit, NML=CABLENML)
  READ(nmlUnit, NML=DEBUGNML)
  CLOSE(nmlUnit)

  ! Initialise the MPI
  CALL mpi_mod_init()
  mpi_grp = mpi_grp_t()

  call cable_netcdf_mod_init(mpi_grp)

  ! TODO(Sean): replace LandmaskFile argument with a derived type for namelist
  ! parameters. Reasoning for this is that the land mask can be initialised from
  ! multiple sources (e.g. the met file or standalone land mask).
  call land_decomp_init(LandmaskFile, mpi_grp, land_decomp)

  ! Set the calendar for the run
  CALL set_calendar(Calendar)

  ! Prepare the meteorology module
  CALL prepare_meteorology(Dt, Met, land_decomp, write_output)

  DO Year = StartYear, EndYear
    ! Compute number of steps in the year
    StepsInYear = (days_in_year(Year) * SecsInDay) / Dt
    DO TimeStep = 1, StepsInYear
      CALL get_meteorology(Year, TimeStep, Met)
      rain_sum = sum(Met%Rain)
      if (write_output) CALL write_meteorology(Met, TimeStep)
    END DO
  END DO

  ! Close everything down
  CALL finalise_meteorology(write_output)
  call cable_netcdf_mod_end()
  CALL mpi_mod_end()
  
END PROGRAM mock_cable
