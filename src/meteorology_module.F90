MODULE meteorology_module

USE mpi_module, ONLY: mpi_grp_t
USE netcdf
USE datasetreader_module, ONLY: DatasetReader,&
  initialise_datasetreader_at_timestep, get_data, close_reader
USE common_module, ONLY: handle_ncstat
USE domain_module, ONLY: ProcessDomain, GlobalDomain
USE output_module, ONLY: NCFile, initialise_output_file, def_variables,&
  put_dimension_data, extend_unlimited_dimension, put_record, close_file
use partition_mod, only: get_n_land, transform_grid_to_land, transform_land_to_grid

IMPLICIT NONE

TYPE MetType
  REAL, DIMENSION(:), ALLOCATABLE :: Rain, Pressure, Temperature, Wind,&
    ShortwaveRad, LongwaveRad
END TYPE MetType

! Set up the variable IDs
INTEGER, PARAMETER :: RainID = 1, TemperatureID = 2, WindID = 3,&
  PressureID = 4, ShortwaveRadID = 5, LongwaveRadID = 6, NumVariables = 6

TYPE(DatasetReader), DIMENSION(NumVariables) :: MetDataReaders

! Information about the process and global domains
TYPE(ProcessDomain), PRIVATE :: ProcDomain
TYPE(GlobalDomain), PRIVATE :: GlobDomain

! File to write output to
TYPE(NCFile) :: MetOutputFile

CONTAINS

SUBROUTINE prepare_meteorology(Timestep, Met, ProcDomainIn, GlobDomainIn,&
    mpi_grp, write_output)
  !*## Purpose
  !
  ! Prepare the meteorology input routines
  !
  !## Method
  !
  ! Read the met.nml to determine the locations of the meteorology files.

  REAL, INTENT(IN) :: Timestep
  TYPE(MetType), INTENT(OUT) :: Met
  TYPE(ProcessDomain), INTENT(IN) :: ProcDomainIn
  TYPE(GlobalDomain), INTENT(IN) :: GlobDomainIn
  TYPE(mpi_grp_t), INTENT(IN) :: mpi_grp
  LOGICAL, INTENT(IN) :: write_output

  CHARACTER(LEN=300) :: RainFile, TemperatureFile, WindFile, PressureFile,&
    ShortwaveRadFile, LongwaveRadFile

  INTEGER :: nmlUnit, VarIter, NPoints

  NAMELIST /metnml/ RainFile, TemperatureFile, WindFile, PressureFile,&
  ShortwaveRadFile, LongwaveRadFile
  
  ! Set the default values for the files
  RainFile = ""
  TemperatureFile = ""
  WindFile = ""
  PressureFile = ""
  ShortwaveRadFile = ""
  LongwaveRadFile = ""

  OPEN(NEWUNIT=nmlUnit, FILE='met.nml', STATUS='OLD', ACTION='READ')
  READ(nmlUnit, NML=metnml)
  CLOSE(nmlUnit)

  ! Grab a local copy of the process and global domain
  ProcDomain = ProcDomainIn
  GlobDomain = GlobDomainIn

  ! Allocate memory for the readers
  NPoints = get_n_land(mpi_grp)
  ALLOCATE(Met%Rain(NPoints), Met%Temperature(NPoints), Met%Wind(NPoints),&
    Met%Pressure(NPoints), Met%ShortwaveRad(NPoints), Met%LongwaveRad(NPoints))

  ! Initialise the dataset readers
  MetDataReaders(RainID) = initialise_datasetreader_at_timestep(&
    RainFile, ['Rainf'], Timestep, ProcDomain, mpi_grp)
  MetDataReaders(TemperatureID) = initialise_datasetreader_at_timestep(&
    TemperatureFile, ['Tair'], Timestep, ProcDomain, mpi_grp)
  MetDataReaders(WindID) = initialise_datasetreader_at_timestep(&
    WindFile, ['wind'], Timestep, ProcDomain, mpi_grp)
  MetDataReaders(PressureID) = initialise_datasetreader_at_timestep(&
    PressureFile, ['Psurf'], Timestep, ProcDomain, mpi_grp)
  MetDataReaders(ShortwaveRadID) = initialise_datasetreader_at_timestep(&
    ShortwaveRadFile, ['SWdown'], Timestep, ProcDomain, mpi_grp)
  MetDataReaders(LongwaveRadID) = initialise_datasetreader_at_timestep(&
    LongwaveRadFile, ['LWdown'], Timestep, ProcDomain, mpi_grp)

  if (.not. write_output) return

  ! Create the file to write the output to
  MetOutputFile = initialise_output_file(&
    "meteorology_output.nc",&
    ["lon ", "lat ", "time"], [SIZE(GlobDomain%LongitudeAxis),&
    SIZE(GlobDomain%LatitudeAxis), NF90_UNLIMITED])
  
  ! Set the longitude/latitude axes
  CALL def_variables(MetOutputFile, "lon", "lon", NF90_FLOAT)
  CALL def_variables(MetOutputFile, "lat", "lat", NF90_FLOAT)
  CALL def_variables(MetOutputFile, "time", "time", NF90_INT)

  ! Now initialise the meteorology variables
  CALL def_variables(MetOutputFile,&
    ["LWdown", "Rainf ", "Tair  ", "wind  ", "Psurf ", "SWdown"],&
    ["lon ", "lat ", "time"], NF90_FLOAT)

  ! Put the dimension data
  CALL put_dimension_data(MetOutputFile, "lon", GlobDomain%LongitudeAxis)
  CALL put_dimension_data(MetOutputFile, "lat", GlobDomain%LatitudeAxis)

END SUBROUTINE prepare_meteorology

SUBROUTINE get_meteorology(mpi_grp, Year, Timestep, Met)
  !*## Purpose
  !
  ! Apply the meteorology from a given year and timestep.
  !
  !## Method
  !
  ! Pass the given year and timestep to the meteorology readers to extract the
  ! appropriate record from the dataset. Extract the required elements from the
  ! record based off the landmask.

  type(mpi_grp_t), intent(in) :: mpi_grp
  INTEGER, INTENT(IN) :: Year, Timestep
  TYPE(MetType), INTENT(INOUT) :: Met

  ! Iterators
  INTEGER :: VarIter, Point

  ! Read the data from file into the DataStorage attached to the readers
  DO VarIter = 1, NumVariables
    CALL get_data(MetDataReaders(VarIter), Year, Timestep)
  END DO

  ! Move the data from the 2D matrices to the 1D vectors
  CALL transform_grid_to_land(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, MetDataReaders(RainID)%DataStorage, Met%Rain)
  CALL transform_grid_to_land(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, MetDataReaders(PressureID)%DataStorage, Met%Pressure)
  CALL transform_grid_to_land(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, MetDataReaders(TemperatureID)%DataStorage, Met%Temperature)
  CALL transform_grid_to_land(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, MetDataReaders(WindID)%DataStorage, Met%Wind)
  CALL transform_grid_to_land(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, MetDataReaders(ShortwaveRadID)%DataStorage, Met%ShortwaveRad)
  CALL transform_grid_to_land(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, MetDataReaders(LongwaveRadID)%DataStorage, Met%LongwaveRad)

END SUBROUTINE get_meteorology

SUBROUTINE write_meteorology(mpi_grp, Met, Time)
  !*## Purpose
  !
  ! Write out the meteorology data back in matrix format
  !
  !## Method
  !
  ! Convert the vectorised meteorology to a matrix, then write it to the file
  ! using the NCFile interface.

  type(mpi_grp_t), intent(in) :: mpi_grp
  TYPE(MetType), INTENT(IN) :: Met
  INTEGER, INTENT(IN) :: Time

  ! Need temporary storage for the reshaped data
  REAL, DIMENSION(:,:), ALLOCATABLE :: MetStorage

  ! Extend the time dimension with the new time
  CALL extend_unlimited_dimension(MetOutputFile, "time", Time)
  
  ! Use the process domain to allocate the storage
  ALLOCATE(MetStorage(ProcDomain%ProcessDomainSize(1),&
    ProcDomain%ProcessDomainSize(2)))

  MetStorage = NF90_FILL_REAL

  ! For each variable, reshape to matrix then write out
  call transform_land_to_grid(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, Met%Rain, MetStorage, fill_value=NF90_FILL_REAL)
  CALL put_record(MetOutputFile, "Rainf", MetStorage)

  call transform_land_to_grid(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, Met%Temperature, MetStorage, fill_value=NF90_FILL_REAL)
  CALL put_record(MetOutputFile, "Tair", MetStorage)

  call transform_land_to_grid(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, Met%Wind, MetStorage, fill_value=NF90_FILL_REAL)
  CALL put_record(MetOutputFile, "wind", MetStorage)

  call transform_land_to_grid(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, Met%Pressure, MetStorage, fill_value=NF90_FILL_REAL)
  CALL put_record(MetOutputFile, "Psurf", MetStorage)

  call transform_land_to_grid(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, Met%ShortwaveRad, MetStorage, fill_value=NF90_FILL_REAL)
  CALL put_record(MetOutputFile, "SWdown", MetStorage)

  call transform_land_to_grid(mpi_grp, ProcDomain%ProcessDomainStart, ProcDomain%ProcessDomainSize, Met%LongwaveRad, MetStorage, fill_value=NF90_FILL_REAL)
  CALL put_record(MetOutputFile, "LWdown", MetStorage)

END SUBROUTINE write_meteorology

SUBROUTINE finalise_meteorology(write_output)
  !*## Purpose
  !
  ! Close all the file handles used in the meteorology
  LOGICAL, INTENT(IN) :: write_output

  INTEGER :: Iter

  if (write_output) CALL close_file(MetOutputFile)

  DO Iter = 1, NumVariables
    CALL close_reader(MetDataReaders(Iter))
  END DO

END SUBROUTINE finalise_meteorology

END MODULE meteorology_module
