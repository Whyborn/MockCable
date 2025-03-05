MODULE meteorology_module

USE mpi_module, ONLY: mpi_grp_t
USE netcdf
USE datasetreader_module, ONLY: DatasetReader,&
  initialise_datasetreader_at_timestep, get_data
USE common_module, ONLY: handle_ncstat
USE domain_module, ONLY: ProcessDomain, GlobalDomain, from_matrix_to_vector,&
  from_vector_to_matrix
USE output_module, ONLY: NCFile, initialise_output_file

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
TYPE(ProcessDomain) :: ProcDomain
TYPE(GlobalDomain) :: GlobDomain

! File to write output to
TYPE(NCFile) :: MetOutputFile

CONTAINS

SUBROUTINE prepare_meteorology(Timestep, Met, ProcDomainIn, GlobDomainIn,&
    mpi_grp)
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

  ! Allocate memory for the readers
  NPoints = SIZE(ProcDomain%LongitudeIDs)
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

  ! Grab a local copy of the process and global domain
  ProcDomain = ProcDomainIn
  GlobDomain = GlobDomainIn

  ! Iterate through the variables

  ! Create the file to write the output to
  MetOutputFile = initialise_output_file("meteorology_output.nc",&
    ["lon", "lat", "time"], [SIZE(GlobDomain%LongitudeAxis),&
    SIZE(GlobDomain%LatitudeAxis), NF90_UNLIMITED])
  
  ! Set the longitude/latitude axes
  CALL def_variables(MetOutputFile, "lon", "lon", NF90_FLOAT)
  CALL def_variables(MetOutputFile, "lat", "lat", NF90_FLOAT)
  CALL put_dimension_data(MetOutputFile, "lon", GlobDomain%LongitudeAxis)
  CALL put_dimension_data(MetOutputFile, "lat", GlobDomain%LatitudeAxis)

  ! Now initialise the meteorology variables
  CALL def_variables(MetOutputFile,&
    ["Rainf", "Tair", "wind", "Psurf", "SWDown", "LWDown"],&
    ["lon", "lat", "time"], NF90_FLOAT)

END SUBROUTINE prepare_meteorology

SUBROUTINE get_meteorology(Year, Timestep, Met)
  !*## Purpose
  !
  ! Apply the meteorology from a given year and timestep.
  !
  !## Method
  !
  ! Pass the given year and timestep to the meteorology readers to extract the
  ! appropriate record from the dataset. Extract the required elements from the
  ! record based off the landmask.

  INTEGER, INTENT(IN) :: Year, Timestep
  TYPE(MetType), INTENT(INOUT) :: Met

  ! Iterators
  INTEGER :: VarIter, Point

  ! Read the data from file into the DataStorage attached to the readers
  DO VarIter = 1, NumVariables
    CALL get_data(MetDataReaders(VarIter), Year, Timestep)
  END DO

  ! Move the data from the 2D matrices to the 1D vectors
  CALL from_matrix_to_vector(MetDataReaders(RainID)%DataStorage,&
    Met%Rain, ProcDomain)
  CALL from_matrix_to_vector(MetDataReaders(PressureID)%DataStorage,&
    Met%Pressure, ProcDomain)
  CALL from_matrix_to_vector(MetDataReaders(TemperatureID)%DataStorage,&
    Met%Temperature, ProcDomain)
  CALL from_matrix_to_vector(MetDataReaders(WindID)%DataStorage,&
    Met%Wind, ProcDomain)
  CALL from_matrix_to_vector(MetDataReaders(ShortwaveRadID)%DataStorage,&
    Met%ShortwaveRad, ProcDomain)
  CALL from_matrix_to_vector(MetDataReaders(LongwaveRadID)%DataStorage,&
    Met%LongwaveRad, ProcDomain)

END SUBROUTINE get_meteorology

SUBROUTINE write_meteorology(Met, Time)
  !*## Purpose
  !
  ! Write out the meteorology data back in matrix format
  !
  !## Method
  !
  ! Convert the vectorised meteorology to a matrix, then write it to the file
  ! using the NCFile interface.

  TYPE(MetType), INTENT(IN) :: Met
  INTEGER, INTENT(IN) :: Time

  ! Need temporary storage for the reshaped data
  REAL, DIMENSION(:,:), ALLOCATABLE :: MetStorage

  ! Extend the time dimension with the new time
  CALL extend_unlimited_dimension(MetOutputFile, "time", Time)
  
  ! Use the process domain to allocate the storage
  ALLOCATE(MetStorage(ProcDomain%ProcessDomainSize(1),&
    ProcDomain%ProcessDomainSize(2)))

  ! For each variable, reshape to matrix then write out
  CALL from_vector_to_matrix(Met%Rain, MetStorage, ProcDomain)
  CALL put_record(MetOutputFile, "Rainf", MetStorage)

  CALL from_vector_to_matrix(Met%Temperature, MetStorage, ProcDomain)
  CALL put_record(MetOutputFile, "Tair", MetStorage)

  CALL from_vector_to_matrix(Met%Wind, MetStorage, ProcDomain)
  CALL put_record(MetOutputFile, "wind", MetStorage)

  CALL from_vector_to_matrix(Met%Pressure, MetStorage, ProcDomain)
  CALL put_record(MetOutputFile, "Psurf", MetStorage)

  CALL from_vector_to_matrix(Met%ShortwaveRad, MetStorage, ProcDomain)
  CALL put_record(MetOutputFile, "SWdown", MetStorage)

  CALL from_vector_to_matrix(Met%LongwaveRad, MetStorage, ProcDomain)
  CALL put_record(MetOutputFile, "LWdown", MetStorage)

END SUBROUTINE write_meteorology

END MODULE meteorology_module
