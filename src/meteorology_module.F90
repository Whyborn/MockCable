MODULE meteorology_module

USE mpi_module, ONLY: mpi_grp_t
USE netcdf, ONLY: NF90_OPEN, NF90_NOWRITE, NF90_INQ_DIMID,&
  NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_VAR
USE datasetreader_module, ONLY: DatasetReader,&
  initialise_datasetreader_at_timestep, get_data
USE common_module, ONLY: handle_ncstat
USE domain_module, ONLY: ProcessDomain

IMPLICIT NONE

TYPE MetType
  REAL, DIMENSION(:), ALLOCATABLE :: Rain, Pressure, Temperature, Wind,&
    ShortwaveRad, LongwaveRad
END TYPE MetType

INTEGER, DIMENSION(:), ALLOCATABLE :: LonIDs, LatIDs

! Set up the variable IDs
INTEGER, PARAMETER :: RainID = 1, TemperatureID = 2, WindID = 3,&
  PressureID = 4, ShortwaveRadID = 5, LongwaveRadID = 6, NumVariables = 6

! Specify which ones are required
INTEGER, DIMENSION(5) :: RequiredVariables =&
  [RainID, TemperatureID, WindID, PressureID, ShortwaveRadID]

! And which ones are optional
INTEGER, DIMENSION(1) :: OptionalVariables =&
  [LongwaveRadID]

TYPE(DatasetReader), DIMENSION(NumVariables) :: MetDataReaders

CONTAINS

SUBROUTINE prepare_meteorology(Timestep, Met, ProcDomain, mpi_grp)
  !*## Purpose
  !
  ! Prepare the meteorology input routines
  !
  !## Method
  !
  ! Read the met.nml to determine the locations of the meteorology files.

  REAL, INTENT(IN) :: Timestep
  TYPE(MetType), INTENT(OUT) :: Met
  TYPE(ProcessDomain), INTENT(IN) :: ProcDomain
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

END SUBROUTINE prepare_meteorology

SUBROUTINE get_meteorology(Year, Timestep, ProcDomain, Met)
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
  TYPE(ProcessDomain), INTENT(IN) :: ProcDomain
  TYPE(MetType), INTENT(INOUT) :: Met

  ! Iterators
  INTEGER :: VarIter, Point

  ! Read the data from file into the DataStorage attached to the readers
  DO VarIter = 1, SIZE(RequiredVariables)
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

SUBROUTINE from_matrix_to_vector(MatrixInput, VectorOutput, ProcDomain)
  !*## Purpose
  !
  ! Map a matrix of input data to a 1D vector using the process domain
  ! information.
  !
  !## Method
  !
  ! The process domain contains the per process mapping from the 2D data input
  ! domain to the 1D vector domain used in CABLE's science routines. Use this
  ! information to map from to the other.

  REAL, DIMENSION(:,:), ALLOCATABLE, INTENT(IN) :: MatrixInput
  REAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: VectorOutput
  TYPE(ProcessDomain), INTENT(IN) :: ProcDomain

  ! Just need an iterator
  INTEGER :: Point

  DO Point = 1, SIZE(ProcDomain%LongitudeIDs)
    VectorOutput(Point) = MatrixInput(ProcDomain%LongitudeIDs(Point),&
      ProcDomain%LatitudeIDs(Point))
  END DO

END SUBROUTINE from_matrix_to_vector

END MODULE meteorology_module
