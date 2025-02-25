MODULE meteorology_module

USE mpi_module, ONLY: mpi_grp_t
USE netcdf, ONLY: NF90_OPEN, NF90_NOWRITE, NF90_INQ_DIMID,&
  NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_VAR
USE datasetreader_module, ONLY: DatasetReader,&
  initialise_datasetreader_at_timestep, get_data
USE common_module, ONLY: handle_ncstat

IMPLICIT NONE

TYPE MetType
  REAL, DIMENSION(:), ALLOCATABLE :: Rain, Pressure, Temperature, Wind,&
    ShortwaveRad, LongwaveRad
END TYPE MetType

TYPE MetContainer
  REAL, DIMENSION(:,:), ALLOCATABLE :: VarData
END TYPE MetContainer

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
TYPE(MetContainer), DIMENSION(NumVariables) :: MetContainers

CONTAINS

SUBROUTINE prepare_meteorology(Timestep, Met, mpi_grp)
  !*## Purpose
  !
  ! Prepare the meteorology input routines and land mask
  !
  !## Method
  !
  ! Read the met.nml to determine the locations of the meteorology and landmask
  ! files.

  REAL, INTENT(IN) :: Timestep
  TYPE(MetType), INTENT(OUT) :: Met
  TYPE(mpi_grp_t), INTENT(IN) :: mpi_grp

  CHARACTER(LEN=300) :: RainFile, TemperatureFile, WindFile, PressureFile,&
    ShortwaveRadFile, LongwaveRadFile, LandmaskFile

  ! Things required for the mask creation+parallelism
  INTEGER, DIMENSION(2) :: ProcessStart
  INTEGER, DIMENSION(:,:), ALLOCATABLE, TARGET :: ProcessMask
  INTEGER :: NPoints

  INTEGER :: nmlUnit, VarIter

  NAMELIST /metnml/ RainFile, TemperatureFile, WindFile, PressureFile,&
  ShortwaveRadFile, LongwaveRadFile, LandmaskFile
  
  ! Set the default values for the files
  RainFile = ""
  TemperatureFile = ""
  WindFile = ""
  PressureFile = ""
  ShortwaveRadFile = ""
  LongwaveRadFile = ""
  LandmaskFile = ""

  OPEN(NEWUNIT=nmlUnit, FILE='met.nml', STATUS='OLD', ACTION='READ')
  READ(nmlUnit, NML=metnml)
  CLOSE(nmlUnit)

  ! Prepare the landmask- it needs to give us information about the mask and
  ! the starting index in each process. 
  CALL prepare_landmask(LandmaskFile, ProcessMask, ProcessStart)

  ! Set the number of points in the given mask
  NPoints = SUM(ProcessMask)

  ! Allocate memory for the readers
  ALLOCATE(Met%Rain(NPoints), Met%Temperature(NPoints), Met%Wind(NPoints),&
    Met%Pressure(NPoints), Met%ShortwaveRad(NPoints), Met%LongwaveRad(NPoints))

  ! Initialise the dataset readers
  MetDataReaders(RainID) = initialise_datasetreader_at_timestep(&
    RainFile, ['Rainf'], Timestep, mpi_grp)
  MetDataReaders(TemperatureID) = initialise_datasetreader_at_timestep(&
    TemperatureFile, ['Tair'], Timestep, mpi_grp)
  MetDataReaders(WindID) = initialise_datasetreader_at_timestep(&
    WindFile, ['wind'], Timestep, mpi_grp)
  MetDataReaders(PressureID) = initialise_datasetreader_at_timestep(&
    PressureFile, ['Psurf'], Timestep, mpi_grp)
  MetDataReaders(ShortwaveRadID) = initialise_datasetreader_at_timestep(&
    ShortwaveRadFile, ['SWdown'], Timestep, mpi_grp)
  MetDataReaders(LongwaveRadID) = initialise_datasetreader_at_timestep(&
    LongwaveRadFile, ['LWdown'], Timestep, mpi_grp)

  ! Assign the mask and indices to each reader
  AssignDomains: DO VarIter = 1, NumVariables
    MetDataReaders(VarIter)%Starts = ProcessStart
    MetDataReaders(VarIter)%Mask => ProcessMask
  END DO AssignDomains
  
END SUBROUTINE prepare_meteorology

SUBROUTINE prepare_landmask(LandmaskFile, ProcessMask, ProcessStart)
  !*## Purpose
  !
  ! Read the specified landmask file to generate the matrix to vector mappings
  ! and return the number of land points in the mask.
  !
  !## Method
  !
  ! Count the number of land points in the mask to allocate memory, then
  ! iterate through the dimensions of the landmask and add coordinates where
  ! the mask is equal to 1 to the list of LonIDs and LatIDs.

  CHARACTER(LEN=300), INTENT(IN) :: LandmaskFile
  INTEGER, DIMENSION(:,:), ALLOCATABLE, TARGET, INTENT(OUT) :: ProcessMask
  INTEGER, DIMENSION(2) :: ProcessStart

  ! Iterators and counters
  INTEGER :: Lat, Lon, Counter, VarIter, NPoints

  ! IDs for netCDF io
  INTEGER :: ncID, LatID, LonID, MaskID, nLat, nLon, ok

  ! The original mask from file
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: Landmask

  CALL handle_ncstat(NF90_OPEN(TRIM(LandmaskFile), NF90_NOWRITE, ncID))
  CALL handle_ncstat(NF90_INQ_DIMID(ncID, 'longitude', LonID))
  CALL handle_ncstat(NF90_INQUIRE_DIMENSION(ncID, LonID, LEN=nLon))

  CALL handle_ncstat(NF90_INQ_DIMID(ncID, 'latitude', LatID))
  CALL handle_ncstat(NF90_INQUIRE_DIMENSION(ncID, LatID, LEN=nLat))

  ! Allocate memory for the mask and met variables
  ALLOCATE(Landmask(nLon, nLat))
  DO VarIter = 1, NumVariables
    ALLOCATE(MetContainers(VarIter)%VarData(nLon, nLat))
  END DO

  ! Read the mask
  CALL handle_ncstat(NF90_INQ_VARID(ncID, 'mask', MaskID))
  CALL handle_ncstat(NF90_GET_VAR(ncID, MaskID, Landmask, START=[1, 1]))

  ! Check how many land points there are
  NPoints = SUM(Landmask)

  ! Now we can allocate memory for the point IDs
  ALLOCATE(LonIDs(Npoints), LatIDs(NPoints))

  ! Walk over the mask and extract the land points
  Counter = 1
  DO Lat = 1, nLat
    DO Lon = 1, nLon
      IF (Landmask(Lon, Lat) == 1) THEN
        LonIDs(Counter) = Lon
        LatIDs(Counter) = Lat
        Counter = Counter + 1
      END IF
    END DO
  END DO

  ! For now, just hardcode the start indices to [1, 1]
  ProcessStart = [1, 1]
  ProcessMask = Landmask

END SUBROUTINE prepare_landmask

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

  DO VarIter = 1, SIZE(RequiredVariables)
    CALL get_data(MetContainers(VarIter)%VarData, MetDataReaders(VarIter),&
      Year, Timestep)
  END DO

  ! Apply the data
  DO Point = 1, SIZE(LonIDs)
    Met%Rain(Point) =&
      MetContainers(RainID)%VarData(LonIDs(Point), LatIDs(Point))
    Met%Pressure(Point) =&
      MetContainers(PressureID)%VarData(LonIDs(Point), LatIDs(Point))
    Met%Temperature(Point) =&
      MetContainers(TemperatureID)%VarData(LonIDs(Point), LatIDs(Point))
    Met%Wind(Point) =&
      MetContainers(WindID)%VarData(LonIDs(Point), LatIDs(Point))
    Met%ShortwaveRad(Point) =&
      MetContainers(ShortwaveRadID)%VarData(LonIDs(Point), LatIDs(Point))
  END DO

  ! Handle optional variables
  IF (MetDataReaders(LongwaveRadID)%HasData) THEN
    CALL get_data(MetContainers(LongwaveRadID)%VarData,&
      MetDataReaders(LongwaveRadID), Year, Timestep)
    DO Point = 1, SIZE(LonIDs)
      Met%LongwaveRad(Point) =&
        MetContainers(LongwaveRadID)%VarData(LonIDs(Point), LatIDs(Point))
    END DO
  ELSE
    Met%LongwaveRad(:) = Met%ShortwaveRad(:)
  END IF

END SUBROUTINE get_meteorology

END MODULE meteorology_module
