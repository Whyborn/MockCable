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

SUBROUTINE prepare_meteorology(Timestep, NPoints, Met, mpi_grp)
  !*## Purpose
  !
  ! Prepare the meteorology input routines and land mask
  !
  !## Method
  !
  ! Read the met.nml to determine the locations of the meteorology and landmask
  ! files.

  REAL, INTENT(IN) :: Timestep
  INTEGER, INTENT(OUT) :: NPoints
  TYPE(MetType), INTENT(OUT) :: Met
  TYPE(mpi_grp_t), INTENT(IN) :: mpi_grp

  CHARACTER(LEN=300) :: RainFile, TemperatureFile, WindFile, PressureFile,&
    ShortwaveRadFile, LongwaveRadFile, LandmaskFile

  INTEGER :: nmlUnit

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

  NPoints = prepare_landmask(LandmaskFile)

  ! Allocate memory
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

END SUBROUTINE prepare_meteorology

FUNCTION prepare_landmask(LandmaskFile) RESULT(NPoints)
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

  CHARACTER(LEN=300) :: LandmaskFile
  INTEGER :: NPoints

  ! Iterators
  INTEGER :: Lat, Lon, Counter, VarIter

  ! IDs for netCDF io
  INTEGER :: ncID, LatID, LonID, MaskID, nLat, nLon, ok

  ! The mask
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: Landmask

  ok = NF90_OPEN(TRIM(LandmaskFile), NF90_NOWRITE, ncID)
  CALL handle_ncstat(ok)
  ok = NF90_INQ_DIMID(ncID, 'longitude', LonID)
  CALL handle_ncstat(ok)
  ok = NF90_INQUIRE_DIMENSION(ncID, LonID, LEN=nLon)
  CALL handle_ncstat(ok)

  ok = NF90_INQ_DIMID(ncID, 'latitude', LatID)
  CALL handle_ncstat(ok)
  ok = NF90_INQUIRE_DIMENSION(ncID, LatID, LEN=nLat)
  CALL handle_ncstat(ok)

  ! Allocate memory for the mask and met variables
  WRITE(*,*) "nLon:", nLon, "nLat:", nLat
  ALLOCATE(Landmask(nLon, nLat))
  DO VarIter = 1, NumVariables
    ALLOCATE(MetContainers(VarIter)%VarData(nLon, nLat))
  END DO

  ! Read the mask
  ok = NF90_INQ_VARID(ncID, 'mask', MaskID)
  CALL handle_ncstat(ok)
  ok = NF90_GET_VAR(ncID, MaskID, Landmask, START=[1, 1])
  CALL handle_ncstat(ok)

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

END FUNCTION prepare_landmask

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
