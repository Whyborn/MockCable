MODULE domain_module
! Contains information about the domain used, both in a computational and
! physical sense.

USE netcdf
USE mpi_module, ONLY: mpi_grp_t
USE common_module, ONLY: handle_ncstat


IMPLICIT NONE

TYPE ProcessDomain
  !# Purpose
  !
  ! Contains information required for the worker to coordinate MPI I/O
  ! operations. To facilitate parallel I/O, all the worker needs is to know the
  ! start indices and the size of the block. To convert from the 2D surface to
  ! the 1D vectors, it also contains a map of longitude/latitude axis indices
  ! to indices in the 1D vectors used by the CABLE physics routines.

  INTEGER, DIMENSION(2) :: ProcessDomainStart, ProcessDomainSize
  INTEGER, DIMENSION(:), ALLOCATABLE :: LongitudeIDs, LatitudeIDs
  REAL, DIMENSION(:), ALLOCATABLE :: Longitudes, Latitudes
END TYPE ProcessDomain

TYPE GlobalDomain
  !# Purpose
  !
  ! Tells processes information about the global domain. Contains the longitude
  ! and latitude axes used to facilitate writing axis data for output and the
  ! original land mask used to construct the domain

  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: GlobalLandmask
  REAL, DIMENSION(:), ALLOCATABLE :: LongitudeAxis, LatitudeAxis
END TYPE GlobalDomain

CONTAINS

SUBROUTINE process_landmask(LandmaskFile, ProcDomain, GlobDomain, mpi_grp)
  !*## Purpose
  !
  ! Process the landmask to set up everything we need to know about the domain
  ! globally and per-process.
  !
  !## Method
  !
  ! Read the landmask netCDF. Use the contained landmask to separate the domain
  ! by process, and then add coordinate information using the landmask axes.

  CHARACTER(LEN=*), INTENT(IN) :: LandmaskFile
  TYPE(ProcessDomain), INTENT(OUT) :: ProcDomain
  TYPE(GlobalDomain), INTENT(OUT) :: GlobDomain
  TYPE(mpi_grp_t), INTENT(IN) :: mpi_grp

  ! Iterators and counters
  INTEGER :: Lat, Lon, Point

  ! IDs for netCDF io
  INTEGER :: ncID, LatID, LonID, MaskID, nLat, nLon

  ! The original mask from file
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: Landmask
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: LogicalLandmask

  CALL handle_ncstat(NF90_OPEN(TRIM(LandmaskFile), NF90_NOWRITE, ncID))
  CALL handle_ncstat(NF90_INQ_DIMID(ncID, 'longitude', LonID))
  CALL handle_ncstat(NF90_INQUIRE_DIMENSION(ncID, LonID, LEN=nLon))

  CALL handle_ncstat(NF90_INQ_DIMID(ncID, 'latitude', LatID))
  CALL handle_ncstat(NF90_INQUIRE_DIMENSION(ncID, LatID, LEN=nLat))

  ! Allocate memory for the mask and met variables
  ALLOCATE(Landmask(nLon, nLat), LogicalLandmask(nLon, nLat))

  ! Read the mask
  CALL handle_ncstat(NF90_INQ_VARID(ncID, 'mask', MaskID))
  CALL handle_ncstat(NF90_GET_VAR(ncID, MaskID, Landmask, START=[1, 1]))

  ! Convert from integers to logicals
  DO Lat = 1, nLat
    DO Lon = 1, nLon
      LogicalLandmask(Lon, Lat) = Landmask(Lon, Lat) == 1
    END DO
  END DO

  ! Assign the mask to the global domain descriptor
  GlobDomain%GlobalLandmask = LogicalLandmask

  ! Use the mask to build the per process domains
  CALL prepare_process_domain(LogicalLandmask, ProcDomain, mpi_grp)

  ! Bind the global longitudes and latitudes to the global domain
  ALLOCATE(GlobDomain%LongitudeAxis(nLon), GlobDomain%LatitudeAxis(nLat))

  ! Get the longitude and latitude values and bind them to the global domain
  CALL handle_ncstat(NF90_INQ_VARID(ncID, 'longitude', LonID))
  CALL handle_ncstat(NF90_GET_VAR(ncID, LonID, GlobDomain%LongitudeAxis))
  
  CALL handle_ncstat(NF90_INQ_VARID(ncID, 'latitude', LatID))
  CALL handle_ncstat(NF90_GET_VAR(ncID, LatID, GlobDomain%LatitudeAxis))

  ! And now get the relevant latitudes and longitudes for the process, by
  ! adding the index in the process-local coordinate to the starting indices
  ! for the process
  DO Point = 1, SIZE(ProcDomain%LongitudeIDs)
    ProcDomain%Longitudes(Point) = GlobDomain%LongitudeAxis(&
      ProcDomain%ProcessDomainStart(1) + ProcDomain%LongitudeIDs(Point))
    ProcDomain%Latitudes(Point) = GlobDomain%LatitudeAxis(&
      ProcDomain%ProcessDomainStart(2) + ProcDomain%LatitudeIDs(Point))
  END DO

END SUBROUTINE process_landmask

SUBROUTINE prepare_process_domain(Landmask, ProcDomain, mpi_grp)
  !*## Purpose
  !
  ! Prepare the per-process domain using the given landmask and MPI config.
  !
  !## Method
  !
  ! Inspect the dimensions of the given landmask, combined with the size of the
  ! MPI world, to subdivide the landmask into chunks.

  LOGICAL, DIMENSION(:,:), INTENT(IN) :: Landmask
  TYPE(ProcessDomain), INTENT(OUT) :: ProcDomain
  TYPE(mpi_grp_t), INTENT(IN) :: mpi_grp

  ! Domain chunkers and iterators
  INTEGER :: LonSize, LatSize, ChunkSize, NPoints, Counter, Lat, Lon

  ! Domain specific mask
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: LocalMask

  ! For now, just divide up along the longitude axis
  LonSize = SIZE(Landmask, 1)
  LatSize = SIZE(Landmask, 2)
  !ChunkSize = LonSize / mpi_grp%size
  ChunkSize = LatSize / mpi_grp%size

  ! Set the process domain
  !ProcDomain%ProcessDomainStart = [1 + mpi_grp%rank * ChunkSize, 1]
  ProcDomain%ProcessDomainStart = [1, 1 + mpi_grp%rank * ChunkSize]

  ! When setting the size of the process domain, we need to make sure we get
  ! the last chunk right, as it may not be exactly divisible by the chunk size
  !ProcDomain%ProcessDomainSize = [&
      !MIN(ChunkSize, LonSize - ProcDomain%ProcessDomainStart(1) + 1), LatSize]
  ProcDomain%ProcessDomainSize = [&
      LonSize, MIN(ChunkSize, LatSize - ProcDomain%ProcessDomainStart(2) + 1)]

  ! Now we have the process range, we can assign it's 2D -> 1D mapping
  ! Start by taking the relevant slice of the landmask
  LocalMask = Landmask(&
    ProcDomain%ProcessDomainStart(1):ProcDomain%ProcessDomainStart(1) +&
    ProcDomain%ProcessDomainSize(1) - 1,&
    ProcDomain%ProcessDomainStart(2):ProcDomain%ProcessDomainStart(2) +&
    ProcDomain%ProcessDomainSize(2) - 1)

  ! How many land points are there in the process's mask?
  NPoints = COUNT(LocalMask)

  ! Allocate memory for the mapping
  ALLOCATE(ProcDomain%LongitudeIDs(NPoints), ProcDomain%LatitudeIDs(NPoints),&
    ProcDomain%Longitudes(NPoints), ProcDomain%Latitudes(NPoints))

  ! Read the landmask chunk to assign the mapping
  Counter = 1
  DO Lat = 1, SIZE(LocalMask, 2)
    DO Lon = 1, SIZE(LocalMask, 1)
      IF (LocalMask(Lon, Lat)) THEN
        ProcDomain%LongitudeIDs(Counter) = Lon
        ProcDomain%LatitudeIDs(Counter) = Lat
        Counter = Counter + 1
      END IF
    END DO
  END DO

END SUBROUTINE prepare_process_domain

END MODULE domain_module
