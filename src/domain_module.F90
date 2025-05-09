MODULE domain_module
! Contains information about the domain used, both in a computational and
! physical sense.

USE netcdf
USE mpi_module, ONLY: mpi_grp_t
#ifdef __MPI__
USE mpi_f08, ONLY: MPI_INFO_NULL
#endif
USE common_module, ONLY: handle_ncstat
use partition_mod, only: partition_mod_init, get_n_land
use partition_mod, only: get_grid_partition_start_count, land_index_local_to_ij_global


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
  INTEGER :: i_global, j_global, land_index, n_land

  ! IDs for netCDF io
  INTEGER :: ncID, LatID, LonID, MaskID, nLat, nLon

  ! The original mask from file
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: Landmask
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: LogicalLandmask

#ifdef __MPI__
  CALL handle_ncstat(NF90_OPEN(TRIM(LandmaskFile), NF90_NOWRITE, ncID, comm=mpi_grp%comm%MPI_Val, info=MPI_INFO_NULL%MPI_Val))
#else
  CALL handle_ncstat(NF90_OPEN(TRIM(LandmaskFile), NF90_NOWRITE, ncID))
#endif
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
  LogicalLandmask = Landmask == 1

  ! Assign the mask to the global domain descriptor
  GlobDomain%GlobalLandmask = LogicalLandmask

  ! Bind the global longitudes and latitudes to the global domain
  ALLOCATE(GlobDomain%LongitudeAxis(nLon), GlobDomain%LatitudeAxis(nLat))

  ! Get the longitude and latitude values and bind them to the global domain
  CALL handle_ncstat(NF90_INQ_VARID(ncID, 'longitude', LonID))
  CALL handle_ncstat(NF90_GET_VAR(ncID, LonID, GlobDomain%LongitudeAxis))
  
  CALL handle_ncstat(NF90_INQ_VARID(ncID, 'latitude', LatID))
  CALL handle_ncstat(NF90_GET_VAR(ncID, LatID, GlobDomain%LatitudeAxis))
  CALL handle_ncstat(NF90_CLOSE(ncID))

  call partition_mod_init(LogicalLandmask, mpi_grp)

  call get_grid_partition_start_count( &
    [nLon, nLat], &
    mpi_grp%size, &
    mpi_grp%rank, &
    ProcDomain%ProcessDomainStart, &
    ProcDomain%ProcessDomainSize &
  )

  n_land = get_n_land(mpi_grp)

  allocate( &
    ProcDomain%Longitudes(n_land), &
    ProcDomain%Latitudes(n_land) &
  )

  do land_index = 1, n_land
    call land_index_local_to_ij_global(mpi_grp, land_index, i_global, j_global)
    ProcDomain%Longitudes(land_index) = GlobDomain%LongitudeAxis(i_global)
    ProcDomain%Latitudes(land_index) = GlobDomain%LatitudeAxis(j_global)
  end do

END SUBROUTINE process_landmask

END MODULE domain_module
