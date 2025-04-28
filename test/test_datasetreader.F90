PROGRAM test_datasetreader

  USE datasetreader_module
  USE time_module
  USE partition_mod
  USE mpi_module


  INTEGER :: Year = 1965, StepIndex = 200

  TYPE(DatasetReader) :: Reader
  CHARACTER(LEN=*), DIMENSION(*), PARAMETER :: RainNames = &
    [CHARACTER(LEN=5) :: 'rain', 'ps', 'Rain', 'Rainf']
  CHARACTER(LEN=300) :: RainFiles

  INTEGER :: FileIndex, IndexInFile

  TYPE(ProcessDomain) :: ProcDomain
  TYPE(mpi_grp_t) :: mpi_grp
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: Landmask
  INTEGER :: i, j

  ! Initialise the mpi
  CALL mpi_mod_init()
  mpi_grp = mpi_grp_t()

  ! Create a 4x4 landmask where every second element is land
  ALLOCATE(Landmask(4,4))
  DO j = 1, 4
    DO i = 1, 4
      Landmask(i, j) = MOD(i + j * 4, 2) == 0
    END DO
  END DO

  RainFiles = "../../run_mock_cable/RainFiles"

  ! Prepare the domain
  call partition_mod_init(Landmask, mpi_grp)
  call get_grid_partition_start_count( &
    shape(Landmask), &
    mpi_grp%size, &
    mpi_grp%rank, &
    ProcDomain%ProcessDomainStart, &
    ProcDomain%ProcessDomainSize &
  )

  Reader = initialise_datasetreader_at_timestep(RainFiles, RainNames, 10800.0,&
    ProcDomain, mpi_grp)

  CALL select_file(Reader, 5000, FileIndex, IndexInFile)

  WRITE(*,*) "Selected file index:", FileIndex, " with index:", IndexInFile

  CALL get_data(Reader, 1960, 200)

  CALL mpi_mod_end()

END PROGRAM test_datasetreader
