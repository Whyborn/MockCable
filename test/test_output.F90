PROGRAM test_output

  USE output_module
  USE domain_module
  USE common_module, ONLY: handle_ncstat
  USE mpi_module

  IMPLICIT NONE

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

  ! Prepare the domain
  CALL prepare_process_domain(Landmask, ProcDomain, mpi_grp)

  CALL initialise_output_module(ProcDomain, mpi_grp)
  CALL test_single_unlimited_dimension()
  CALL test_put_data(ProcDomain, mpi_grp)
  CALL mpi_mod_end()

CONTAINS

SUBROUTINE test_single_unlimited_dimension()
  !* Purpose
  !
  ! Confirm that the output module can create a NetCDF file with a single
  ! unlimited dimension and write to it.

  TYPE(NCFile) :: TestNCFile

  TestNCFile = initialise_output_file("TestOutput.nc", ["time"],&
    [NF90_UNLIMITED])

  CALL def_variables(TestNCFile, "time", "time", NF90_FLOAT)

  CALL extend_unlimited_dimension(TestNCFile, "time", 1.0)

  CALL close_file(TestNCFile)

END SUBROUTINE test_single_unlimited_dimension

SUBROUTINE test_put_data(ProcDomainLoc, mpi_grp_loc)
  !* Purpose
  !
  ! Test adding a 2D spatial array of data to the file.

  ! Need the process domain to decide what shape the domain is
  TYPE(ProcessDomain), INTENT(IN) :: ProcDomainLoc
  TYPE(mpi_grp_t), INTENT(IN) :: mpi_grp_loc

  TYPE(NCFile) :: TestNCFile
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: DummyDataSpace, DummyDataRecord
  INTEGER :: t

  TestNCFile = initialise_output_file("TestOutput.nc", ["lon", "lat", "time"],&
    [4, 4, NF90_UNLIMITED])

  CALL def_variables(TestNCFile, "lon", "lon", NF90_INT)
  CALL def_variables(TestNCFile, "lat", "lat", NF90_INT)
  CALL def_variables(TestNCFile, "time", ["time"], NF90_INT)
  CALL def_variables(TestNCFile, "procs", ["lon", "lat"], NF90_INT)
  CALL def_variables(TestNCFile, "procstime", ["lon", "lat", "time"], NF90_INT)

  CALL put_dimension_data(TestNCFile, "lon", [1, 2, 3, 4])
  CALL put_dimension_data(TestNCFile, "lat", [1, 2, 3, 4])

  ! Fill in the dummy data
  ALLOCATE(DummyDataSpace(ProcDomainLoc%ProcessDomainSize(1),&
    ProcDomainLoc%ProcessDomainSize(2)),&
    DummyDataRecord(ProcDomainLoc%ProcessDomainSize(1),&
    ProcDomainLoc%ProcessDomainSize(2)))

  DummyDataSpace = mpi_grp_loc%rank

  CALL put_variable_data(TestNCFile, "procs", DummyDataSpace)

  DO t = 1, 5
    DummyDataRecord = DummyDataSpace * t
    CALL extend_unlimited_dimension(TestNCFile, "time", t)
    CALL put_record(TestNCFile, "procstime", DummyDataRecord, "time")
  END DO

  CALL close_file(TestNCFile)

END SUBROUTINE test_put_data

END PROGRAM test_output
