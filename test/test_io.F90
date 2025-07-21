module io_tests

  use fortuno_interface_m, only: check, test_list, suite
  use datasetreader_module
  use time_module
  use netcdf

  implicit none

  type :: io_test_env
    integer :: start_year, end_year, dt
    character(len=50), dimension(:), allocatable :: nc_files
  contains
    final :: final_io_test_env
  end type io_test_env

contains

  function io_test_suite()
    !*## Purpose
    !
    ! Return the set of tests contained in the IO test suite.

    type(suite) :: io_tests

    io_tests = test_list([&
      suite("io_tests", test_list([&
        test("test_datasetreader", test_datasetreader),&
        test("test_output", test_output)&
      ]))&
    ])

  end function io_test_suite

  subroutine test_datasetreader()
    !*## Purpose
    !
    ! Test the behaviour of the DatasetReader.


  subroutine init_io_test_env(this)
    !*## Purpose
    !
    ! Initialise the test environment by creating a set of NetCDF files,
    ! created from repeatable function data.

    type(io_test_env), intent(out) :: this

    character(len=50) :: filename
    real, dimension(:,:,:), allocatable :: dummy_data
    integer, dimension(:), allocatable :: times
    integer :: nx, ny, dt, year, i, t, slice_start
    integer :: nc_id, x_id, yid, t_id, tvar_id, dvar_id, t

    nx = 20
    ny = 20
    dt = 12 * 3600
    start_year = 1980
    end_year = 1983

    ! Create 4 years of 20x20 dummy records at 12 hourly intervals
    dummy_data = create_dummy_data(start_year, end_year, nx, ny, dt)

    ! Put the data into NetCDF files, one year per file
    i = 1
    slice_start = 1
    do year = start_year, end_year
      ! Create the file name and create the new netcdf file
      write(filename, '(A10, I4, A3)') "dummy_data", year, ".nc"
      call nf90_create(filename, nf90_clobber, nc_id)
      this%nc_files(i) = filename

      ! Create the required dimensions
      call nf90_def_dim(ncid, "lon", nx, x_id)
      call nf90_def_dim(ncid, "lat", ny, y_id)
      call nf90_def_dim(ncid, "time", days_in_year(year) * 2, t_id)

      ! Set up the time variable- required for dataset reader to search
      call nf90_def_var(nc_id, "time", nf90_int, t_id, tvar_id)
      call nf90_put_att(nc_id, tvar_id, "units", "seconds since "//&
        time_as_string(start_year, 1, 1))
      
      ! Set up the dummy data variable
      call nf90_def_var(nc_id, "data", nf90_float, [x_id, y_id, t_id], dvar_id)
      
      ! Now assign the data
      call nf90_enddef(nc_id)

      times = [(t * dt, t = 1, days_in_year(year) * 2)]
      call nf90_put_var(nc_id, tvar_id, times)

      call nf90_put_var(nc_id, dvar_id,&
        dummy_data(:, :, slice_start:(days_in_year(year) * 2 - 1)))

      call nf90_close(nc_id)
    end do

    ! Fill in the rest of the data to be used by the tests
    this%start_year = start_year
    this%end_year = end_year
    this%dt = dt

  end subroutine init_io_test_env
      
  subroutine final_io_test_env(this)
    !*## Purpose
    !
    ! Tear down the test environment on completion/exit.

    type(io_test_env), intent(inout) :: this

    integer :: f, f_id, f_st

    do f = 1, size(this%nc_files)
      open(this%nc_files(f), newunit=f_id, status="old")
      close(f_id, status="delete")
    end do

  end subroutine final_io_test_env

  function create_dummy_data(start_year, end_year, nx, ny, dt)&
    RESULT(dummy_data)
    !*## Purpose
    !
    ! Create a set of dummy data going from start_year to end_year.
    
    integer, intent(in) :: start_year, end_year, nx, ny, dt
    real, dimension(:,:,:), allocatable :: dummy_data

    integer :: t, year, nt

    ! Work out how much space to allocate for the array
    t = 0
    do year = start_year, end_year
      t = t + days_in_year(year) * (SecsInDay / dt)
    end do

    ALLOCATE(dummy_data(nx, ny, nt))

    nt = 1
    do year = start_year, end_year
      do t = 1, (days_in_year(year) * (SecsInDay / dt))
        dummy_data(:,:,nt) = create_dummy_data_slice(nx, ny, year, t * dt)
      end do
    end do

  end function create_dummy_data

  function create_dummy_data_slice(nx, ny, year, t) RESULT(dummy_data_slice)
    !## Purpose
    !
    ! Create a slice of dummy data of size (nx, ny) using the year and time in
    ! seconds during the year.

    integer, intent(in) :: nx, ny, year, t
    real, dimension(nx,ny) :: dummy_data_slice
    integer :: i, j
    
    do j = 1, ny
      do i = 1, nx
        dummy_data_slice(i,j) = real(mod(year, 50)) / 50 +&
          real(mod(t, 1000)) / 1000 + real(mod(x, 10)) / 10 +&
          real(mod(y, 20)) / 20
      end do
    end do

  end function create_dummy_data_slice

end module io_tests
  !subroutine test_datasetreader()
    !!*## Purpose
    !!
    !! Test that the DatasetReader's behaviour is as expected.
    !!
    !!## Method
    !!
    !! Read a dummy dataset which extends over multiple files and multiple years
    !! and determine that it accesses data correctly.


  !USE datasetreader_module
  !USE time_module
  !USE partition_mod
  !USE mpi_module

  !INTEGER :: Year = 1965, StepIndex = 200

  !TYPE(DatasetReader) :: Reader
  !CHARACTER(LEN=*), DIMENSION(*), PARAMETER :: RainNames = &
    ![CHARACTER(LEN=5) :: 'rain', 'ps', 'Rain', 'Rainf']
  !CHARACTER(LEN=300) :: RainFiles

  !INTEGER :: FileIndex, IndexInFile

  !TYPE(ProcessDomain) :: ProcDomain
  !TYPE(mpi_grp_t) :: mpi_grp
  !LOGICAL, DIMENSION(:,:), ALLOCATABLE :: Landmask
  !INTEGER :: i, j

  !! Initialise the mpi
  !CALL mpi_mod_init()
  !mpi_grp = mpi_grp_t()

  !! Create a 4x4 landmask where every second element is land
  !ALLOCATE(Landmask(4,4))
  !DO j = 1, 4
    !DO i = 1, 4
      !Landmask(i, j) = MOD(i + j * 4, 2) == 0
    !END DO
  !END DO

  !RainFiles = "../../run_mock_cable/RainFiles"

  !! Prepare the domain
  !call partition_mod_init(Landmask, mpi_grp)
  !call get_grid_partition_start_count( &
    !shape(Landmask), &
    !mpi_grp%size, &
    !mpi_grp%rank, &
    !ProcDomain%ProcessDomainStart, &
    !ProcDomain%ProcessDomainSize &
  !)

  !Reader = initialise_datasetreader_at_timestep(RainFiles, RainNames, 10800.0,&
    !ProcDomain, mpi_grp)

  !CALL select_file(Reader, 5000, FileIndex, IndexInFile)

  !WRITE(*,*) "Selected file index:", FileIndex, " with index:", IndexInFile

  !CALL get_data(Reader, 1960, 200)

  !CALL mpi_mod_end()

END PROGRAM test_datasetreader
