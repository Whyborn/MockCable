module fixtures_mod
  use fortuno_interface_m, only: test_case_t, test_item, check, check_failed, global_comm, num_ranks, this_rank
  use mpi_module, only: mpi_grp_t, MPI_COMM_UNDEFINED
  use file_utils, only: file_delete
  use cable_netcdf_mod, only: cable_netcdf_io_t, CABLE_NETCDF_FILE_MAX_STR_LEN
  use cable_netcdf_nf90_mod, only: cable_netcdf_nf90_io_t
  use cable_netcdf_pio_mod, only: cable_netcdf_pio_io_t
  implicit none

  private

  public :: test_case_nf90, test_case_pio

  character(len=CABLE_NETCDF_FILE_MAX_STR_LEN), parameter :: nc_file_name = "file.nc"

  type, extends(test_case_t) :: test_case_cable_netcdf_nf90_t
    procedure(cable_netcdf_test_interface), pointer, nopass :: test
  contains
    procedure :: run => run_test_cable_netcdf_nf90
  end type test_case_cable_netcdf_nf90_t

  type, extends(test_case_t) :: test_case_cable_netcdf_pio_t
    procedure(cable_netcdf_test_interface), pointer, nopass :: test
  contains
    procedure :: run => run_test_pio
  end type test_case_cable_netcdf_pio_t

  abstract interface
    subroutine cable_netcdf_test_interface(io_handler, file_name)
      import cable_netcdf_io_t
      class(cable_netcdf_io_t), intent(inout) :: io_handler
      character(*), intent(in) :: file_name
    end subroutine
  end interface

contains

  function test_case_nf90(name, test) result(testitem)
    character(*), intent(in) :: name
    procedure(cable_netcdf_test_interface) :: test
    type(test_item) :: testitem
    testitem = test_item(test_case_cable_netcdf_nf90_t(name=name, test=test))
  end function test_case_nf90

  subroutine run_test_cable_netcdf_nf90(this)
    class(test_case_cable_netcdf_nf90_t), intent(in) :: this
    class(cable_netcdf_io_t), allocatable :: io_handler
    call check(num_ranks() == 1, msg="NetCDF tests must be run with a single process.")
    if (check_failed()) return
    io_handler = cable_netcdf_nf90_io_t()
    call io_handler%init()
    call this%test(io_handler, nc_file_name)
    call io_handler%finalise()
    call file_delete(nc_file_name)
  end subroutine run_test_cable_netcdf_nf90

  function test_case_pio(name, test) result(testitem)
    character(*), intent(in) :: name
    procedure(cable_netcdf_test_interface) :: test
    type(test_item) :: testitem
    testitem = test_item(test_case_cable_netcdf_pio_t(name=name, test=test))
  end function test_case_pio

  subroutine run_test_pio(this)
    class(test_case_cable_netcdf_pio_t), intent(in) :: this
    class(cable_netcdf_io_t), allocatable :: io_handler
    type(mpi_grp_t) :: mpi_grp
    logical :: mpi_comm_defined
    mpi_grp = mpi_grp_t(global_comm())
    call check(&
      mpi_grp%comm%mpi_val /= MPI_COMM_UNDEFINED%mpi_val,&
      msg="MPI communicator must be defined for PIO tests"&
    )
    if (.not. check_failed()) then
      io_handler = cable_netcdf_pio_io_t(mpi_grp)
      call io_handler%init()
      call this%test(io_handler, nc_file_name)
      call io_handler%finalise()
    end if
    call mpi_grp%barrier()
    if (this_rank() == 0) call file_delete(nc_file_name)
  end subroutine run_test_pio

end module fixtures_mod