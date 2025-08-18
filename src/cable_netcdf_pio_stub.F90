module cable_netcdf_pio_mod
  use cable_netcdf_mod
  use mpi_module, only: mpi_grp_t
  use iso_fortran_env, only: error_unit
  use cable_netcdf_stub_types_mod, only: cable_netcdf_stub_io_t
  use cable_netcdf_stub_types_mod, only: cable_netcdf_pio_decomp_t => cable_netcdf_stub_decomp_t
  use cable_netcdf_stub_types_mod, only: cable_netcdf_pio_file_t => cable_netcdf_stub_file_t
  implicit none

  type, extends(cable_netcdf_stub_io_t) :: cable_netcdf_pio_io_t
  end type

  interface cable_netcdf_pio_io_t
    procedure cable_netcdf_pio_io_constructor
  end interface

contains

  function cable_netcdf_pio_io_constructor(mpi_grp) result(this)
    type(cable_netcdf_pio_io_t) :: this
    type(mpi_grp_t), intent(in) :: mpi_grp
    write(error_unit, *) "Error instantiating cable_netcdf_pio_io_t: PIO support not available"
    call mpi_grp%abort()
    this = cable_netcdf_pio_io_t()
  end function

end module cable_netcdf_pio_mod
