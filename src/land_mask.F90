module land_mask_mod
  use cable_netcdf_mod
  implicit none

  private

  public :: &
    land_mask_t, &
    land_mask_init

  integer, parameter :: LAND_VAL = 1

  type land_mask_t
    integer :: n_land_global
    integer :: n_lon, n_lat
    logical, allocatable :: mask(:,:)
    real, allocatable :: longitude(:), latitude(:)
  end type

contains

  subroutine land_mask_init(land_mask_file_name, land_mask)
    !! Initialise land mask data structure
    character(*), intent(in) :: land_mask_file_name
    type(land_mask_t), intent(out) :: land_mask
    class(cable_netcdf_file_t), allocatable :: file
    integer, allocatable :: land_mask_integer(:,:)

    integer :: land_index, grid_offset, grid_index(2), grid_shape(2)

    file = cable_netcdf_open_file(trim(land_mask_file_name))

    call file%inq_dim_len("longitude", land_mask%n_lon)
    call file%inq_dim_len("latitude", land_mask%n_lat)

    allocate(land_mask_integer(land_mask%n_lon, land_mask%n_lat))
    call file%get_var("mask", land_mask_integer)
    land_mask%mask = land_mask_integer == LAND_VAL
    land_mask%n_land_global = count(land_mask%mask)

    allocate(land_mask%longitude(land_mask%n_lon))
    call file%get_var("longitude", land_mask%longitude)

    allocate(land_mask%latitude(land_mask%n_lat))
    call file%get_var("latitude", land_mask%latitude)

    call file%close()

  end subroutine

end module
