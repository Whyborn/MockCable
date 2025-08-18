program test_cable_netcdf_write_darray
  use mpi_module, only: mpi_grp_t, mpi_mod_init, mpi_mod_end
  use array_utils_mod, only: array_offset, array_index
  use cable_netcdf_mod

  implicit none

  type land_decomp_t
    integer :: n_land_global, x_size, y_size
    integer :: land_start, land_count
    integer, allocatable :: land_x(:), land_y(:)
  end type

  integer, parameter :: N = 4, FILL = 0

  integer :: i, j, k, grid_index, offset, n_non_zero, partition_start, partition_count, values(N, N)
  integer, allocatable :: compmap(:), local_array(:)
  logical :: land_mask(N, N)

  type(mpi_grp_t) :: mpi_grp
  class(cable_netcdf_decomp_t), allocatable :: decomp
  class(cable_netcdf_file_t), allocatable :: file
  type(land_decomp_t) :: land_decomp

  call mpi_mod_init()
  mpi_grp = mpi_grp_t()

  ! values:
  !        i
  !   +-----------+
  !   | 1  .  . 4 |
  ! j | 5  6  7 . | = [1, 0, 0, 4, 5, 6, 7, 0, 0, 10, 11, 0, 0, 14, 15, 0]
  !   | . 10 11 . |
  !   | . 14 15 . |
  !   +-----------+

  do j = 1, N
    do i = 1, N
      grid_index = (i - 1) + (j - 1) * N + 1
      select case(grid_index)
      case(1, 4, 5, 6, 7, 10, 11, 14, 15)
        values(i, j) = grid_index
      case default
        values(i, j) = FILL
      end select
    end do
  end do

  land_mask = values /= FILL

  call init_land_decomp(land_mask, mpi_grp, land_decomp)
  call init_compmap_xy_grid(land_decomp, [land_decomp%land_count], compmap)

  local_array = compmap

  call cable_netcdf_mod_init(mpi_grp)

  file = cable_netcdf_create_file("file.nc")
  decomp = cable_netcdf_create_decomp(compmap, dims=[N, N], type=CABLE_NETCDF_INT)
  call file%def_dims(["i", "j", "t"], [N, N, CABLE_NETCDF_UNLIMITED])
  call file%def_var("values", ["i", "j", "t"], CABLE_NETCDF_INT)
  call file%end_def()
  call file%write_darray("values", local_array, decomp, fill_value=0, frame=1)
  call file%write_darray("values", local_array, decomp, fill_value=0, frame=2)
  call file%close()

  call cable_netcdf_mod_end()

  call mpi_mod_end()

contains

  subroutine get_partition_start_count(n, k, p, start, count)
    integer, intent(in) :: n, k, p
    integer, intent(out) :: start, count
    integer :: q, r

    q = n / k
    r = mod(n, k)

    if (p < r) then
      count = q + 1
      start = 1 + (q + 1) * p
    else
      count = q
      start = 1 + (q + 1) * r + q * (p - r)
    end if

  end subroutine get_partition_start_count

  subroutine init_land_decomp(land_mask, mpi_grp, land_decomp)
    logical, intent(in) :: land_mask(:,:)
    type(mpi_grp_t), intent(in) :: mpi_grp
    type(land_decomp_t), intent(out) :: land_decomp
    integer :: i, j, land_index

    land_decomp%n_land_global = count(land_mask)
    land_decomp%x_size = size(land_mask, 1)
    land_decomp%y_size = size(land_mask, 2)
    allocate(land_decomp%land_x(land_decomp%n_land_global))
    allocate(land_decomp%land_y(land_decomp%n_land_global))

    land_index = 1
    do j = 1, size(land_mask, 2)
      do i = 1, size(land_mask, 1)
        if (.not. land_mask(i, j)) cycle
        land_decomp%land_x(land_index) = i
        land_decomp%land_y(land_index) = j
        land_index = land_index + 1
      end do
    end do

    call get_partition_start_count( &
      land_decomp%n_land_global, &
      mpi_grp%size, &
      mpi_grp%rank, &
      land_decomp%land_start, &
      land_decomp%land_count &
    )

  end subroutine

  subroutine init_compmap_xy_grid(land_decomp, mem_shape, compmap)
    type(land_decomp_t), intent(in) :: land_decomp
    integer, intent(in) :: mem_shape(:)
    integer, allocatable, intent(out) :: compmap(:)
    integer, allocatable :: mem_index(:), grid_index(:), grid_shape(:)
    integer :: mem_offset, land_index, x_index, y_index

    allocate(compmap(product(mem_shape)))
    allocate(mem_index(size(mem_shape)))
    allocate(grid_index(size(mem_shape) + 1))
    allocate(grid_shape(size(mem_shape) + 1))

    land_index = ubound(mem_shape, 1)
    x_index = land_index
    y_index = land_index + 1

    grid_shape(:land_index) = mem_shape(:land_index)
    grid_shape(x_index) = land_decomp%x_size
    grid_shape(y_index) = land_decomp%y_size

    do mem_offset = 1, size(compmap)
      call array_index(mem_offset, mem_shape, mem_index)
      grid_index(:land_index) = mem_index(:land_index)
      grid_index(x_index) = land_decomp%land_x(land_decomp%land_start + mem_index(land_index) - 1)
      grid_index(y_index) = land_decomp%land_y(land_decomp%land_start + mem_index(land_index) - 1)
      compmap(mem_offset) = array_offset(grid_index, grid_shape)
    end do

  end subroutine

end program test_cable_netcdf_write_darray
