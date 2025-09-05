module land_decomp_mod
  use land_mask_mod, only: land_mask_t, land_mask_init
  use array_utils_mod, only: array_offset, array_index
  use cable_netcdf_mod, only: cable_netcdf_create_decomp, cable_netcdf_decomp_t
  use mpi_module, only: mpi_grp_t
  implicit none

  private

  public :: &
    land_decomp_t, &
    land_decomp_init, &
    io_decomp_grid

  type land_decomp_t
    integer :: land_start !! Starting land point index in global land array.
    integer :: land_count !! Number of land points for this rank.
    integer, allocatable :: land_grid_offset(:) !! Grid offsets of each land point in global domain.
    type(land_mask_t) :: land_mask
  end type

contains

  subroutine land_decomp_init(land_mask_file_name, mpi_grp, land_decomp)
    character(*), intent(in) :: land_mask_file_name
    type(mpi_grp_t), intent(in) :: mpi_grp
    type(land_decomp_t), intent(out) :: land_decomp

    integer :: i, grid_index(2)

    call land_mask_init(land_mask_file_name, land_decomp%land_mask)

    ! Note: work is distributed by allocating land points to each rank where the
    ! difference between the number of land points assigned to different ranks
    ! is at most 1. This leads to load balance issues because calculations are
    ! done on "patches" whose number differ between landpoints. This can be
    ! improved in future by distributing the compute based on patches with
    ! additional care taken when aggregating patches for each grid cell.

    call init_global_decomp_path(land_decomp%land_mask, land_decomp%land_grid_offset)

    call get_partition_start_count( &
      land_decomp%land_mask%n_land_global, &
      mpi_grp%size, &
      mpi_grp%rank, &
      land_decomp%land_start, &
      land_decomp%land_count &
    )

  end subroutine

  subroutine init_global_decomp_path(land_mask, land_grid_offset)
    type(land_mask_t), intent(in) :: land_mask
    integer, allocatable, intent(out) :: land_grid_offset(:)
    integer :: land_index, grid_offset, grid_shape(2), grid_index(2)

    land_index = 1
    grid_shape = shape(land_mask%mask)
    allocate(land_grid_offset(land_mask%n_land_global))
    do grid_offset = 1, size(land_mask%mask)
      call array_index(grid_offset, grid_shape, grid_index)
      if (.not. land_mask%mask(grid_index(1), grid_index(2))) cycle
      land_grid_offset(land_index) = grid_offset
      land_index = land_index + 1
    end do
    
  end subroutine

  function io_decomp_grid(land_decomp, mem_shape, type) result(decomp)
    type(land_decomp_t), intent(in) :: land_decomp
    integer, intent(in) :: mem_shape(:), type
    class(cable_netcdf_decomp_t), allocatable :: decomp

    integer, allocatable :: compmap(:), mem_index(:), grid_index(:), grid_shape(:)
    integer :: mem_offset, grid_offset, land_index, x_index, y_index

    allocate(compmap(product(mem_shape)))
    allocate(mem_index(size(mem_shape)))
    allocate(grid_index(size(mem_shape) + 1))
    allocate(grid_shape(size(mem_shape) + 1))

    land_index = ubound(mem_shape, 1)
    x_index = land_index
    y_index = land_index + 1

    grid_shape(:land_index) = mem_shape(:land_index)
    grid_shape(x_index:y_index) = shape(land_decomp%land_mask%mask)

    do mem_offset = 1, size(compmap)
      call array_index(mem_offset, mem_shape, mem_index)
      grid_offset = land_decomp%land_grid_offset(land_decomp%land_start + mem_index(land_index) - 1)
      grid_index(:land_index) = mem_index(:land_index)
      call array_index(grid_offset, grid_shape, grid_index(x_index:y_index))
      compmap(mem_offset) = array_offset(grid_index, grid_shape)
    end do

    decomp = cable_netcdf_create_decomp(compmap, grid_shape, type)

  end function

  subroutine get_partition_start_count(n, k, p, start, count)
    !* Compute start and count for the p'th partition of an array of size n
    ! where p = 0, 1, ... , k - 1.
    !
    ! For k partitions, an array of n elements can be partitioned into r
    ! partitions of length q + 1, and k - r partitions of length q where q and r
    ! are the quotient and remainder of n divided by k (i.e. n = q * k + r).
    ! Note, we assume that the r partitions of length q + 1 precede the k - r
    ! partitions of length q in the array.
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

end module
