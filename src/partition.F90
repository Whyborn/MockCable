module partition_mod

#ifdef __MPI__
  use mpi_f08, only: MPI_REAL, MPI_Alltoallv
#endif
  use mpi_module, only: mpi_grp_t

  implicit none

  private
  public :: &
    partition_mod_init, &
    partition_mod_end, &
    transform_grid_to_land, &
    transform_land_to_grid, &
    get_n_land, &
    get_grid_partition_index, &
    get_grid_partition_start_count, &
    rectangular_partitioning

  integer, parameter :: INDEX_UNDEFINED = -1
  ! TODO(Sean): this should not be defined in this module
  real, parameter :: DEFAULT_FILL_VALUE = 0.0

  type partition_transfer_t
    !* Derived type containing counts and displacements for sending and
    ! receiving with MPI_Alltoallv.
    integer, dimension(:), allocatable :: counts_send
    integer, dimension(:), allocatable :: counts_recv
    integer, dimension(:), allocatable :: displacements_send
    integer, dimension(:), allocatable :: displacements_recv
  end type partition_transfer_t

  integer :: n_land !! Total number of land points in simulation domain
  integer, dimension(2) :: grid_shape_global !! 2D array shape of simulation domain
  logical :: rectangular_partitioning = .true.
    !! Enable rectangular partitioning of spatial grid, otherwise sliced partitioning is used.

  integer, dimension(:), allocatable :: grid_index_to_land_index
    !! Array of indexes in the global land vector for each grid point index (memory offset)
  integer, dimension(:), allocatable :: land_index_to_grid_index
    !! Array of indexes (memory offsets) in the global grid for each land point index
  integer, dimension(:), allocatable :: grid_partition_land_index_start
    !! Array of starting indexes in the global land vector for each grid partition
  integer, dimension(:), allocatable :: grid_partition_land_index_count
    !* Array of counts in the global land vector for each grid partition where
    ! each element contains the number of land points in the partition

  type(partition_transfer_t) :: partition_transfer_grid_to_land, partition_transfer_land_to_grid

contains

  subroutine partition_mod_init(mask_global, mpi_grp)
    !! Initialise data structures required for partition transfer.
    logical, dimension(:,:), allocatable, intent(in) :: mask_global !! Land mask of simulation domain
    type(mpi_grp_t), intent(in) :: mpi_grp !! MPI group

    integer, dimension(2) :: grid_partition_start, grid_partition_count
    integer :: i, j, rank
    integer :: land_index_global, land_index_start, land_index_count, land_index_end
    integer :: grid_index_global, grid_index_local

    grid_shape_global = shape(mask_global)

    n_land = count(mask_global)

    allocate( &
      grid_index_to_land_index(product(grid_shape_global)), &
      land_index_to_grid_index(n_land), &
      source=INDEX_UNDEFINED &
    )

    allocate( &
      partition_transfer_grid_to_land%counts_send(mpi_grp%size), &
      partition_transfer_grid_to_land%counts_recv(mpi_grp%size), &
      partition_transfer_grid_to_land%displacements_send(mpi_grp%size), &
      partition_transfer_grid_to_land%displacements_recv(mpi_grp%size), &
      partition_transfer_land_to_grid%counts_send(mpi_grp%size), &
      partition_transfer_land_to_grid%counts_recv(mpi_grp%size), &
      partition_transfer_land_to_grid%displacements_send(mpi_grp%size), &
      partition_transfer_land_to_grid%displacements_recv(mpi_grp%size), &
      source=0 &
    )

    allocate(grid_partition_land_index_start(mpi_grp%size), source=INDEX_UNDEFINED)
    allocate(grid_partition_land_index_count(mpi_grp%size), source=0)

    land_index_global = 1
    do rank = 0, mpi_grp%size - 1
      call get_grid_partition_start_count(grid_shape_global, mpi_grp%size, rank, grid_partition_start, grid_partition_count)
      do grid_index_local = 1, product(grid_partition_count)
        grid_index_global = grid_index_local_to_global(grid_shape_global, grid_partition_start, grid_partition_count, grid_index_local)
        call grid_index_to_ij(grid_index_global, grid_shape_global, i, j)
        if (.not. mask_global(i, j)) cycle
        grid_index_to_land_index(grid_index_global) = land_index_global
        land_index_to_grid_index(land_index_global) = grid_index_global
        if (grid_partition_land_index_start(rank + 1) == INDEX_UNDEFINED) then
          grid_partition_land_index_start(rank + 1) = land_index_global
        end if
        grid_partition_land_index_count(rank + 1) = land_index_global - grid_partition_land_index_start(rank + 1) + 1
        land_index_global = land_index_global + 1
      end do
    end do

    land_index_start = grid_partition_land_index_start(mpi_grp%rank + 1)
    land_index_count = grid_partition_land_index_count(mpi_grp%rank + 1)
    land_index_end = land_index_start + land_index_count - 1
    do land_index_global = land_index_start, land_index_end
      rank = get_partition_index(n_land, mpi_grp%size, land_index_global)
      partition_transfer_grid_to_land%counts_send(rank + 1) = partition_transfer_grid_to_land%counts_send(rank + 1) + 1
    end do

    call get_partition_start_count(n_land, mpi_grp%size, mpi_grp%rank, land_index_start, land_index_count)
    land_index_end = land_index_start + land_index_count - 1
    do land_index_global = land_index_start, land_index_end
      grid_index_global = land_index_to_grid_index(land_index_global)
      rank = get_grid_partition_index(grid_shape_global, grid_index_global, mpi_grp%size)
      partition_transfer_grid_to_land%counts_recv(rank + 1) = partition_transfer_grid_to_land%counts_recv(rank + 1) + 1
    end do

    do rank = 1, mpi_grp%size - 1
      partition_transfer_grid_to_land%displacements_send(rank + 1) &
        = partition_transfer_grid_to_land%displacements_send(rank) + partition_transfer_grid_to_land%counts_send(rank)
      partition_transfer_grid_to_land%displacements_recv(rank + 1) &
        = partition_transfer_grid_to_land%displacements_recv(rank) + partition_transfer_grid_to_land%counts_recv(rank)
    end do

    partition_transfer_land_to_grid%counts_send = partition_transfer_grid_to_land%counts_recv
    partition_transfer_land_to_grid%counts_recv = partition_transfer_grid_to_land%counts_send
    partition_transfer_land_to_grid%displacements_send = partition_transfer_grid_to_land%displacements_recv
    partition_transfer_land_to_grid%displacements_recv = partition_transfer_grid_to_land%displacements_send

  end subroutine partition_mod_init

  subroutine partition_mod_end()
    !! Deallocate data structures required for partition transfer.

    deallocate( &
      grid_index_to_land_index, &
      land_index_to_grid_index, &
      grid_partition_land_index_start, &
      grid_partition_land_index_count, &
      partition_transfer_grid_to_land%counts_send, &
      partition_transfer_grid_to_land%counts_recv, &
      partition_transfer_grid_to_land%displacements_send, &
      partition_transfer_grid_to_land%displacements_recv, &
      partition_transfer_land_to_grid%counts_send, &
      partition_transfer_land_to_grid%counts_recv, &
      partition_transfer_land_to_grid%displacements_send, &
      partition_transfer_land_to_grid%displacements_recv &
    )

  end subroutine partition_mod_end

  subroutine transform_grid_to_land(mpi_grp, grid_partition_start, grid_partition_count, grid_input, land_output)
    !! Perform grid to land partition transfer.
    type(mpi_grp_t), intent(in) :: mpi_grp !! MPI group
    integer, dimension(2), intent(in) :: grid_partition_start !! 2D start indexes of grid partition
    integer, dimension(2), intent(in) :: grid_partition_count !! 2D counts of grid partition
    real, dimension(:, :), allocatable, intent(in) :: grid_input !! Input grid
    real, dimension(:), allocatable, intent(out) :: land_output !! Output land vector

    real, dimension(:), allocatable :: buffer_send_grid_to_land
    integer :: i, j, send_buffer_index, land_index_global, land_index_start, land_index_count
    integer :: grid_index_local, grid_index_global

    allocate(buffer_send_grid_to_land(sum(partition_transfer_grid_to_land%counts_send)))
    allocate(land_output(sum(partition_transfer_grid_to_land%counts_recv)))

    land_index_start = grid_partition_land_index_start(mpi_grp%rank + 1)
    land_index_count = grid_partition_land_index_count(mpi_grp%rank + 1)
    do send_buffer_index = 1, land_index_count
      land_index_global = land_index_start + send_buffer_index - 1
      grid_index_global = land_index_to_grid_index(land_index_global)
      grid_index_local = grid_index_global_to_local(grid_shape_global, grid_partition_start, grid_partition_count, grid_index_global)
      call grid_index_to_ij(grid_index_local, grid_partition_count, i, j)
      buffer_send_grid_to_land(send_buffer_index) = grid_input(i, j)
    end do

    if (mpi_grp%size == 1) then
      land_output = buffer_send_grid_to_land
    else
#ifdef __MPI__
      CALL MPI_Alltoallv( &
        buffer_send_grid_to_land, &
        partition_transfer_grid_to_land%counts_send, &
        partition_transfer_grid_to_land%displacements_send, &
        MPI_REAL, &
        land_output, &
        partition_transfer_grid_to_land%counts_recv, &
        partition_transfer_grid_to_land%displacements_recv, &
        MPI_REAL, &
        mpi_grp%comm &
      )
#endif
    end if

  end subroutine transform_grid_to_land

  subroutine transform_land_to_grid(mpi_grp, grid_partition_start, grid_partition_count, land_input, grid_output, fill_value)
    !! Perform grid to land partition transfer.
    type(mpi_grp_t), intent(in) :: mpi_grp !! MPI group
    integer, dimension(2), intent(in) :: grid_partition_start !! 2D start indexes of grid partition
    integer, dimension(2), intent(in) :: grid_partition_count !! 2D counts of grid partition
    real, dimension(:), allocatable, intent(in) :: land_input !! Input land vector
    real, dimension(:,:), allocatable, intent(out) :: grid_output !! Output grid
    real, optional :: fill_value

    real, dimension(:), allocatable :: buffer_recv_land_to_grid
    integer, dimension(2) :: grid_shape_local
    real :: fv = DEFAULT_FILL_VALUE
    integer :: i, j, recv_buffer_index, grid_index_local, grid_index_global
    integer :: land_index_global, land_index_start, land_index_count

    if (present(fill_value)) then
      fv = fill_value
    end if

    allocate(buffer_recv_land_to_grid(sum(partition_transfer_land_to_grid%counts_recv)))
    allocate(grid_output(grid_partition_count(1), grid_partition_count(2)), source=fv)

    if (mpi_grp%size == 1) then
      buffer_recv_land_to_grid = land_input
    else
#ifdef __MPI__
      CALL MPI_Alltoallv( &
        land_input, &
        partition_transfer_land_to_grid%counts_send, &
        partition_transfer_land_to_grid%displacements_send, &
        MPI_REAL, &
        buffer_recv_land_to_grid, &
        partition_transfer_land_to_grid%counts_recv, &
        partition_transfer_land_to_grid%displacements_recv, &
        MPI_REAL, &
        mpi_grp%comm &
      )
#endif
    end if

    land_index_start = grid_partition_land_index_start(mpi_grp%rank + 1)
    land_index_count = grid_partition_land_index_count(mpi_grp%rank + 1)
    do recv_buffer_index = 1, land_index_count
      land_index_global = land_index_start + recv_buffer_index - 1
      grid_index_global = land_index_to_grid_index(land_index_global)
      grid_index_local = grid_index_global_to_local(grid_shape_global, grid_partition_start, grid_partition_count, grid_index_global)
      call grid_index_to_ij(grid_index_local, grid_partition_count, i, j)
      grid_output(i, j) = buffer_recv_land_to_grid(recv_buffer_index)
    end do

  end subroutine transform_land_to_grid

  function ij_to_grid_index(i, j, grid_shape) result(grid_index)
    !! Return grid index (memory offset) for a given ij coordinate.
    integer, intent(in) :: i, j
    integer, dimension(2), intent(in) :: grid_shape
    integer :: grid_index
    grid_index = (i - 1) + (j - 1) * grid_shape(1) + 1
  end function ij_to_grid_index

  subroutine grid_index_to_ij(grid_index, grid_shape, i, j)
    !! Compute ij coordinate for a given grid index (memory offset).
    integer, intent(in) :: grid_index
    integer, dimension(2), intent(in) :: grid_shape
    integer, intent(out) :: i, j
    i = mod(grid_index - 1, grid_shape(1)) + 1
    j = (grid_index - 1) / grid_shape(1) + 1
  end subroutine grid_index_to_ij

  function grid_index_local_to_global(grid_shape_global, start, count, grid_index_local) result(grid_index_global)
    !! Return global grid index for a given local grid index.
    integer, dimension(2), intent(in) :: grid_shape_global, start, count
    integer, intent(in) :: grid_index_local
    integer :: i_local, j_local, i_global, j_global, grid_index_global

    call grid_index_to_ij(grid_index_local, count, i_local, j_local)
    i_global = i_local + start(1) - 1
    j_global = j_local + start(2) - 1
    grid_index_global = ij_to_grid_index(i_global, j_global, grid_shape_global)

  end function grid_index_local_to_global

  function grid_index_global_to_local(grid_shape_global, start, count, grid_index_global) result(grid_index_local)
    !! Return local grid index for a given global grid index.
    integer, dimension(2), intent(in) :: grid_shape_global, start, count
    integer, intent(in) :: grid_index_global !! Global grid index
    integer :: i_local, j_local, i_global, j_global, grid_index_local

    call grid_index_to_ij(grid_index_global, grid_shape_global, i_global, j_global)
    i_local = i_global - start(1) + 1
    j_local = j_global - start(2) + 1
    grid_index_local = ij_to_grid_index(i_local, j_local, count)

  end function grid_index_global_to_local

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

  function get_partition_index(n, k, i) result(p)
    !* Compute the partition index p of the i'th element for an array of size n
    ! partitioned into k partitions where p = 0, 1, ..., k - 1.
    !
    ! For k partitions, an array of n elements can be partitioned into r
    ! partitions of length q + 1, and k - r partitions of length q where q and r
    ! are the quotient and remainder of n divided by k (i.e. n = q * k + r).
    ! Note, we assume that the r partitions of length q + 1 precede the k - r
    ! partitions of length q in the array.
    integer, intent(in) :: n, k, i
    integer :: p, q, r

    q = n / k
    r = mod(n, k)

    if (i < (q + 1) * r + 1) then
      p = (i - 1) / (q + 1)
    else
      p = r + (i - 1 - (q + 1) * r) / q
    end if

  end function get_partition_index

  subroutine get_ij_process_counts(k, k_i, k_j)
    !* Compute the number of processes along the i and j axes for a total of k
    ! processes.
    !
    ! Trial division is used to compute the process counts along the i and j axes.
    ! The total number of processes k must be divisible by 2. The returned
    ! number of processes along the i axis is greater than or equal to the number
    ! of processes along the j axis.
    integer, intent(in) :: k
    integer, intent(out) :: k_i, k_j
    integer :: n

    if (k == 1) then
      k_i = 1
      k_j = 1
      return
    end if

    do n = 2, int(sqrt(real(k))) + 1
      if (mod(k, n) /= 0) cycle
      k_i = max(n, k / n)
      k_j = min(n, k / n)
    end do

  end subroutine get_ij_process_counts

  subroutine get_grid_partition_start_count(grid_shape_global, k, p, start, count)
    !* Compute ij start indexes and counts for a given partition index p for k
    ! partitions.
    integer, dimension(2), intent(in) :: grid_shape_global
    integer, intent(in) :: k, p
    integer, dimension(2), intent(out) :: start, count
    integer :: k_i, k_j, p_i, p_j, i_start, i_count, j_start, j_count

    if (rectangular_partitioning) then
      call get_ij_process_counts(k, k_i, k_j)
      p_i = mod(p, k_i)
      p_j = p / k_j
      call get_partition_start_count(grid_shape_global(1), k_i, p_i, i_start, i_count)
      call get_partition_start_count(grid_shape_global(2), k_j, p_j, j_start, j_count)
      start = [i_start, j_start]
      count = [i_count, j_count]
    else
      call get_partition_start_count(grid_shape_global(2), k, p, j_start, j_count)
      start = [1, j_start]
      count = [grid_shape_global(1), j_count]
    end if

  end subroutine get_grid_partition_start_count

  function get_grid_partition_index(grid_shape_global, grid_index, k) result(p)
    !* Compute the partition index p for a given grid index (memory offset) for
    ! k partitions.
    integer, dimension(2), intent(in) :: grid_shape_global
    integer, intent(in) :: grid_index
    integer, intent(in) :: k
    integer :: i, j, k_i, k_j, p_i, p_j, p

    call grid_index_to_ij(grid_index, grid_shape_global, i, j)

    if (rectangular_partitioning) then
      call get_ij_process_counts(k, k_i, k_j)
      p_i = get_partition_index(grid_shape_global(1), k_i, i)
      p_j = get_partition_index(grid_shape_global(2), k_j, j)
      p = p_i + p_j * k_i
    else
      p = get_partition_index(grid_shape_global(2), k, j)
    end if

  end function get_grid_partition_index

  function get_n_land(mpi_grp) result(land_index_count)
    !! Compute the size of the land vector partition on the current process.
    type(mpi_grp_t), intent(in) :: mpi_grp
    integer :: land_index_start, land_index_count
    call get_partition_start_count(n_land, mpi_grp%size, mpi_grp%rank, land_index_start, land_index_count)
  end function get_n_land

end module partition_mod
