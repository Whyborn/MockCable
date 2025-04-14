program simple4by4

  use mpi_module, only: mpi_grp_t, mpi_mod_init, mpi_mod_end, mpi_grp_subset

  use partition_mod, only: partition_mod_init, partition_mod_end
  use partition_mod, only: transform_grid_to_land, transform_land_to_grid
  use partition_mod, only: get_grid_partition_start_count

  implicit none

  type domain_t
    integer, dimension(2) :: start, count
  end type domain_t

  integer, parameter :: N = 4, M = 4
  integer, parameter :: MAX_N_IO_RANKS = 2
  integer, parameter :: IO_RANKS(MAX_N_IO_RANKS) = [0, 1]
  real, parameter :: FILL_VALUE = 0.0
  integer, parameter :: LAND = 1, NOT_LAND = 0

  logical, dimension(:, :), allocatable :: mask_global

  real, dimension(:, :), allocatable :: input_data_global, input_data_local, output_data_local
  real, dimension(:), allocatable :: land_vector

  type(domain_t) :: domain_global, domain_local
  type(mpi_grp_t) :: mpi_grp, mpi_grp_io
  integer :: grid_index
  integer :: nland
  integer :: n_io_ranks
  logical :: is_io_rank
  integer :: i, j, rank

  call mpi_mod_init()
  mpi_grp = mpi_grp_t()

  n_io_ranks = min(mpi_grp%size, MAX_N_IO_RANKS)
  if (mpi_grp%size > MAX_N_IO_RANKS) then
    mpi_grp_io = mpi_grp_subset(mpi_grp, IO_RANKS)
  else
    mpi_grp_io = mpi_grp
  end if

  domain_global%start = [1, 1]
  domain_global%count = [N, M]

  allocate(mask_global(N, M))
  allocate(input_data_global(N, M))

  ! mask_global:
  !        i
  !   +---------+
  !   | x . . x |
  ! j | x x x . | = [1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0]
  !   | . x x . |
  !   | . x x . |
  !   +---------+
  !
  ! input_data_global = [1, 0, 0, 4, 5, 6, 7, 0, 0, 10, 11, 0, 0, 14, 15, 0]

  do j = 1, M
    do i = 1, N
      grid_index = (i - 1) + (j - 1) * domain_global%count(1) + 1
      select case(grid_index)
      case(1, 4, 5, 6, 7, 10, 11, 14, 15)
        mask_global(i, j) = .true.
        input_data_global(i, j) = real(grid_index)
      case default
        mask_global(i, j) = .false.
        input_data_global(i, j) = FILL_VALUE
      end select
    end do
  end do

  call partition_mod_init(mask_global, mpi_grp, n_io_ranks)
  is_io_rank = mpi_grp%rank < n_io_ranks
  if (is_io_rank) then
    call get_grid_partition_start_count(domain_global%count, n_io_ranks, mpi_grp%rank, domain_local%start, domain_local%count)
  else
    domain_local%start = [1, 1]
    domain_local%count = [0, 0]
  end if

  allocate( &
    input_data_local, &
    source=input_data_global( &
      domain_local%start(1):domain_local%start(1) + domain_local%count(1) - 1, &
      domain_local%start(2):domain_local%start(2) + domain_local%count(2) - 1 &
    ) &
  )

  call transform_grid_to_land(mpi_grp, domain_local%start, domain_local%count, input_data_local, land_vector)

  print *, 'rank', mpi_grp%rank, 'land_vector', land_vector

  call transform_land_to_grid(mpi_grp, domain_local%start, domain_local%count, land_vector, output_data_local)

  print *, 'rank', mpi_grp%rank, 'output_data_local', output_data_local

  call partition_mod_end()
  call mpi_mod_end()

end program simple4by4
