program pio4by4write

  use mpi_module, only: mpi_grp_t, mpi_mod_init, mpi_mod_end

  use pio, only: pio_file_desc_t => file_desc_t
  use pio, only: pio_iosystem_desc_t => iosystem_desc_t
  use pio, only: pio_io_desc_t => io_desc_t
  use pio, only: pio_var_desc_t => var_desc_t
  use pio, only: pio_init
  use pio, only: pio_setframe
  use pio, only: pio_initdecomp
  use pio, only: pio_createfile
  use pio, only: pio_def_dim
  use pio, only: pio_def_var
  use pio, only: pio_enddef
  use pio, only: pio_closefile
  use pio, only: pio_write_darray
  use pio, only: pio_syncfile
  use pio, only: pio_strerror
  use pio, only: pio_offset_kind
  use pio, only: PIO_INT
  use pio, only: PIO_MAX_NAME
  use pio, only: PIO_REARR_BOX
  use pio, only: PIO_IOTYPE_NETCDF
  use pio, only: PIO_CLOBBER
  use pio, only: PIO_NOERR
  use pio, only: PIO_UNLIMITED

  implicit none

  integer, parameter :: N = 4, M = 4
  integer, parameter :: n_iotasks = 4
  integer, parameter :: n_aggregator = 0 
  integer, parameter :: FILL_VALUE = 0
  integer, parameter :: LAND = 1, NOT_LAND = 0

  logical, dimension(:, :), allocatable :: mask_global

  integer, dimension(:,:), allocatable :: land_vector
  integer, dimension(:), allocatable :: compdof_local, compdof_global

  type(mpi_grp_t) :: mpi_grp
  type(pio_iosystem_desc_t) :: iosystem
  type(pio_file_desc_t) :: file
  type(pio_io_desc_t) :: iodesc
  type(pio_var_desc_t) :: var
  integer(kind=pio_offset_kind) :: recnum = 1

  integer :: grid_index
  integer :: dimids(4)
  integer :: status
  integer :: i, j, p
  integer :: land_start, land_count, offset
  integer :: n_land
  integer :: n_patches = 2 ! Suppose we write out two more values per land point ("patches")

  call mpi_mod_init()
  mpi_grp = mpi_grp_t()

  if (mpi_grp%size /= 4) then
    print *, "Error: number of ranks must be 4."
    call mpi_grp%abort()
  end if

  allocate(mask_global(N, M))

  ! mask_global:
  !        i
  !   +---------+
  !   | x . . x |
  ! j | x x x . | = [1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0]
  !   | . x x . |
  !   | . x x . |
  !   +---------+
  !
  ! values:
  !        i
  !   +-----------+
  !   | 1  .  . 4 |
  ! j | 5  6  7 . | = [1, 0, 0, 4, 5, 6, 7, 0, 0, 10, 11, 0, 0, 14, 15, 0]
  !   | . 10 11 . |
  !   | . 14 15 . |
  !   +-----------+
  !
  ! decompose land vector along 2 by 2 blocks
  !        i
  !   +---------+
  !   | a . . d |
  ! j | b c e . |
  !   | . f h . |
  !   | . g i . |
  !   +---------+
  ! 
  !  rank 0 : compdof_local = land_vector = [ 1,  5, 6]
  !  rank 1 : compdof_local = land_vector = [ 4,  7]
  !  rank 2 : compdof_local = land_vector = [10, 11]
  !  rank 3 : compdof_local = land_vector = [14, 15]

  do j = 1, M
    do i = 1, N
      grid_index = (i - 1) + (j - 1) * N + 1
      select case(grid_index)
      case(1, 4, 5, 6, 7, 10, 11, 14, 15)
        mask_global(i, j) = .true.
      case default
        mask_global(i, j) = .false.
      end select
    end do
  end do

  n_land = count(mask_global)

  call get_partition_start_count(n_land, mpi_grp%size, mpi_grp%rank, land_start, land_count)

  allocate(compdof_global(n_land * n_patches))

  ! decomposition along memory storage order
  offset = 1
  do j = 1, M
    do i = 1, N
      if (.not. mask_global(i, j)) cycle
      do p = 1, n_patches
        compdof_global(offset) = compute_offset([p, i, j], [n_patches, N, M])
        offset = offset + 1
      end do
    end do
  end do

  allocate(compdof_local, source=compdof_global((land_start - 1) * n_patches + 1 : (land_start + land_count - 1) * n_patches))

  if (size(compdof_local) /= land_count * n_patches) then
    print *, "Error: invalid size for compdof_local"
    call mpi_grp%abort()
  end if

  allocate(land_vector(n_patches, land_count))
  do i = 1, land_count
    do j = 1, n_patches
      land_vector(j, i) = compdof_local(j + (i - 1) * n_patches)
    end do
  end do

  call pio_init( &
    comp_rank=mpi_grp%rank, &
    comp_comm=mpi_grp%comm%mpi_val, &
    num_iotasks=mpi_grp%size, &
    num_aggregator=0, & ! This argument is obsolete (see https://github.com/NCAR/ParallelIO/issues/1888)
    stride=1, &
    rearr=PIO_REARR_BOX, &
    iosystem=iosystem &
  )

  call pio_initdecomp( &
    iosystem=iosystem, &
    basepiotype=PIO_INT, &
    dims=[n_patches, N, M], &
    compdof=compdof_local, &
    iodesc=iodesc &
  )
  call check_pio(pio_createfile(iosystem, file, PIO_IOTYPE_NETCDF, "pio4by4.nc", PIO_CLOBBER))
  call check_pio(pio_def_dim(file, 'p', n_patches, dimids(1)))
  call check_pio(pio_def_dim(file, 'i', N, dimids(2)))
  call check_pio(pio_def_dim(file, 'j', M, dimids(3)))
  call check_pio(pio_def_dim(file, 't', PIO_UNLIMITED, dimids(4)))
  call check_pio(pio_def_var(file, "values", PIO_INT, dimids, var))
  call check_pio(pio_enddef(file))
  call pio_setframe(file, var, recnum)
  call pio_write_darray(file, var, iodesc, land_vector, status, fillval=FILL_VALUE)
  call check_pio(status)
  call pio_syncfile(file)
  call pio_closefile(file)

  call mpi_mod_end()

contains

  subroutine check_pio(status)
    integer, intent(in) :: status
    integer :: strerror_status
    character(len=PIO_MAX_NAME) :: err_msg
    if (status /= PIO_NOERR) then
        strerror_status = pio_strerror(status, err_msg)
        print *, trim(err_msg)
        stop 2
    end if
  end subroutine check_pio

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

  function compute_offset(index, shape) result(offset)
    integer, intent(in) :: index(:), shape(:)
    integer :: i, offset, shape_factor
    offset = 1
    shape_factor = 1
    do i = 1, size(index)
      offset = offset + (index(i) - 1) * shape_factor
      shape_factor = shape_factor * shape(i)
    end do
  end function compute_offset

end program pio4by4write
