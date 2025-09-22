module cable_netcdf_pio_mod
  use cable_netcdf_mod

  use mpi_module, only: mpi_grp_t

  use pio, only: pio_file_desc_t => file_desc_t
  use pio, only: pio_iosystem_desc_t => iosystem_desc_t
  use pio, only: pio_io_desc_t => io_desc_t
  use pio, only: pio_var_desc_t => var_desc_t
  use pio, only: pio_init
  use pio, only: pio_initdecomp
  use pio, only: pio_createfile
  use pio, only: pio_openfile
  use pio, only: pio_closefile
  use pio, only: pio_syncfile
  use pio, only: pio_def_dim
  use pio, only: pio_def_var
  use pio, only: pio_def_var_chunking
  use pio, only: pio_put_att
  use pio, only: pio_put_var
  use pio, only: pio_get_var
  use pio, only: pio_setframe
  use pio, only: pio_write_darray
  use pio, only: pio_read_darray
  use pio, only: pio_strerror
  use pio, only: pio_enddef
  use pio, only: pio_inq_dimid
  use pio, only: pio_inq_varid
  use pio, only: pio_finalize
  use pio, only: PIO_MAX_NAME
  use pio, only: PIO_OFFSET_KIND
  use pio, only: PIO_INT
  use pio, only: PIO_REAL
  use pio, only: PIO_DOUBLE
  use pio, only: PIO_REARR_BOX
  use pio, only: PIO_IOTYPE_NETCDF4C
  use pio, only: PIO_CLOBBER
  use pio, only: PIO_UNLIMITED
  use pio, only: PIO_NOERR
  use pio, only: PIO_GLOBAL
  implicit none

  private
  public :: cable_netcdf_pio_io_t

  integer, parameter :: PIO_CHUNKED = 0, PIO_CONTIGOUS = 1

  type, extends(cable_netcdf_decomp_t) :: cable_netcdf_pio_decomp_t
    type(pio_io_desc_t), private :: pio_io_desc
  end type

  type, extends(cable_netcdf_io_t) :: cable_netcdf_pio_io_t
    private
    type(mpi_grp_t) :: mpi_grp
    type(pio_iosystem_desc_t) :: pio_iosystem_desc
  contains
    procedure :: init => cable_netcdf_pio_io_init
    procedure :: finalise => cable_netcdf_pio_io_finalise
    procedure :: create_file => cable_netcdf_pio_io_create_file
    procedure :: open_file => cable_netcdf_pio_io_open_file
    procedure :: create_decomp => cable_netcdf_pio_io_create_decomp
  end type

  interface cable_netcdf_pio_io_t
    procedure cable_netcdf_pio_io_constructor
  end interface

  type, extends(cable_netcdf_file_t) :: cable_netcdf_pio_file_t
    type(pio_file_desc_t), private :: pio_file_desc
  contains
    procedure :: close => cable_netcdf_pio_file_close
    procedure :: end_def => cable_netcdf_pio_file_end_def
    procedure :: sync => cable_netcdf_pio_file_sync
    procedure :: def_dims => cable_netcdf_pio_file_def_dims
    procedure :: def_var => cable_netcdf_pio_file_def_var
    procedure :: def_var_chunking => cable_netcdf_pio_file_def_var_chunking
    procedure :: put_att_global_string => cable_netcdf_pio_file_put_att_global_string
    procedure :: put_att_var_string => cable_netcdf_pio_file_put_att_var_string
    procedure :: put_var_int32_1d => cable_netcdf_pio_file_put_var_int32_1d
    procedure :: put_var_int32_2d => cable_netcdf_pio_file_put_var_int32_2d
    procedure :: put_var_int32_3d => cable_netcdf_pio_file_put_var_int32_3d
    procedure :: put_var_real32_1d => cable_netcdf_pio_file_put_var_real32_1d
    procedure :: put_var_real32_2d => cable_netcdf_pio_file_put_var_real32_2d
    procedure :: put_var_real32_3d => cable_netcdf_pio_file_put_var_real32_3d
    procedure :: put_var_real64_1d => cable_netcdf_pio_file_put_var_real64_1d
    procedure :: put_var_real64_2d => cable_netcdf_pio_file_put_var_real64_2d
    procedure :: put_var_real64_3d => cable_netcdf_pio_file_put_var_real64_3d
    procedure :: write_darray_int32_1d => cable_netcdf_pio_file_write_darray_int32_1d
    procedure :: write_darray_int32_2d => cable_netcdf_pio_file_write_darray_int32_2d
    procedure :: write_darray_int32_3d => cable_netcdf_pio_file_write_darray_int32_3d
    procedure :: write_darray_real32_1d => cable_netcdf_pio_file_write_darray_real32_1d
    procedure :: write_darray_real32_2d => cable_netcdf_pio_file_write_darray_real32_2d
    procedure :: write_darray_real32_3d => cable_netcdf_pio_file_write_darray_real32_3d
    procedure :: write_darray_real64_1d => cable_netcdf_pio_file_write_darray_real64_1d
    procedure :: write_darray_real64_2d => cable_netcdf_pio_file_write_darray_real64_2d
    procedure :: write_darray_real64_3d => cable_netcdf_pio_file_write_darray_real64_3d
    procedure :: get_var_int32_1d => cable_netcdf_pio_file_get_var_int32_1d
    procedure :: get_var_int32_2d => cable_netcdf_pio_file_get_var_int32_2d
    procedure :: get_var_int32_3d => cable_netcdf_pio_file_get_var_int32_3d
    procedure :: get_var_real32_1d => cable_netcdf_pio_file_get_var_real32_1d
    procedure :: get_var_real32_2d => cable_netcdf_pio_file_get_var_real32_2d
    procedure :: get_var_real32_3d => cable_netcdf_pio_file_get_var_real32_3d
    procedure :: get_var_real64_1d => cable_netcdf_pio_file_get_var_real64_1d
    procedure :: get_var_real64_2d => cable_netcdf_pio_file_get_var_real64_2d
    procedure :: get_var_real64_3d => cable_netcdf_pio_file_get_var_real64_3d
    procedure :: read_darray_int32_1d => cable_netcdf_pio_file_read_darray_int32_1d
    procedure :: read_darray_int32_2d => cable_netcdf_pio_file_read_darray_int32_2d
    procedure :: read_darray_int32_3d => cable_netcdf_pio_file_read_darray_int32_3d
    procedure :: read_darray_real32_1d => cable_netcdf_pio_file_read_darray_real32_1d
    procedure :: read_darray_real32_2d => cable_netcdf_pio_file_read_darray_real32_2d
    procedure :: read_darray_real32_3d => cable_netcdf_pio_file_read_darray_real32_3d
    procedure :: read_darray_real64_1d => cable_netcdf_pio_file_read_darray_real64_1d
    procedure :: read_darray_real64_2d => cable_netcdf_pio_file_read_darray_real64_2d
    procedure :: read_darray_real64_3d => cable_netcdf_pio_file_read_darray_real64_3d
  end type

contains

  function type_pio(basetype)
    integer, intent(in) :: basetype
    integer :: type_pio
    select case(basetype)
    case(CABLE_NETCDF_INT)
      type_pio = PIO_INT
    case(CABLE_NETCDF_FLOAT)
      type_pio = PIO_REAL
    case(CABLE_NETCDF_DOUBLE)
      type_pio = PIO_DOUBLE
    case default
      ! TODO: abort
    end select
  end function type_pio

  function storage_pio(storage)
    integer, intent(in) :: storage
    integer :: storage_pio
    select case(storage)
    case(CABLE_NETCDF_CONTIGUOUS)
      storage_pio = PIO_CONTIGOUS
    case(CABLE_NETCDF_CHUNKED)
      storage_pio = PIO_CHUNKED
    case default
      ! TODO: abort
    end select
  end function storage_pio

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

  function cable_netcdf_pio_io_constructor(mpi_grp) result(this)
    type(cable_netcdf_pio_io_t) :: this
    type(mpi_grp_t), intent(in) :: mpi_grp
    this%mpi_grp = mpi_grp
  end function

  subroutine cable_netcdf_pio_io_init(this)
    class(cable_netcdf_pio_io_t), intent(inout) :: this
    ! TODO: get PIO configuration settings
    call pio_init( &
      comp_rank=this%mpi_grp%rank, &
      comp_comm=this%mpi_grp%comm%mpi_val, &
      num_iotasks=1, &
      num_aggregator=0, & ! This argument is obsolete (see https://github.com/NCAR/ParallelIO/issues/1888)
      stride=1, &
      rearr=PIO_REARR_BOX, &
      iosystem=this%pio_iosystem_desc, &
      base=1 &
    )
  end subroutine

  subroutine cable_netcdf_pio_io_finalise(this)
    class(cable_netcdf_pio_io_t), intent(inout) :: this
    integer :: status
    call pio_finalize(this%pio_iosystem_desc, status)
    call check_pio(status)
  end subroutine


  function cable_netcdf_pio_io_create_file(this, path) result(file)
    class(cable_netcdf_pio_io_t), intent(inout) :: this
    character(len=*), intent(in) :: path
    class(cable_netcdf_file_t), allocatable :: file
    type(pio_file_desc_t) :: pio_file_desc
    call check_pio(pio_createfile(this%pio_iosystem_desc, pio_file_desc, PIO_IOTYPE_NETCDF4C, path, PIO_CLOBBER))
    file = cable_netcdf_pio_file_t(pio_file_desc)
  end function

  function cable_netcdf_pio_io_open_file(this, path) result(file)
    class(cable_netcdf_pio_io_t), intent(inout) :: this
    character(len=*), intent(in) :: path
    class(cable_netcdf_file_t), allocatable :: file
    type(pio_file_desc_t) :: pio_file_desc
    call check_pio(pio_openfile(this%pio_iosystem_desc, pio_file_desc, PIO_IOTYPE_NETCDF4C, path))
    file = cable_netcdf_pio_file_t(pio_file_desc)
  end function

  function cable_netcdf_pio_io_create_decomp(this, compmap, dims, type) result(decomp)
    class(cable_netcdf_pio_io_t), intent(inout) :: this
    integer, intent(in) :: compmap(:), dims(:)
    integer, intent(in) :: type
    class(cable_netcdf_decomp_t), allocatable :: decomp
    type(pio_io_desc_t) :: pio_io_desc
    call pio_initdecomp( &
      this%pio_iosystem_desc, &
      type_pio(type), &
      dims, &
      compmap, &
      pio_io_desc &
    )
    allocate(decomp, source=cable_netcdf_pio_decomp_t(compmap, dims, type, pio_io_desc=pio_io_desc))
  end function

  subroutine cable_netcdf_pio_file_close(this)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    call pio_closefile(this%pio_file_desc)
  end subroutine

  subroutine cable_netcdf_pio_file_end_def(this)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    call check_pio(pio_enddef(this%pio_file_desc))
  end subroutine

  subroutine cable_netcdf_pio_file_sync(this)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    call pio_syncfile(this%pio_file_desc)
  end subroutine

  subroutine cable_netcdf_pio_file_def_dims(this, dim_names, dim_lens)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: dim_names(:)
    integer, intent(in) :: dim_lens(:)
    integer :: i, tmp
    do i = 1, size(dim_names)
      if (dim_lens(i) == CABLE_NETCDF_UNLIMITED) then
        call check_pio(pio_def_dim(this%pio_file_desc, dim_names(i), PIO_UNLIMITED, tmp))
      else
        call check_pio(pio_def_dim(this%pio_file_desc, dim_names(i), dim_lens(i), tmp))
      end if
    end do
  end subroutine

  subroutine cable_netcdf_pio_file_def_var(this, var_name, dim_names, type)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name, dim_names(:)
    integer, intent(in) :: type
    integer, allocatable :: dimids(:)
    integer :: i
    type(pio_var_desc_t) :: tmp
    allocate(dimids(size(dim_names)))
    do i = 1, size(dimids)
      call check_pio(pio_inq_dimid(this%pio_file_desc, dim_names(i), dimids(i)))
    end do
    call check_pio(pio_def_var(this%pio_file_desc, var_name, type_pio(type), dimids, tmp))
  end subroutine

  subroutine cable_netcdf_pio_file_def_var_chunking(this, var_name, storage, chunksizes)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer, intent(in) :: storage, chunksizes(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    call check_pio(pio_def_var_chunking(this%pio_file_desc, var_desc, storage_pio(storage), chunksizes))
  end subroutine

  subroutine cable_netcdf_pio_file_put_att_global_string(this, att_name, att_value)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: att_name, att_value
    call check_pio(pio_put_att(this%pio_file_desc, PIO_GLOBAL, att_name, att_value))
  end subroutine

  subroutine cable_netcdf_pio_file_put_att_var_string(this, var_name, att_name, att_value)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name, att_name, att_value
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    call check_pio(pio_put_att(this%pio_file_desc, var_desc, att_name, att_value))
  end subroutine

  subroutine cable_netcdf_pio_file_put_var_int32_1d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in) :: values(:)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_put_var_int32_2d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in) :: values(:, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_put_var_int32_3d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in) :: values(:, :, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_put_var_real32_1d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in) :: values(:)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_put_var_real32_2d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in) :: values(:, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_put_var_real32_3d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in) :: values(:, :, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_put_var_real64_1d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in) :: values(:)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_put_var_real64_2d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in) :: values(:, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_put_var_real64_3d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in) :: values(:, :, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_put_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_int32(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in) :: values(..)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    integer :: status
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(frame)) then
      call pio_setframe(this%pio_file_desc, var_desc, int(frame, PIO_OFFSET_KIND))
    end if
    select type(decomp)
    type is (cable_netcdf_pio_decomp_t)
      select rank(values)
      rank(1)
        call pio_write_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status, fill_value)
      rank(2)
        call pio_write_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status, fill_value)
      rank(3)
        call pio_write_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status, fill_value)
      rank default
        ! TODO: abort
      end select
      call check_pio(status)
    class default
      ! TODO: abort
    end select
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_int32_1d(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in) :: values(:)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_write_darray_int32(this, var_name, values, decomp, fill_value, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_int32_2d(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in) :: values(:, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_write_darray_int32(this, var_name, values, decomp, fill_value, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_int32_3d(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in) :: values(:, :, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_write_darray_int32(this, var_name, values, decomp, fill_value, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_real32(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in) :: values(..)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    integer :: status
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(frame)) then
      call pio_setframe(this%pio_file_desc, var_desc, int(frame, PIO_OFFSET_KIND))
    end if
    select type(decomp)
    type is (cable_netcdf_pio_decomp_t)
      select rank(values)
      rank(1)
        call pio_write_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status, fill_value)
      rank(2)
        call pio_write_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status, fill_value)
      rank(3)
        call pio_write_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status, fill_value)
      rank default
        ! TODO: abort
      end select
      call check_pio(status)
    class default
      ! TODO: abort
    end select
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_real32_1d(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in) :: values(:)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_write_darray_real32(this, var_name, values, decomp, fill_value, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_real32_2d(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in) :: values(:, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_write_darray_real32(this, var_name, values, decomp, fill_value, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_real32_3d(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in) :: values(:, :, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_write_darray_real32(this, var_name, values, decomp, fill_value, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_real64(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in) :: values(..)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    integer :: status
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(frame)) then
      call pio_setframe(this%pio_file_desc, var_desc, int(frame, PIO_OFFSET_KIND))
    end if
    select type(decomp)
    type is (cable_netcdf_pio_decomp_t)
      select rank(values)
      rank(1)
        call pio_write_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status, fill_value)
      rank(2)
        call pio_write_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status, fill_value)
      rank(3)
        call pio_write_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status, fill_value)
      rank default
        ! TODO: abort
      end select
      call check_pio(status)
    class default
      ! TODO: abort
    end select
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_real64_1d(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in) :: values(:)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_write_darray_real64(this, var_name, values, decomp, fill_value, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_real64_2d(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in) :: values(:, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_write_darray_real64(this, var_name, values, decomp, fill_value, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_write_darray_real64_3d(this, var_name, values, decomp, fill_value, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in) :: values(:, :, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(in), optional :: fill_value
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_write_darray_real64(this, var_name, values, decomp, fill_value, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_get_var_int32_1d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(out) :: values(:)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_get_var_int32_2d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(out) :: values(:, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_get_var_int32_3d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(out) :: values(:, :, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_get_var_real32_1d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(out) :: values(:)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_get_var_real32_2d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(out) :: values(:, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_get_var_real32_3d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(out) :: values(:, :, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_get_var_real64_1d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(out) :: values(:)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_get_var_real64_2d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(out) :: values(:, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_get_var_real64_3d(this, var_name, values, start, count)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(out) :: values(:, :, :)
    integer, intent(in), optional :: start(:), count(:)
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(start) .and. present(count)) then
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, start, count, values))
    else
      call check_pio(pio_get_var(this%pio_file_desc, var_desc, values))
    end if
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_int32(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(out) :: values(..)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    integer :: status
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(frame)) then
      call pio_setframe(this%pio_file_desc, var_desc, int(frame, PIO_OFFSET_KIND))
    end if
    select type(decomp)
    type is (cable_netcdf_pio_decomp_t)
      select rank(values)
      rank(1)
        call pio_read_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status)
      rank(2)
        call pio_read_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status)
      rank(3)
        call pio_read_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status)
      rank default
        ! TODO: abort
      end select
      call check_pio(status)
    class default
      ! TODO: abort
    end select
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_int32_1d(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(out) :: values(:)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_read_darray_int32(this, var_name, values, decomp, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_int32_2d(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(out) :: values(:, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_read_darray_int32(this, var_name, values, decomp, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_int32_3d(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    integer(kind=CABLE_NETCDF_INT32_KIND), intent(out) :: values(:, :, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_read_darray_int32(this, var_name, values, decomp, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_real32(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(out) :: values(..)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    integer :: status
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(frame)) then
      call pio_setframe(this%pio_file_desc, var_desc, int(frame, PIO_OFFSET_KIND))
    end if
    select type(decomp)
    type is (cable_netcdf_pio_decomp_t)
      select rank(values)
      rank(1)
        call pio_read_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status)
      rank(2)
        call pio_read_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status)
      rank(3)
        call pio_read_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status)
      rank default
        ! TODO: abort
      end select
      call check_pio(status)
    class default
      ! TODO: abort
    end select
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_real32_1d(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(out) :: values(:)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_read_darray_real32(this, var_name, values, decomp, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_real32_2d(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(out) :: values(:, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_read_darray_real32(this, var_name, values, decomp, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_real32_3d(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL32_KIND), intent(out) :: values(:, :, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_read_darray_real32(this, var_name, values, decomp, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_real64(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(out) :: values(..)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    integer :: status
    type(pio_var_desc_t) :: var_desc
    call check_pio(pio_inq_varid(this%pio_file_desc, var_name, var_desc))
    if (present(frame)) then
      call pio_setframe(this%pio_file_desc, var_desc, int(frame, PIO_OFFSET_KIND))
    end if
    select type(decomp)
    type is (cable_netcdf_pio_decomp_t)
      select rank(values)
      rank(1)
        call pio_read_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status)
      rank(2)
        call pio_read_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status)
      rank(3)
        call pio_read_darray(this%pio_file_desc, var_desc, decomp%pio_io_desc, values, status)
      rank default
        ! TODO: abort
      end select
      call check_pio(status)
    class default
      ! TODO: abort
    end select
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_real64_1d(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(out) :: values(:)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_read_darray_real64(this, var_name, values, decomp, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_real64_2d(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(out) :: values(:, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_read_darray_real64(this, var_name, values, decomp, frame)
  end subroutine

  subroutine cable_netcdf_pio_file_read_darray_real64_3d(this, var_name, values, decomp, frame)
    class(cable_netcdf_pio_file_t), intent(inout) :: this
    character(len=*), intent(in) :: var_name
    real(kind=CABLE_NETCDF_REAL64_KIND), intent(out) :: values(:, :, :)
    class(cable_netcdf_decomp_t), intent(inout) :: decomp
    integer, intent(in), optional :: frame
    call cable_netcdf_pio_file_read_darray_real64(this, var_name, values, decomp, frame)
  end subroutine

end module cable_netcdf_pio_mod
