function initialise_datasetreader(text_file_list, var_name,&
    timestep_size, decomp) result(new_reader)

  character(len=*), intent(in) :: text_file_list, var_name
  type(timedelta), intent(in) :: timestep
  type(land_decomp_t) :: decomp
  type(dataset_reader) :: new_reader

  ! Array to hold all the file names contained in the specified file
  character(len=250), dimension(:), allocatable :: list_of_files


  list_of_files = read_lines(text_file_list)

  new_reader%dataset_files = sort_by_start_date(list_of_files)
  new_reader%var_name = var_name
  new_reader%timestep_size = timestep_size
  new_reader%decomp = decomp

  call new_reader%identify_start_year()
  call new_reader%assign_file_indices()

end function initialise_datasetreader

function read_lines(text_file) result(lines)
  character(len=*), intent(in) :: text_file

  character(len=250), dimension(:), allocatable :: lines

  integer :: max_lines = 10000
  character(len=250), dimension(max_lines) :: temp_lines
  integer :: ctr, ios, file_unit
  character(len=100) :: io_message

  open(newunit=file_unit, file=text_file, iostat=ios)

  read_file: do ctr = 1, max_lines
    read(file_unit, '(A)', iostat=ios, iomsg=io_message) temp_lines(ctr)

    if (ios < 0) then
      exit read_file
    elseif (ios /= 0) then
      write(error_unit, '(A)') 'Error reading lines: '//io_msg
    end if
  end do read_file

  ctr = ctr - 1

  if ctr == 0 then
    write(error_unit, '(A)') 'File '//text_file//' was empty.'
    stop 1
  end if

  allocate(lines(ctr))
  lines(:) = temp_lines(1:ctr)

end function read_lines

function sort_by_start_date(file_list) result(sorted_files)
  character(len=*), dimension(:), intent(in) :: file_list
  
  character(len=250), dimension(:), allocatable :: sorted_files

  integer, dimension(:), allocatable :: time_values, indexer

  integer :: ctr

  allocate(sorted_files(size(file_list)), time_values(size(file_list)))

  do ctr = 1, size(file_list)
    file = cable_netcdf_open_file(file_list(ctr))
    call file%get_var("time", time_values(ctr), count=[1])
    call file%close()
  end do

  call sort(time_values, indexer)
  do ctr = 1, size(indexer)
    sorted_files(ctr) = file_list(indexer(ctr))
  end do

end function sort_by_start_date

subroutine identify_start_year(this)
  !*## Purpose
  !
  ! Identify which year the dataset begins.

  type(dataset_reader), intent(inout) :: this

  integer :: ncstat, first_time, ref_year, ref_month, ref_day
  character(len=33) :: time_units
  type(datetime) :: ref_time, start_time
  class(cable_netcdf_file_t), allocatable :: file

  ! Set the current file to the first one in the list
  call this%set_current_file(1)

  ! The time attribute must be "seconds since YYYY/MM/DD"
  file%get_att("time", "units", time_units)
  if (time_units(1:13) /= 'seconds_since') then
    write(error_unit,'(A)') 'Invalid units for time attribute.'
    stop 1
  end if

  ! Create datetime object for the start time
  read(time_units(15:18), '(I4)') ref_year
  read(time_units(20:21), '(I2)') ref_month
  read(time_units(23:24), '(I2)') ref_day

  ref_time = datetime(year=ref_year, month=ref_month, day=ref_day)

  ! How far from that ref time is the start time?
  file%get_var("time", first_time, count=[1])

  ! Now compute the new datetime by adding the timedelta
  this%start_time = ref_time + timedelta(seconds=first_time)

end subroutine identify_start_year
  
subroutine assign_file_indices(this)
  !*# Purpose
  !
  ! Assign the range of time indices to each file to accelerate searching.
  type(dataset_reader), intent(inout) :: this

  integer, dimension(:), allocatable :: index_range
  integer :: ctr, dim_l

  ! We want to set the indices to say "this is where the nth file starts"
  allocate(index_range(size(this%dataset_files)))
  index_range(1) = 1

  do ctr = 1, (size(dataset_files) - 1)
    this%set_current_file(ctr)
    this%current_file%inq_dim_len("time", dim_l)

    index_range(ctr + 1) = index_range(ctr) + dim_l
  end do

end subroutine assign_file_indices

subroutine set_current_file(this, file_number)
  !*## Purpose
  !
  ! Set the current file handle for the reader to the specified file number.
  type(dataset_reader), intent(inout) :: this
  integer, intent(in) :: file_number

  if (this%current_file_index /= file_number) then
    this%file = cable_netcdf_open_file(this%dataset_files(file_number))
    this%current_file = file_number
    ! This will happen almost every time, except the first call
    if (allocated(this%file)) then
      call this%file%close()
    end if
  end if

end subroutine set_current_file

subroutine get_data(this, year, timestep_count, data)
  !*## Purpose
  !
  ! Retrieve the array of data associated with a given timestep in the
  ! specified year.

  type(dataset_reader), intent(inout) :: this
  integer, intent(in) :: year, timestep_count
  real, dimension(:), intent(out) :: data

  curr_time = datetime(year=year) * timestep_count * this%timestep_size
  time_diff = curr_time - this%start_time

  total_minutes = time_diff%getdays() * d2m + time_diff%gethours() * 60 +&
    time_diff%getminutes()
  timestep_as_minutes = this%timestep_size%getdays() * d2m +&
    this%timestep_size%get_hours() * 60 +&
    this%timestep_size%get_minutes()
  ind = total_minutes / timestep_as_minutes

  this%set_file_by_ind(ind)

  this%file%read_darray(....)

end subroutine get_data


