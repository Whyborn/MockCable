module file_utils
  implicit none

  private

  public :: &
    file_exists, &
    file_delete

contains

  function file_exists(file_name)
    character(len=*), intent(in) :: file_name
    logical :: file_exists
    inquire(file=trim(file_name), exist=file_exists)
  end function

  subroutine file_delete(file_name)
    character(len=*), intent(in) :: file_name
    integer :: file_unit
    if (.not. file_exists(file_name)) return
    open(file=file_name, newunit=file_unit)
    close(file_unit, status="delete")
  end subroutine

end module file_utils