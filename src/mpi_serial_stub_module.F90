! Stubs for MPI datatypes when compiling without MPI
! Adapted from https://github.com/open-mpi/ompi/blob/v3.0.0/ompi/mpi/fortran/use-mpi-f08/mod/mpi-f08-types.F90

module mpi_serial_stub_module
  implicit none

#ifndef __MPI__

  type, bind(c) :: MPI_Status
    integer :: MPI_SOURCE
    integer :: MPI_TAG
    integer :: MPI_ERROR
  end type MPI_Status

  type, bind(c) :: MPI_Comm
    integer :: MPI_VAL
  end type MPI_Comm

  type, bind(c) :: MPI_Datatype
    integer :: MPI_VAL
  end type MPI_Datatype

  type, bind(c) :: MPI_Errhandler
    integer :: MPI_VAL
  end type MPI_Errhandler

  type, bind(c) :: MPI_File
    integer :: MPI_VAL
  end type MPI_File

  type, bind(c) :: MPI_Group
    integer :: MPI_VAL
  end type MPI_Group

  type, bind(c) :: MPI_Info
    integer :: MPI_VAL
  end type MPI_Info

  type, bind(c) :: MPI_Message
    integer :: MPI_VAL
  end type MPI_Message

  type, bind(c) :: MPI_Op
    integer :: MPI_VAL
  end type MPI_Op

  type, bind(c) :: MPI_Request
    integer :: MPI_VAL
  end type MPI_Request

  type, bind(c) :: MPI_Session
    integer :: MPI_VAL
  end type MPI_Session

  type, bind(c) :: MPI_Win
    integer :: MPI_VAL
  end type MPI_Win

  type(MPI_Comm), parameter       :: MPI_COMM_NULL       = MPI_Comm(0)
  type(MPI_Datatype), parameter   :: MPI_DATATYPE_NULL   = MPI_Datatype(0)
  type(MPI_Errhandler), parameter :: MPI_ERRHANDLER_NULL = MPI_Errhandler(0)
  type(MPI_File), parameter       :: MPI_FILE_NULL       = MPI_File(0)
  type(MPI_Group),  parameter     :: MPI_GROUP_NULL      = MPI_Group(0)
  type(MPI_Info), parameter       :: MPI_INFO_NULL       = MPI_Info(0)
  type(MPI_Message), parameter    :: MPI_MESSAGE_NULL    = MPI_Message(0)
  type(MPI_Op), parameter         :: MPI_OP_NULL         = MPI_Op(0)
  type(MPI_Request), parameter    :: MPI_REQUEST_NULL    = MPI_Request(0)
  type(MPI_Win), parameter        :: MPI_WIN_NULL        = MPI_Win(0)

  type(MPI_Datatype), parameter :: MPI_INTEGER           = MPI_Datatype(0)
  type(MPI_Datatype), parameter :: MPI_INTEGER8          = MPI_Datatype(0)
  type(MPI_Datatype), parameter :: MPI_DOUBLE_PRECISION  = MPI_Datatype(0)
  type(MPI_Datatype), parameter :: MPI_DOUBLE_COMPLEX    = MPI_Datatype(0)
  type(MPI_Datatype), parameter :: MPI_2DOUBLE_PRECISION = MPI_Datatype(0)
  type(MPI_Datatype), parameter :: MPI_CHARACTER         = MPI_Datatype(0)
  type(MPI_Datatype), parameter :: MPI_LOGICAL           = MPI_Datatype(0)

  type(MPI_Op), parameter :: MPI_SUM      = MPI_Op(0)
  type(MPI_Op), parameter :: MPI_MINLOC   = MPI_Op(0)
  type(MPI_Op), parameter :: MPI_MAXLOC   = MPI_Op(0)
  type(MPI_Op), parameter :: MPI_LOR      = MPI_Op(0)
  type(MPI_Op), parameter :: MPI_LAND     = MPI_Op(0)
  type(MPI_Op), parameter :: MPI_MAX      = MPI_Op(0)
  type(MPI_Op), parameter :: MPI_MIN      = MPI_Op(0)
  type(MPI_Op), parameter :: MPI_IN_PLACE = MPI_Op(0)

  interface operator (.EQ.)
    module procedure oct_mpi_comm_op_eq
    module procedure oct_mpi_datatype_op_eq
    module procedure oct_mpi_errhandler_op_eq
    module procedure oct_mpi_file_op_eq
    module procedure oct_mpi_group_op_eq
    module procedure oct_mpi_info_op_eq
    module procedure oct_mpi_message_op_eq
    module procedure oct_mpi_op_op_eq
    module procedure oct_mpi_request_op_eq
    module procedure oct_mpi_win_op_eq
  end interface operator (.EQ.)

  interface operator (.NE.)
    module procedure oct_mpi_comm_op_ne
    module procedure oct_mpi_datatype_op_ne
    module procedure oct_mpi_errhandler_op_ne
    module procedure oct_mpi_file_op_ne
    module procedure oct_mpi_group_op_ne
    module procedure oct_mpi_info_op_ne
    module procedure oct_mpi_message_op_ne
    module procedure oct_mpi_op_op_ne
    module procedure oct_mpi_request_op_ne
    module procedure oct_mpi_win_op_ne
  end interface operator (.NE.)

contains

  ! .EQ. operator
  !-----------------
  logical function oct_mpi_comm_op_eq(a, b)
    type(MPI_Comm), intent(in) :: a, b
    oct_mpi_comm_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function oct_mpi_comm_op_eq

  logical function oct_mpi_datatype_op_eq(a, b)
    type(MPI_Datatype), intent(in) :: a, b
    oct_mpi_datatype_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function oct_mpi_datatype_op_eq

  logical function oct_mpi_errhandler_op_eq(a, b)
    type(MPI_Errhandler), intent(in) :: a, b
    oct_mpi_errhandler_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function oct_mpi_errhandler_op_eq

  logical function oct_mpi_file_op_eq(a, b)
    type(MPI_File), intent(in) :: a, b
    oct_mpi_file_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function oct_mpi_file_op_eq

  logical function oct_mpi_group_op_eq(a, b)
    type(MPI_Group), intent(in) :: a, b
    oct_mpi_group_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function oct_mpi_group_op_eq

  logical function oct_mpi_info_op_eq(a, b)
    type(MPI_Info), intent(in) :: a, b
    oct_mpi_info_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function oct_mpi_info_op_eq

  logical function oct_mpi_message_op_eq(a, b)
    type(MPI_Message), intent(in) :: a, b
    oct_mpi_message_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function oct_mpi_message_op_eq

  logical function oct_mpi_op_op_eq(a, b)
    type(MPI_Op), intent(in) :: a, b
    oct_mpi_op_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function oct_mpi_op_op_eq

  logical function oct_mpi_request_op_eq(a, b)
    type(MPI_Request), intent(in) :: a, b
    oct_mpi_request_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function oct_mpi_request_op_eq

  logical function oct_mpi_win_op_eq(a, b)
    type(MPI_Win), intent(in) :: a, b
    oct_mpi_win_op_eq = (a%MPI_VAL .EQ. b%MPI_VAL)
  end function oct_mpi_win_op_eq

  ! .NE. operator
  !-----------------
  logical function oct_mpi_comm_op_ne(a, b)
    type(MPI_Comm), intent(in) :: a, b
    oct_mpi_comm_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function oct_mpi_comm_op_ne

  logical function oct_mpi_datatype_op_ne(a, b)
    type(MPI_Datatype), intent(in) :: a, b
    oct_mpi_datatype_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function oct_mpi_datatype_op_ne

  logical function oct_mpi_errhandler_op_ne(a, b)
    type(MPI_Errhandler), intent(in) :: a, b
    oct_mpi_errhandler_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function oct_mpi_errhandler_op_ne

  logical function oct_mpi_file_op_ne(a, b)
    type(MPI_File), intent(in) :: a, b
    oct_mpi_file_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function oct_mpi_file_op_ne

  logical function oct_mpi_group_op_ne(a, b)
    type(MPI_Group), intent(in) :: a, b
    oct_mpi_group_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function oct_mpi_group_op_ne

  logical function oct_mpi_info_op_ne(a, b)
    type(MPI_Info), intent(in) :: a, b
    oct_mpi_info_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function oct_mpi_info_op_ne

  logical function oct_mpi_message_op_ne(a, b)
    type(MPI_Message), intent(in) :: a, b
    oct_mpi_message_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function oct_mpi_message_op_ne

  logical function oct_mpi_op_op_ne(a, b)
    type(MPI_Op), intent(in) :: a, b
    oct_mpi_op_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function oct_mpi_op_op_ne

  logical function oct_mpi_request_op_ne(a, b)
    type(MPI_Request), intent(in) :: a, b
    oct_mpi_request_op_ne  = (a%MPI_VAL .NE. b%MPI_VAL)
  end function oct_mpi_request_op_ne

  logical function oct_mpi_win_op_ne(a, b)
    type(MPI_Win), intent(in) :: a, b
    oct_mpi_win_op_ne = (a%MPI_VAL .NE. b%MPI_VAL)
  end function oct_mpi_win_op_ne

#endif

end module
