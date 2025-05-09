! CSIRO Open Source Software License Agreement (variation of the BSD / MIT License)
! Copyright (c) 2015, Commonwealth Scientific and Industrial Research Organisation
! (CSIRO) ABN 41 687 119 230.

MODULE mpi_module
  !! Module for handling some common MPI operations and MPI groups
#ifdef __MPI__
  USE mpi_f08
#endif
  USE mpi_serial_stub_module
  USE iso_fortran_env, ONLY : error_unit
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: &
    mpi_grp_t, &
    mpi_mod_init, &
    mpi_mod_end, &
    mpi_info_set_hints, &
    mpi_info_hints_t, &
    mpi_check_error

  TYPE(MPI_COMM), PARAMETER :: MPI_COMM_UNDEFINED = MPI_COMM_NULL
  INTEGER, PARAMETER :: HINT_UNDEFINED_INTEGER = 0
  CHARACTER(len=*), PARAMETER :: HINT_UNDEFINED_CHARACTER = ""

  TYPE(MPI_COMM) :: default_comm ! Default communicator to use when creating groups

  TYPE mpi_info_hints_t
    CHARACTER(len=20) :: collective_buffering = HINT_UNDEFINED_CHARACTER
    INTEGER :: cb_block_size = HINT_UNDEFINED_INTEGER
    INTEGER :: cb_buffer_size = HINT_UNDEFINED_INTEGER
    INTEGER :: cb_nodes = HINT_UNDEFINED_INTEGER
    CHARACTER(len=20) :: cb_config_list = HINT_UNDEFINED_CHARACTER
    INTEGER :: striping_factor = HINT_UNDEFINED_INTEGER
    INTEGER :: stripe_size = HINT_UNDEFINED_INTEGER
    INTEGER :: striping_unit = HINT_UNDEFINED_INTEGER
    INTEGER :: stripe_width = HINT_UNDEFINED_INTEGER
  END TYPE mpi_info_hints_t

  TYPE mpi_grp_t
    !* Class to handle MPI groups.
    ! This class stores information about the group and
    ! the current proccess.
    TYPE(MPI_COMM) :: comm = MPI_COMM_UNDEFINED  !! Communicator
    INTEGER :: rank = -1   !! Rank of the current process
    INTEGER :: size = -1   !! Size of the communicator
  CONTAINS
    PROCEDURE :: abort => mpi_grp_abort !! Send abort signal to processes in this group
  END TYPE mpi_grp_t

  INTERFACE mpi_grp_t
    !* Overload the default construct for mpi_grp_t
    PROCEDURE mpi_grp_constructor
  END INTERFACE mpi_grp_t

CONTAINS

  SUBROUTINE mpi_mod_init()
    !* Initialise MPI and set default communicator.
    !
    ! The default communicator is set to MPI_COMM_WORLD if MPI support is
    ! available or to MPI_COMM_UNDEFINED if not.
#ifdef __MPI__
    INTEGER :: ierr

    CALL MPI_Init(ierr)
    CALL mpi_check_error(ierr)
    default_comm = MPI_COMM_WORLD
#else
    default_comm = MPI_COMM_UNDEFINED
#endif

  END SUBROUTINE mpi_mod_init

  SUBROUTINE mpi_mod_end()
    !* Finalise MPI.
#ifdef __MPI__
    INTEGER :: ierr

    IF (default_comm /= MPI_COMM_UNDEFINED) THEN
      CALL MPI_Finalize(ierr)
      CALL mpi_check_error(ierr)
    END IF
#endif

  END SUBROUTINE mpi_mod_end


  FUNCTION mpi_grp_constructor(comm) RESULT(mpi_grp)
    !* Contructor for mpi_grp_t class.
    !
    ! This sets the communicator of the group and gets the size of the group and
    ! rank of current process. If no communicator is provided, it will use
    ! the default defined when calling mpi_mod_init.
    !
    ! Note that when the undefined communicator is used, the group size is 1 and
    ! the rank to 0, such that the code can work in serial mode.
    TYPE(MPI_COMM), INTENT(IN), OPTIONAL :: comm !! MPI communicator
    TYPE(mpi_grp_t) :: mpi_grp

    INTEGER :: ierr

    IF (PRESENT(comm)) THEN
#ifdef __MPI__
      CALL MPI_Comm_dup(comm, mpi_grp%comm, ierr)
      call mpi_check_error(ierr)
#else
      mpi_grp%comm = comm
#endif
    ELSE
#ifdef __MPI__
      CALL MPI_Comm_dup(default_comm, mpi_grp%comm, ierr)
      call mpi_check_error(ierr)
#else
      mpi_grp%comm = default_comm
#endif
    END IF

    IF (mpi_grp%comm /= MPI_COMM_UNDEFINED) THEN
#ifdef __MPI__
      call MPI_Comm_rank(mpi_grp%comm, mpi_grp%rank, ierr)
      call mpi_check_error(ierr)

      call MPI_Comm_size(mpi_grp%comm, mpi_grp%size, ierr)
      call mpi_check_error(ierr)
#else
      WRITE(error_unit,*) "Error initialising mpi group: CABLE was compiled without MPI support."
      STOP
#endif
    ELSE
      mpi_grp%rank = 0
      mpi_grp%size = 1
    END IF

  END FUNCTION mpi_grp_constructor

  SUBROUTINE mpi_grp_abort(this)
    !* Class method to abort execution of an MPI group.
    CLASS(mpi_grp_t), INTENT(IN) :: this

    INTEGER :: ierr

    IF (this%comm /= MPI_COMM_UNDEFINED) THEN
      ! Here we use an arbitrary error code
#ifdef __MPI__
      call MPI_Abort(this%comm, 999, ierr)
#endif
      call mpi_check_error(ierr)
    END IF

  END SUBROUTINE mpi_grp_abort

  SUBROUTINE mpi_check_error(ierr)
    !* Check if an MPI return code signaled an error. If so, print the
    ! corresponding message and abort the execution.
    INTEGER, INTENT(IN) :: ierr !! Error code

#ifdef __MPI__
    CHARACTER(len=MPI_MAX_ERROR_STRING) :: msg
    INTEGER :: length, tmp

    IF (ierr /= MPI_SUCCESS ) THEN
      CALL MPI_Error_String(ierr, msg, length, tmp)
      WRITE(error_unit,*) msg(1:length)
      CALL MPI_Abort(MPI_COMM_WORLD, 1 , tmp)
    END if
#endif

  END SUBROUTINE mpi_check_error

  SUBROUTINE mpi_info_set_hints(info_handle, hints)
    TYPE(MPI_Info), INTENT(INOUT) :: info_handle
    TYPE(mpi_info_hints_t), INTENT(IN) :: hints
    CHARACTER(len=20) :: buffer
    INTEGER :: ierr

#ifdef __MPI__
    IF (hints%collective_buffering /= HINT_UNDEFINED_CHARACTER) THEN
      CALL MPI_Info_set(info_handle, "collective_buffering", hints%collective_buffering, ierr)
    END IF

    IF (hints%cb_block_size /= HINT_UNDEFINED_INTEGER) then
      write(buffer, "(I0)") hints%cb_block_size
      CALL MPI_Info_set(info_handle, "cb_block_size", buffer, ierr)
    END IF

    IF (hints%cb_buffer_size /= HINT_UNDEFINED_INTEGER) THEN
      write(buffer, "(I0)") hints%cb_buffer_size
      CALL MPI_Info_set(info_handle, "cb_buffer_size", buffer, ierr)
    END IF

    IF (hints%cb_nodes /= HINT_UNDEFINED_INTEGER) THEN
      write(buffer, "(I0)") hints%cb_nodes
      CALL MPI_Info_set(info_handle, "cb_nodes", buffer, ierr)
    END IF

    IF (hints%cb_config_list /= HINT_UNDEFINED_CHARACTER) THEN
      CALL MPI_Info_set(info_handle, "cb_config_list", hints%cb_config_list, ierr)
    END IF

    IF (hints%striping_factor /= HINT_UNDEFINED_INTEGER) THEN
      write(buffer, "(I0)") hints%striping_factor
      CALL MPI_Info_set(info_handle, "striping_factor", buffer, ierr)
    END IF

    IF (hints%stripe_size /= HINT_UNDEFINED_INTEGER) THEN
      write(buffer, "(I0)") hints%stripe_size
      CALL MPI_Info_set(info_handle, "stripe_size", buffer, ierr)
    END IF

    IF (hints%striping_unit /= HINT_UNDEFINED_INTEGER) THEN
      write(buffer, "(I0)") hints%striping_unit
      CALL MPI_Info_set(info_handle, "striping_unit", buffer, ierr)
    END IF

    IF (hints%stripe_width /= HINT_UNDEFINED_INTEGER) THEN
      write(buffer, "(I0)") hints%stripe_width
      CALL MPI_Info_set(info_handle, "stripe_width", buffer, ierr)
    END IF
#endif

  END SUBROUTINE mpi_info_set_hints

END MODULE mpi_module
