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
    mpi_check_error, &
    mpi_grp_subset, &
    mpi_info_set_hints, &
    mpi_info_hints

  TYPE(MPI_COMM), PARAMETER :: MPI_COMM_UNDEFINED = MPI_COMM_NULL

  TYPE(MPI_COMM) :: default_comm ! Default communicator to use when creating groups

  TYPE mpi_info_hints_t
    CHARACTER(len=20) :: collective_buffering = ""
    CHARACTER(len=20) :: cb_block_size = ""
    CHARACTER(len=20) :: cb_buffer_size = ""
    CHARACTER(len=20) :: cb_nodes = ""
    CHARACTER(len=20) :: cb_config_list = ""
    CHARACTER(len=20) :: striping_factor = ""
    CHARACTER(len=20) :: stripe_size = ""
    CHARACTER(len=20) :: striping_unit = ""
    CHARACTER(len=20) :: stripe_width = ""
  END TYPE mpi_info_hints_t

  TYPE(mpi_info_hints_t) :: mpi_info_hints

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
      IF (comm /= MPI_COMM_UNDEFINED) THEN
        CALL MPI_Comm_dup(comm, mpi_grp%comm, ierr)
        CALL mpi_check_error(ierr)
      ELSE
        mpi_grp%comm = comm
      END IF
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

  FUNCTION mpi_grp_subset(mpi_grp_parent, ranks) RESULT(mpi_grp)
    TYPE(mpi_grp_t), INTENT(IN) :: mpi_grp_parent
    INTEGER, INTENT(IN) :: ranks(:)
    TYPE(MPI_Group) :: parent_group, subset_group
    TYPE(MPI_COMM) :: comm
    INTEGER :: ierr
    TYPE(mpi_grp_t) :: mpi_grp

    CALL MPI_Comm_group(mpi_grp_parent%comm, parent_group, ierr)
    CALL mpi_check_error(ierr)

    CALL MPI_Group_incl(parent_group, size(ranks), ranks, subset_group, ierr)
    CALL mpi_check_error(ierr)

    CALL MPI_Comm_create(mpi_grp_parent%comm, subset_group, comm, ierr)
    CALL mpi_check_error(ierr)

    mpi_grp = mpi_grp_t(comm)

  END FUNCTION mpi_grp_subset

  SUBROUTINE mpi_info_set_hints(info_handle)
    TYPE(MPI_Info), INTENT(INOUT) :: info_handle
    INTEGER :: ierr

#ifdef __MPI__
    IF (len_trim(mpi_info_hints%collective_buffering) > 0) &
      CALL MPI_Info_set(info_handle, "collective_buffering", mpi_info_hints%collective_buffering, ierr)
    IF (len_trim(mpi_info_hints%cb_block_size) > 0) &
      CALL MPI_Info_set(info_handle, "cb_block_size", mpi_info_hints%cb_block_size, ierr)
    IF (len_trim(mpi_info_hints%cb_buffer_size) > 0) &
      CALL MPI_Info_set(info_handle, "cb_buffer_size", mpi_info_hints%cb_buffer_size, ierr)
    IF (len_trim(mpi_info_hints%cb_nodes) > 0) &
      CALL MPI_Info_set(info_handle, "cb_nodes", mpi_info_hints%cb_nodes, ierr)
    IF (len_trim(mpi_info_hints%cb_config_list) > 0) &
      CALL MPI_Info_set(info_handle, "cb_config_list", mpi_info_hints%cb_config_list, ierr)
    IF (len_trim(mpi_info_hints%striping_factor) > 0) &
      CALL MPI_Info_set(info_handle, "striping_factor", mpi_info_hints%striping_factor, ierr)
    IF (len_trim(mpi_info_hints%stripe_size) > 0) &
      CALL MPI_Info_set(info_handle, "stripe_size", mpi_info_hints%stripe_size, ierr)
    IF (len_trim(mpi_info_hints%striping_unit) > 0) &
      CALL MPI_Info_set(info_handle, "striping_unit", mpi_info_hints%striping_unit, ierr)
    IF (len_trim(mpi_info_hints%stripe_width) > 0) &
      CALL MPI_Info_set(info_handle, "stripe_width", mpi_info_hints%stripe_width, ierr)
#endif

  END SUBROUTINE mpi_info_set_hints

END MODULE mpi_module
