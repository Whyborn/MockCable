! Copyright (C) 2025 M. Oliveira
!!
!! This Source Code Form is subject to the terms of the Mozilla Public
!! License, v. 2.0. If a copy of the MPL was not distributed with this
!! file, You can obtain one at https://mozilla.org/MPL/2.0/.
!!

!> @brief This module implements classes for handling exceptions in Fortran. It
!! is heavily inspired by the code of B. Aradi's errorfx library
!! (https://github.com/aradi/errorfx), but with some relevant differences.
!!
!! Bellow is a brief explanation of how this error handler is supposed to be used.
!!
!! First, two classes are defined, fatal_error_t and warning_t, that store the
!! relevant information about the error. These classes store a message
!! describing the error/warning and a flags that tells if the error/warning is
!! active or not. Furthermore, each class has a finalizer that will trigger some
!! action (e.g., printing a message) if the error/warning is active.
!!
!! Routines where exceptions might occur take a dummy argument declared with the
!! allocatable and intent(out) attributes. The variable passed to that routine
!! should be passed to it without being allocated beforehand, otherwise the
!! intent(out) attribute will trigger the deallocation and, more importantly,
!! call the finalizer. If an exception occurs inside the routine, one should
!! allocate the error variable, set the relevant information about the error,
!! and active it. This would look something like this:
!!
!! ```
!!   subroutine my_routine(..., error)
!!     ...
!!     class(error_t), allocatable, intent(out) :: error
!!     ...
!!    ! Error occurs here
!!    allocated(error)
!!    error%msg = ["An error has occured"]
!!    error%active = .true.
!! ```
!!
!! Although one can explicitly allocate the error variable and set the relevant
!! information, this module provides two routines that can be used to do this:
!! new_fatal_error and new_warning. The above snippet would look like this when
!! using these functions:
!!
!! ```
!!   subroutine my_routine(..., error)
!!     ...
!!     class(error_t), allocatable, intent(out) :: error
!!     ...
!!    ! Error occurs here
!!    call new_fatal_error(error, ["An error has occured"])
!! ```
!!
!! Upon return, one can then check if an error has occured by checking the
!! allocation status of the variable. If it is returned allocated, i.e., an
!! exception occured, one can then handle it appropriatly or propagate the error
!! upward. If the error was handled and the intention is for the code to
!! continue to run normally, the error should be deactivated using the
!! appropriately named deactive method of the classes. Otherwise the code might
!! stop when the error variable goes out-of-scope and the finalizer is
!! called. This makes the error handling robust by not allowing the code to
!! continue running if an error occured and was simply ignored. Here is an
!! example of another subroutine that calls the routine defined above and some
!! options one can use to handle the error:
!!
!! ```
!!  subroutine another_routine(..., error)
!!     ...
!!     class(error_t), allocatable, intent(out) :: error
!!
!!    ...
!!
!!    ! First possible error
!!    call my_routine(..., error)
!!    if (allocated(error)) then
!!      ! Do something here about the error
!!      ...
!!      ! Deactivate error
!!      call error%deactivate()
!!    end if
!!
!!    ...
!!
!!    ! Second possible error
!!    call my_routine(..., error)
!!    ! Propagate error upward
!!    if (allocated(error)) return
!!
!!    ...
!! ```
!!
!! Note that in the above example, if an error occured during the first call to
!! my_routine and the error was not deactivate, then the second call to
!! my_routine would immediatly trigger the error finalizer.
!!
!! You might have noticed that in the above examples the dummy arguments are
!! declared as class(error_t). Both fatal_error_t and warning_t are both
!! extensions of this base class, error_t. We recommended that any routine that
!! can throw an error declare the corresponding dummy argument to be
!! class(error_t), so that it is possible for the same routine to return either
!! a fatal error or a warning.
!!
!! If a routine that can throw an error is part of a library, it is possible
!! that the host code might want unhandled errors to behave in some specific way
!! different from the default behaviour defined in this module (e.g., maybe
!! instead of sending the error message to stdout, the code might want to write
!! to a specific log file). The obvious solution for this is to create new
!! derived types that extends the ones defined here and define new
!! finalizers. The issue with this is that, for obvious reasons, the library
!! that is being called has no knowledge of those new derived
!! types. Nevertheless, Fortran provides an option to allocate an instance of a
!! class using a mold. In that case the concrete type that will be allocated
!! will be the same as the one of the mold, but no actual data will be copied
!! between instances. We can use this feature to allow the library to allocate a
!! new error of the desired type by providing the corresponding mold. There are
!! two allocatable variables defined in this module that can be used for this
!! exact purpose: fatal_error_mold and warning_mold. The new_fatal_error and
!! new_warning subroutines then check if the molds are allocated and, if that's
!! the case, uses them when allocating new errors. For this to work, the only
!! thing the host code needs to do is to allocate the mold to the desired type
!! before calling any routine that might throw an error. Note that the molds
!! should not be activated, otherwise an error will be thrown when they are
!! deallocated, either explicity or when going out-of-scope.
!!

module kraken_error_handler
  implicit none
  private

  public ::             &
    error_t,            &
    fatal_error_t,      &
    fatal_error_mold,   &
    new_fatal_error,    &
    warning_t,          &
    warning_mold,       &
    new_warning

  !> @brief Base class providing common functionalities for all errors.
  !!
  !! It is possible to extend this class in case new types of errors are needed,
  !! but it is recommended instead to extend the fatal_error_t and warning_t
  !! classes if possible. This will ensure the new classes can be used as molds
  !! by any library that uses this error handler.
  !!
  !! \note It is expected that any finalizer of classes extending this one
  !! honour the state of the error, that is, if the error is active or not.
  type :: error_t
    logical :: active = .false. !< Is the error active?
    character(len=:), allocatable :: msg(:) !< Text describing the error.
  contains
    procedure :: deactivate => error_deactivate  !< @copydoc error_deactivate::error_deactivate
  end type error_t

  !> @brief Fatal error class.
  !!
  !! Specialization of the error class that will print a message and stop the
  !! code if the error is left unhandled.
  type, extends(error_t) ::  fatal_error_t
  contains
    final :: fatal_error_finalizer
  end type fatal_error_t

  !> @brief Warning class.
  !!
  !! Specialization of the error class that will print a message if the error is
  !! left unhandled. It will not stop the code.
  type, extends(error_t) :: warning_t
  contains
    final :: warning_finalizer
  end type warning_t

  !> Instance of the fatal error class that, if allocated, will be used as a
  !> mold when allocating new errors.
  class(fatal_error_t),   allocatable :: fatal_error_mold

  !> Instance of the warning class that, if allocated, will be used as a mold
  !> when allocating new warnings.
  class(warning_t), allocatable :: warning_mold

contains

  ! ---------------------------------------------------------
  !> @brief Deactivate an error.
  subroutine error_deactivate(this)
    class(error_t), intent(inout) :: this

    this%active = .false.

  end subroutine error_deactivate

  ! ---------------------------------------------------------
  !> @brief Handles error when instance is deallocated.
  subroutine fatal_error_finalizer(this)
    type(fatal_error_t), intent(inout) :: this

    integer :: i

    if (this%active) then
      if (allocated(this%msg)) then
        write(*,*) "A fatal error occured:"
        do i = 1, size(this%msg)
          write(*,*) this%msg(i)
        end do
      end if
      deallocate(this%msg)
      stop 1
    end if

  end subroutine fatal_error_finalizer

  ! ---------------------------------------------------------
  !> @brief Factory returning a new error.
  !!
  !! This routine takes care of allocating the new error to be of the default
  !! type (fatal_error_t) or uses the fatal error mold if that is available.
  !! It also activates the error.
  subroutine new_fatal_error(error, msg)
    character(len=*),            intent(in)  :: msg(:)
    class(error_t), allocatable, intent(out) :: error

    if (allocated(fatal_error_mold)) then
      allocate(error, mold=fatal_error_mold)
    else
      allocate(fatal_error_t :: error)
    end if
    error%msg = msg
    error%active = .true.

  end subroutine new_fatal_error

  ! ---------------------------------------------------------
  !> @brief Handles warning when instance is deallocated.
  subroutine warning_finalizer(this)
    type(warning_t), intent(inout) :: this

    integer :: i

    if (this%active .and. allocated(this%msg)) then
      write(*,*) "Warning:"
      do i = 1, size(this%msg)
        write(*,*) this%msg(i)
      end do
      deallocate(this%msg)
    end if

  end subroutine warning_finalizer

  ! ---------------------------------------------------------
  !> @brief Factory returning a new warning.
  !!
  !! This routine takes care of allocating the new warning to be of the default
  !! type (warning_t) or uses the warning mold if that is available. It also
  !! activates the warning.
  subroutine new_warning(error, msg)
    character(len=*),            intent(in) :: msg(:)
    class(error_t), allocatable, intent(out) :: error

    if (allocated(warning_mold)) then
      allocate(error, mold=warning_mold)
    else
      allocate(warning_t :: error)
    end if
    error%msg = msg
    error%active = .true.

  end subroutine new_warning

end module kraken_error_handler
