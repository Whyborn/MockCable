! Author: Lachlan Whyborn
! Last Modified: Fri 08 Nov 2024 06:27:19 PM AEDT

MODULE common_module

USE iso_fortran_env, ONLY: ERROR_UNIT, OUTPUT_UNIT
USE netcdf, ONLY: NF90_NOERR, NF90_STRERROR, NF90_EBADDIM, NF90_ENOTVAR,&
                  NF90_INQ_DIMID, NF90_INQ_VARID

IMPLICIT NONE

INTERFACE sort
  MODULE PROCEDURE sort_int
  MODULE PROCEDURE sort_real
END INTERFACE sort

! Dimension name lists for searching NetCDF dimensions IDs
CHARACTER(LEN=*), DIMENSION(*), PARAMETER :: LatNames = &
  [CHARACTER(LEN=8) :: "latitude", "lat", "lats", "y", "Latitude"]
CHARACTER(LEN=*), DIMENSION(*), PARAMETER :: LonNames = &
  [CHARACTER(LEN=9) :: "longitude", "lon", "lons", "x", "Longitude"]
CHARACTER(LEN=*), DIMENSION(*), PARAMETER :: TimeNames = &
  [CHARACTER(LEN=4) :: "time", "t", "Time"]

CONTAINS

SUBROUTINE sort_int(IntArray, Indexer)
  !*## Purpose
  !
  ! Sort 1D array of integers in place in ascending order.
  !
  !## Method
  !
  ! Use a bubble sorting algorithm to sort the passed array into ascending order
  ! in place. Optionally also returns the indexer which can be used to map other
  ! arrays to the same order.

  INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: IntArray
  
  INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: Indexer

  ! Indexers and temporary storage values
  INTEGER :: i, j, tmp

  ! Fill the indexer with the 1:N, if required
  IF (PRESENT(Indexer)) THEN
    Indexer = [(i, i = 1, SIZE(IntArray))]
  END IF

  ! Now perform the bubble sort
  DO i = 1, SIZE(IntArray)
    DO j = 1, SIZE(IntArray) - i
      IF (IntArray(j) > IntArray(j+1)) THEN
        tmp = IntArray(j)
        IntArray(j) = IntArray(j+1)
        IntArray(j+1) = tmp

        IF (PRESENT(Indexer)) THEN
          tmp = Indexer(j)
          Indexer(j) = Indexer(j+1)
          Indexer(j+1) = tmp
        END IF
      END IF
    END DO
  END DO
END SUBROUTINE sort_int

SUBROUTINE sort_real(RealArray, Indexer)
  !*## Purpose
  !
  ! Sort 1D array of reals in place in ascending order.
  !
  !## Method
  !
  ! Use a bubble sorting algorithm to sort the passed array into ascending order
  ! in place. Optionally also returns the indexer which can be used to map other
  ! arrays to the same order.

  REAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: RealArray
  
  INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: Indexer

  ! Indexers and temporary storage values
  INTEGER :: i, j, tmp

  ! Fill the indexer with the 1:N, if required
  IF (PRESENT(Indexer)) THEN
    Indexer = [(i, i = 1, SIZE(RealArray))]
  END IF

  ! Now perform the bubble sort
  DO i = 1, SIZE(RealArray)
    DO j = 1, SIZE(RealArray) - i
      IF (RealArray(j) > RealArray(j+1)) THEN
        tmp = RealArray(j)
        RealArray(j) = RealArray(j+1)
        RealArray(j+1) = tmp

        IF (PRESENT(Indexer)) THEN
          tmp = Indexer(j)
          Indexer(j) = Indexer(j+1)
          Indexer(j+1) = tmp
        END IF
      END IF
    END DO
  END DO
END SUBROUTINE sort_real

FUNCTION get_dimid(ncID, DimNames) RESULT(DimID)
  !*## Purpose
  !
  ! Retrieve the dimension ID which occurs in the array of names.
  !
  !## Method
  !
  ! Iterate through the list of dimension names provided until one is found in
  ! the netCDF file at ncID. If no such variable is found, fail with
  ! NF90_EBADDIM.

  INTEGER, INTENT(IN) :: ncID
  CHARACTER(LEN=*), DIMENSION(:) :: DimNames

  INTEGER :: DimID

  ! Iterator and status checker
  INTEGER :: i, ok = NF90_EBADDIM

  CheckNames: DO i = 1,SIZE(DimNames)
    ok = NF90_INQ_DIMID(ncID, TRIM(DimNames(i)), DimID)
    IF (ok == NF90_NOERR) THEN
      EXIT CheckNames
    END IF
  END DO CheckNames

  CALL handle_ncstat(ok, "Failed to find any "//TRIM(DimNames(1))//&
    " dimension from the supplied list.")

END FUNCTION get_dimid

FUNCTION get_varid(ncID, VarNames) RESULT(VarID)
  !*## Purpose
  !
  ! Retrieve the variable ID which occurs in the array of names.
  !
  !## Method
  !
  ! Iterate through the list of dimension names provided until one is found in
  ! the netCDF file at ncID. If no such variable is found, fail with
  ! NF90_ENOTVAR.

  INTEGER, INTENT(IN) :: ncID
  CHARACTER(LEN=*), DIMENSION(:) :: VarNames

  INTEGER :: VarID

  ! Iterator and status checker
  INTEGER :: i, ok = NF90_ENOTVAR

  CheckNames: DO i = 1,SIZE(VarNames)
    ok = NF90_INQ_VARID(ncID, TRIM(VarNames(i)), VarID)
    IF (ok == NF90_NOERR) THEN
      EXIT CheckNames
    END IF
  END DO CheckNames

  CALL handle_ncstat(ok, "Failed to find any "//TRIM(VarNames(1))//&
    " variable from the supplied list.")

END FUNCTION get_varid

SUBROUTINE handle_ncstat(ncStatus, errMsg)
  !*## Purpose
  !
  ! Handle NetCDF return statuses and return informative error messages if the
  ! operation failed for any reason.
  !
  !## Method
  !
  ! Compare the returned status to NF90_NOERR and if not equivalent, put the
  ! passed error message in the error stream as well as the generated NF90 error
  ! string.

  INTEGER, INTENT(IN) :: ncStatus
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: errMsg

  IF (ncStatus /= NF90_NOERR) THEN
    WRITE(ERROR_UNIT,'(A)') "Error in netCDF operation."
    IF (PRESENT(errMsg)) THEN
      WRITE(ERROR_UNIT,'(A)') "Local error:", errMsg
    END IF
    WRITE(ERROR_UNIT,'(A)') "NetCDF error:", TRIM(NF90_STRERROR(ncStatus))
    STOP ncStatus
  END IF

END SUBROUTINE handle_ncstat

END MODULE common_module
