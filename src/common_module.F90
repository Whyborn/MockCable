MODULE common_module

  USE iso_fortran_env, ONLY: ERROR_UNIT, OUTPUT_UNIT
  USE netcdf, ONLY: NF90_NOERR, NF90_STRERROR, NF90_EBADDIM, NF90_ENOTVAR,&
                    NF90_INQ_DIMID, NF90_INQ_VARID
  USE time_module, ONLY: days_in_month, month_from_day, days_in_year
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

    INTEGER, DIMENSION(:), INTENT(INOUT) :: IntArray
    
    INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT), OPTIONAL :: Indexer

    ! Indexers and temporary storage values
    INTEGER :: i, j, tmp

    ! Fill the indexer with the 1:N, if required
    IF (PRESENT(Indexer)) THEN
      ALLOCATE(Indexer(SIZE(IntArray)))
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

  FUNCTION find_largest_element_less_than_sorted(Values, UpperLimit)&
      RESULT(IndexOfLargest)
    !*## Purpose
    !
    ! Find the index of the largest element in the ascending vector which is
    ! smaller than the specified value.
    !
    !## Method
    !
    ! Use a binary search to logarithmically approach the desired index.

    ! Variables required for the binary search

    INTEGER, DIMENSION(:) :: Values
    INTEGER :: UpperLimit
    INTEGER :: IndexOfLargest

    INTEGER :: Lowerbound, UpperBound, Middle

    ! Set an initial value for the Index
    IndexOfLargest = 0

    ! Check which file we want by using a binary search
    LowerBound = 1
    UpperBound = SIZE(Values)

    ! Remember IndexRange contains the last index for each file- so we want to
    ! find the first index that is greater than our desired index
    DO WHILE (LowerBound <= UpperBound)
      Middle = (LowerBound + UpperBound) / 2
      IF (Values(Middle) <= UpperLimit) THEN
        ! The middle index is less than or equal the desired index
        IF (Values(Middle+1) > UpperLimit) THEN
          EXIT
        ELSE
          ! Adjust the lower bound of our bracket
          LowerBound = Middle + 1
        END IF
      ELSE
        ! Adjust the upper bound of our bracket
        UpperBound = Middle - 1
      END IF
    END DO

    IndexOfLargest = Middle

  END FUNCTION find_largest_element_less_than_sorted

  FUNCTION approx_equal(LHS, RHS, Tolerance) RESULT(IsEqual)
    !*## Purpose
    !
    ! Check whether two floating point numbers are approximately equal, with some
    ! tolerance.
    !
    !## Method
    !
    ! Check whether the delta between the LHS and RHS is greater than the
    ! tolerance.

    REAL :: LHS, RHS
    REAL, OPTIONAL :: Tolerance
    LOGICAL :: IsEqual

    ! Set a default value for the tolerance if it is not already set
    IF (.NOT. PRESENT(Tolerance)) THEN
      Tolerance = 1e-8
    END IF

    ! Check the delta
    IF (ABS(RHS - LHS) <= Tolerance) THEN
      IsEqual = .TRUE.
    ELSE
      IsEqual = .FALSE.
    END IF

  END FUNCTION approx_equal

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

  ! Methods for handling temporal aggregation

  ABSTRACT INTERFACE
    FUNCTION trigger_method(Year, Step, Timestep) RESULT(Trigger)
      INTEGER :: Year, Step, Timestep
      INTEGER :: Trigger
    END FUNCTION
  END INTERFACE

  FUNCTION on_timestep_trigger(Year, Step, Timestep) RESULT(Trigger)
    INTEGER :: Year, Step, Timestep
    INTEGER :: Trigger

    Trigger = 1
  END FUNCTION on_timestep_trigger
    
  FUNCTION daily_trigger(Year, Step, Timestep) RESULT(Trigger)
    INTEGER :: Year, Step, Timestep
    INTEGER :: Trigger

    ! Just number of steps in the day
    Trigger = SecsInDay / Timestep
  END FUNCTION daily_trigger

  FUNCTION monthly_trigger(Year, Step, Timestep) RESULT(Trigger)
    INTEGER :: Year, Step, Timestep
    INTEGER :: Trigger

    INTEGER :: DayOfYear, DaysInMonth

    ! Need to determine which month we're in first
    DayOfYear = CEILING(Step * Timestep / SecsInDay)
    DaysInMonth = days_in_month(month_from_day(DayOfYear, Year))

    Trigger = SecsInDay * DaysInMonth / Timestep
  END FUNCTION monthly_trigger
    
  FUNCTION yearly_trigger(Year, Step, Timestep) RESULT(Trigger)
    INTEGER :: Year, Step, Timestep
    INTEGER :: Trigger

    INTEGER :: DaysInYear

    ! Need to determine how long the year is
    DaysInYear = days_in_year(Year)

    Trigger = SecsInDay * DaysInYear / Timestep
  END FUNCTION monthly_trigger
    
  TYPE, ABSTRACT :: Aggregator
    INTEGER :: Trigger, Counter = 0
  CONTAINS
    PROCEDURE :: accumulate_data
    PROCEDURE(accumulate_array_data), DEFERRED :: accumulate_array_data
    PROCEDURE, POINTER :: get_trigger
  END TYPE Aggregator

  TYPE, EXTENDS(Aggregator) :: Aggregator1D
    REAL, ALLOCATABLE :: DataAccum(:)
  CONTAINS
    PROCEDURE, POINTER :: accumulate_array_data
  END TYPE Aggregator1D

  TYPE, EXTENDS(Aggregator) :: Aggregator2D
    REAL, ALLOCTABLE :: DataAccum(:,:)
  CONTAINS
    PROCEDURE, POINTER :: accumulate_array_data
  END TYPE Aggregator1D

  FUNCTION accumulate_data(this, CurrentData, Year, Step) RESULT(IsTriggered)
    CLASS(Aggregator), INTENT(INOUT) :: this
    REAL, DIMENSION(..), INTENT(IN) :: CurrentData
    INTEGER, INTENT(IN) :: Year, Step
    LOGICAL :: IsTriggered

    ! By default, the accumulator is not triggered
    IsTriggered = .FALSE.

    ! Set the next trigger point
    IF (AggHandler%Counter == 0) THEN
      AggHandler%get_trigger(Year, Step)
    END IF

    ! Accumulate the array
    AggHandler%accumulate_array_data(CurrentData)
    AggHandler%Counter = AggHandler%Counter + 1
    ! Check if we hit the trigger
    IF (AggHandler%Counter == AggHandler%Trigger) THEN
      AggHandler%Counter = 0
      IsTriggered = .TRUE.
    END IF

  END FUNCTION accumulate_data


  ABSTRACT INTERFACE
    SUBROUTINE agg_method(StepData, AccumData, AccumPeriod)
      REAL, DIMENSION(:,:), INTENT(IN) :: StepData
      INTEGER, INTENT(IN) :: AccumPeriod
      REAL, DIMENSION(:,:), INTENT(INOUT) :: AccumData
    END SUBROUTINE agg_method
  END INTERFACE

  SUBROUTINE agg_mean(StepData, AccumData, AccumPeriod)
    REAL, DIMENSION(:,:), INTENT(IN) :: StepData
    INTEGER, INTENT(IN) :: AccumPeriod
    REAL, DIMENSION(:,:), INTENT(INOUT) :: AccumData

    ! Accumulate the data while scaling by the number of steps in the
    ! aggregation period
    AccumData = AccumData + StepData / AccumPeriod
  END SUBROUTINE agg_mean

  SUBROUTINE agg_sum(StepData, AccumData, AccumPeriod)
    REAL, DIMENSION(:,:), INTENT(IN) :: StepData
    INTEGER, INTENT(IN) :: AccumPeriod
    REAL, DIMENSION(:,:), INTENT(INOUT) :: AccumData

    ! Simple summation, don't need to think about the accumulation period
    AccumData = AccumData + StepData
  END SUBROUTINE agg_sum

  SUBROUTINE agg_max(StepData, AccumData, AccumPeriod)
    REAL, DIMENSION(:,:), INTENT(IN) :: StepData
    INTEGER, INTENT(IN) :: AccumPeriod
    REAL, DIMENSION(:,:), INTENT(INOUT) :: AccumData

    ! Intrinsic MAX is elemental, simply apply
    StepData = MAX(StepData, AccumData)
  END SUBROUTINE agg_max

  SUBROUTINE agg_min(StepData, AccumData, AccumPeriod)
    REAL, DIMENSION(:,:), INTENT(IN) :: StepData
    INTEGER, INTENT(IN) :: AccumPeriod
    REAL, DIMENSION(:,:), INTENT(INOUT) :: AccumData

    ! Intrinsic MIN is elemental, simply apply
    StepData = MIN(StepData, AccumData)
  END SUBROUTINE agg_min

END MODULE common_module
