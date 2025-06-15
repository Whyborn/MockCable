! aggregator.F90 contains the workings of the Aggregator type. The Aggregator
! type is intended to assist in temporally aggregating data, primarily for the
! purpose of outputting on intervals. Potentially useable in other science
! areas as well.

MODULE aggregator_module
  !* The ```Aggregator``` type is used to aggregate data over specified time
  ! time periods. Allowed time periods are:
  ! * per timestep
  ! * per day
  ! * per month
  ! * per year
  !
  ! The possible aggregation methods over each of these time periods are:
  ! * mean
  ! * sum
  ! * max
  ! * min
  !
  ! Each of these methods is performed per-element of the array. For example,
  ! if the soil moisture at an instant in time over many tiles and layers is
  ! represented by a 2D array, the aggregator will perform the aggregation
  ! per element in the array, so the resulting array is the same size as the
  ! original array representing the snapshot in time.
  !
  ! The aggregator is initialised via the
  ! [[aggregator_module:initialise_aggregator]] subroutine. While internally,
  ! aggregators representing different rank arrays must have their own
  ! definitions (```Aggregator1D``` and ```Aggregator2D```), the base class
  ! should be used in other sections of the code. The specific internal
  ! representation is chosen based on the rank of the array passed as the
  ! ```Mold``` to the initialiser. This would be a typical code snippet to
  ! initialise an ```Aggregator```:
  !
  ! ```fortran
  ! CLASS(Aggregator), ALLOCATABLE :: AggFor1D, AggFor2D
  ! REAL, DIMENSION(:), ALLOCATABLE :: Some1DData
  ! REAL, DIMENSION(:), ALLOCATABLE :: Some2DData
  ! ...
  ! ALLOCATE(Some1DData(10), Some2DData(6,10))
  ! CALL initialise_aggregator(Some1DData, "daily", "mean", StepSize,&
  !   StartYear, AggFor1D)
  ! CALL initialise_aggregator(Some2DData, "monthly", "max", StepSize,&
  !   StartYear, AggFor2D)
  ! ```
  !
  ! Now we have 2 aggregators, one which will be used to compute the daily mean
  ! of a quantity represented a length 10 vector, and one to compute the
  ! monthly maximum of a 6x10 matrix. The ```StepSize``` is the timestep at 
  ! which the data will be aggregated- typically the time step of the
  ! simulation in seconds. ```StartYear``` is the year the aggregator is being
  ! initialised, usually the starting year of the run.
  !
  ! To accumulate the data in the aggregator, use the ```accumulate_data``` 
  ! procedure bound to the aggregator, e.g.
  !
  ! ```fortran
  ! LOGICAL :: Trigger
  ! ...
  ! Trigger = AggFor1D%accumlate_data(Some1DStateData)
  ! ```
  !
  ! The trigger is a ```LOGICAL``` used to determine when the specific
  ! aggregation period is over. This would be used to trigger an action e.g.
  ! writing data to file, storing data for climatology. The data within the
  ! aggregator is retrieved via [[aggregator_module:get_data]]. Once some 
  ! action has been triggered, the aggregator's trigger point has to be
  ! recomputed, in case of non-constant periods i.e. months or years. So an
  ! an example snippet would look like:
  !
  ! ```fortran
  ! Trigger = AggFor1D%accumulate_data(Some1DStateData)
  !
  ! IF (Trigger) THEN
  !   ! Do something with the data in the aggregator
  !   CALL AggFor1D%get_data(Some1DData) ! Reuse the mold from initialisation
  !
  !   ! Do something useful with the data in ```Some1DData```
  !
  !   CALL AggFor1D%get_trigger(CurrYear, Step+1)
  ! END IF
  ! ```

  USE iso_fortran_env,  ONLY: ERROR_UNIT
  USE time_module,      ONLY: days_in_month, month_from_day, days_in_year,&
                              SecsInDay

  IMPLICIT NONE

  TYPE, ABSTRACT :: Aggregator
    !*## Purpose
    !
    ! Type to manage the aggregation of data over time periods. The data
    ! structure is rank agnostic, and supports the following aggregation
    ! methods:
    ! * Mean
    ! * Sum
    ! * Min
    ! * Max
    !
    ! The methods can be applied over the following time periods:
    ! * On timestep
    ! * Daily
    ! * Monthly
    ! * Yearly
    !
    ! The method and time periods are specified on initialisation. Note that
    ! the timestep does not have to the fundamental timestep of the simulation-
    ! it can be any interval between calls to a science routine. A mold for
    ! the data that is being aggregated must also be supplied i.e. an array
    ! with the same shape as the desired data.
    !
    ! ## Method
    !
    ! The Aggregator class is an abstract class, which contains information
    ! about the number of "accumulations" instances have occurred, when the
    ! current aggregation period ends and the accumulated data. The potential
    ! difference ranks of the accumulate data are handled by concrete child
    ! types of the abstract Aggregator class. 

    ! The aggregation method and method for determining the length of the 
    ! interval are bound to the type on initialisation. 

    INTEGER :: Trigger, Timestep, Counter = 0
    PROCEDURE(trigger_method), POINTER :: trigger_method
    PROCEDURE(accumulate_array_data), POINTER :: accum_array_data
  CONTAINS
    PROCEDURE(get_agg_data), DEFERRED :: get_data
    PROCEDURE :: accumulate_data
    PROCEDURE :: get_trigger
  END TYPE Aggregator

  ABSTRACT INTERFACE
    SUBROUTINE accumulate_array_data(this, CurrentData)
      !*## Purpose
      !
      ! Abstract method used as a placeholder for a specific aggregation method
      ! and array rank.
      IMPORT Aggregator
      CLASS(Aggregator), INTENT(INOUT) :: this
      REAL, DIMENSION(..), INTENT(IN) :: CurrentData
    END SUBROUTINE accumulate_array_data
  END INTERFACE

  ABSTRACT INTERFACE
    SUBROUTINE trigger_method(this, Year, Step)
      !*## Purpose
      !
      ! Abstract method used as a placeholder for a specific aggregation method
      ! and array rank.
      IMPORT Aggregator
      CLASS(Aggregator), INTENT(INOUT) :: this
      INTEGER, INTENT(IN) :: Year, Step
    END SUBROUTINE trigger_method
  END INTERFACE

  ABSTRACT INTERFACE
    SUBROUTINE get_agg_data(this, Storage)
      !*## Purpose
      !
      ! Retrieve the data from the aggregator in a polymorphic way
      IMPORT Aggregator
      CLASS(Aggregator), INTENT(IN) :: this
      REAL, DIMENSION(..), ALLOCATABLE, INTENT(OUT) :: Storage
    END SUBROUTINE get_agg_data
  END INTERFACE

  TYPE, EXTENDS(Aggregator) :: Aggregator1D
    !*## Purpose
    !
    ! Child of the Aggregator abstract type used for 1D array aggregation.

    REAL, ALLOCATABLE :: DataAccum(:)
  CONTAINS
    PROCEDURE :: get_data => get_agg_data_1D
  END TYPE Aggregator1D

  TYPE, EXTENDS(Aggregator) :: Aggregator2D
    !*## Purpose
    !
    ! Child of the Aggregator abstract type used for 2D array aggregation.

    REAL, ALLOCATABLE :: DataAccum(:,:)
  CONTAINS
    PROCEDURE :: get_data => get_agg_data_2D
  END TYPE Aggregator2D

  ! Possible periods for aggregation
  PRIVATE :: WritePeriod_, OnTimestep, Daily, Monthly, Yearly
  ENUM, BIND(C)
    ENUMERATOR :: WritePeriod_ = 0
    ENUMERATOR :: OnTimestep = 1
    ENUMERATOR :: Daily = 2
    ENUMERATOR :: Monthly = 3
    ENUMERATOR :: Yearly = 4
  END ENUM
    
  ! Possible aggregation methods- don't clash with intrinsics SUM, MIN, MAX
  PRIVATE :: AggMethod_, MeanAgg, SumAgg, MinAgg, MaxAgg
  ENUM, BIND(C)
    ENUMERATOR :: AggMethod_ = 0
    ENUMERATOR :: MeanAgg = 1
    ENUMERATOR :: SumAgg = 2
    ENUMERATOR :: MinAgg = 3
    ENUMERATOR :: MaxAgg = 4
  END ENUM

CONTAINS

  FUNCTION accumulate_data(this, CurrentData) RESULT(IsTriggered)
    !*## Purpose
    !
    ! Procedure used to as the "public" interface used to accumulate data with
    ! the aggregator. This function is bound to the Aggregator type.
    ! Returns a logical denoting whether the aggregation period
    ! has ended. Use the return value to trigger events e.g. writing.

    CLASS(Aggregator), INTENT(INOUT) :: this
    REAL, DIMENSION(..), INTENT(IN) :: CurrentData
    LOGICAL :: IsTriggered

    ! By default, the accumulator is not triggered
    IsTriggered = .FALSE.

    ! Accumulate the array
    CALL this%accum_array_data(CurrentData)

    this%Counter = this%Counter + 1

    ! Check if we hit the trigger
    IF (this%Counter == this%Trigger) THEN
      this%Counter = 0
      IsTriggered = .TRUE.
    END IF

  END FUNCTION accumulate_data

  SUBROUTINE get_trigger(this, Year, Step)
    !*## Purpose
    !
    ! Get the next trigger point for the accumulator,
    CLASS(Aggregator), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: Year, Step

    CALL this%trigger_method(Year, Step)

  END SUBROUTINE get_trigger

  SUBROUTINE get_agg_data_1D(this, Storage)
    CLASS(Aggregator1D), INTENT(IN) :: this
    REAL, DIMENSION(..), ALLOCATABLE, INTENT(OUT) :: Storage

    SELECT RANK (Storage)
    RANK (1)
      Storage = this%DataAccum

    RANK DEFAULT
      WRITE(ERROR_UNIT, '(A)') "Something went very wrong"

    END SELECT
    
  END SUBROUTINE get_agg_data_1D
    
  SUBROUTINE get_agg_data_2D(this, Storage)
    CLASS(Aggregator2D), INTENT(IN) :: this
    REAL, DIMENSION(..), ALLOCATABLE, INTENT(OUT) :: Storage

    SELECT RANK (Storage)
    RANK (2)
      Storage = this%DataAccum

    RANK DEFAULT
      WRITE(ERROR_UNIT, '(A)') "Something went very wrong"

    END SELECT
    
  END SUBROUTINE get_agg_data_2D
    
  ! -------------------------------------------------------------------------!
  ! Now follows the fundamental aggregation methods- separate routines are
  ! required for each method and each potential rank

  SUBROUTINE sum_accumulation(this, CurrentData)
    CLASS(Aggregator), INTENT(INOUT) :: this
    REAL, DIMENSION(..), INTENT(IN) :: CurrentData

    ! Just capture everything in the Rank 1 case, as it's the only one relevant
    SELECT TYPE (this)
    TYPE IS (Aggregator1D)
      SELECT RANK (CurrentData)
      RANK (1)
        ! If a write has just been triggered, reset the accumulator
        IF (this%Counter == 0) THEN
          this%DataAccum = CurrentData
        ELSE
          this%DataAccum = this%DataAccum + CurrentData
        END IF

      RANK DEFAULT
        WRITE(ERROR_UNIT, '(A)') "Something went very wrong"

      END SELECT
    
    TYPE IS (Aggregator2D)

      SELECT RANK (CurrentData)
      RANK (2)
        ! If a write has just been triggered, reset the accumulator
        IF (this%Counter == 0) THEN
          this%DataAccum = CurrentData
        ELSE
          this%DataAccum = this%DataAccum + CurrentData
        END IF

      RANK DEFAULT
        WRITE(ERROR_UNIT, '(A)') "Something went very wrong"

      END SELECT
    
    END SELECT

  END SUBROUTINE sum_accumulation

  SUBROUTINE mean_accumulation(this, CurrentData)
    CLASS(Aggregator), INTENT(INOUT) :: this
    REAL, DIMENSION(..), INTENT(IN) :: CurrentData

    ! Just capture everything in the Rank 1 case, as it's the only one relevant
    SELECT TYPE (this)
    TYPE IS (Aggregator1D)
      
      SELECT RANK (CurrentData)
      RANK (1)
        ! If a write has just been triggered, reset the accumulator
        IF (this%Counter == 0) THEN
          this%DataAccum = CurrentData / this%Trigger
        ELSE
          this%DataAccum = this%DataAccum + CurrentData / this%Trigger
        END IF

      RANK DEFAULT
        WRITE(ERROR_UNIT, '(A)') "Something went very wrong"

      END SELECT
    
    TYPE IS (Aggregator2D)
      
      SELECT RANK (CurrentData)
      RANK (2)
        ! If a write has just been triggered, reset the accumulator
        IF (this%Counter == 0) THEN
          this%DataAccum = CurrentData / this%Trigger
        ELSE
          this%DataAccum = this%DataAccum + CurrentData / this%Trigger
        END IF

      RANK DEFAULT
        WRITE(ERROR_UNIT, '(A)') "Something went very wrong"

      END SELECT
    
    END SELECT

  END SUBROUTINE mean_accumulation

  SUBROUTINE max_accumulation(this, CurrentData)
    CLASS(Aggregator), INTENT(INOUT) :: this
    REAL, DIMENSION(..), INTENT(IN) :: CurrentData

    ! Just capture everything in the Rank 1 case, as it's the only one relevant
    SELECT TYPE (this)
    TYPE IS (Aggregator1D)
      
      SELECT RANK (CurrentData)
      RANK (1)
        ! If a write has just been triggered, reset the accumulator
        IF (this%Counter == 0) THEN
          this%DataAccum = CurrentData
        ELSE
          this%DataAccum = MAX(this%DataAccum, CurrentData)
        END IF

      RANK DEFAULT
        WRITE(ERROR_UNIT, '(A)') "Something went very wrong"

      END SELECT
    
    TYPE IS (Aggregator2D)
      
      SELECT RANK (CurrentData)
      RANK (2)
        ! If a write has just been triggered, reset the accumulator
        IF (this%Counter == 0) THEN
          this%DataAccum = CurrentData
        ELSE
          this%DataAccum = MAX(this%DataAccum, CurrentData)
        END IF

      RANK DEFAULT
        WRITE(ERROR_UNIT, '(A)') "Something went very wrong"

      END SELECT
    
    END SELECT

  END SUBROUTINE max_accumulation

  SUBROUTINE min_accumulation(this, CurrentData)
    CLASS(Aggregator), INTENT(INOUT) :: this
    REAL, DIMENSION(..), INTENT(IN) :: CurrentData

    ! Just capture everything in the Rank 1 case, as it's the only one relevant

    SELECT TYPE (this)
    TYPE IS (Aggregator1D)
      
      SELECT RANK (CurrentData)
      RANK (1)
        ! If a write has just been triggered, reset the accumulator
        IF (this%Counter == 0) THEN
          this%DataAccum = CurrentData
        ELSE
          this%DataAccum = MIN(this%DataAccum, CurrentData)
        END IF

      RANK DEFAULT
        WRITE(ERROR_UNIT, '(A)') "Something went very wrong"

      END SELECT
    
    TYPE IS (Aggregator2D)
      
      SELECT RANK (CurrentData)
      RANK (2)
        ! If a write has just been triggered, reset the accumulator
        IF (this%Counter == 0) THEN
          this%DataAccum = CurrentData
        ELSE
          this%DataAccum = MIN(this%DataAccum, CurrentData)
        END IF

      RANK DEFAULT
        WRITE(ERROR_UNIT, '(A)') "Something went very wrong"

      END SELECT
    
    END SELECT

  END SUBROUTINE min_accumulation

!-----------------------------------------------------------------------------!
! Now define the trigger methods- it's important to remember we're always
! getting the trigger for the next period, so for months and years, we need to
! look at the next month/year
  SUBROUTINE timestep_trigger(this, Year, Step)
    CLASS(Aggregator), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: Year, Step

    this%trigger = 1
  END SUBROUTINE timestep_trigger

  SUBROUTINE daily_trigger(this, Year, Step)
    CLASS(Aggregator), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: Year, Step

    this%trigger = SecsInDay / this%Timestep
  END SUBROUTINE daily_trigger

  SUBROUTINE monthly_trigger(this, Year, Step)
    CLASS(Aggregator), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: Year, Step
    INTEGER :: Month

    Month = month_from_day(Step * this%Timestep / SecsInDay, Year)
    Month = MOD(Month + 1, 12)
    this%trigger = (SecsInDay * days_in_month(Month, Year)) / this%Timestep

  END SUBROUTINE monthly_trigger
    
  SUBROUTINE yearly_trigger(this, Year, Step)
    CLASS(Aggregator), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: Year, Step

    this%trigger = SecsInDay * days_in_year(Year + 1) / this%Timestep

  END SUBROUTINE yearly_trigger

!-----------------------------------------------------------------------------!

  SUBROUTINE initialise_aggregator(Mold, Period, Method, Timestep, Year, Agg)
    !*## Purpose
    !
    ! Initialise a new aggregator in place, which aggregates data with the same
    ! shape as the Mold, accumulated at the specified Timestep using the 
    ! specified Method over the specified Period.
    ! 
    ! Options for the Period are:
    ! * "timestep"
    ! * "daily"
    ! * "monthly"
    ! * "yearly"
    !
    ! Options for the Method are:
    ! * "mean"
    ! * "sum"
    ! * "min"
    ! * "max"

    REAL, INTENT(IN), DIMENSION(..) :: Mold
    CHARACTER(LEN=*), INTENT(IN) :: Period, Method
    INTEGER, INTENT(IN) :: Timestep, Year
    CLASS(Aggregator), ALLOCATABLE, INTENT(OUT) :: Agg

    ! Need to provide options for the potential concrete type
    TYPE(Aggregator1D) :: Agg1D
    TYPE(Aggregator2D) :: Agg2D

    ! Select the rank- need to do the method here as well
    SELECT RANK(Mold)
    RANK(1)
      ALLOCATE(Agg1D%DataAccum, MOLD=Mold)
      Agg1D%DataAccum = 0.0
      Agg = Agg1D

      ! 1D methods
      IF (Method == "mean") THEN
        Agg%accum_array_data => mean_accumulation
      ELSEIF (Method == "sum") THEN
        Agg%accum_array_data => sum_accumulation
      ELSEIF (Method == "min") THEN
        Agg%accum_array_data => min_accumulation
      ELSEIF (Method == "max") THEN
        Agg%accum_array_data => max_accumulation
      ELSE
        WRITE(ERROR_UNIT, '(A)') "Agg method: "//Method//" is not valid."
      END IF

    RANK(2)
      ALLOCATE(Agg2D%DataAccum, MOLD=Mold)
      Agg2D%DataAccum = 0.0
      Agg = Agg2D

      ! 2D methods
      IF (Method == "mean") THEN
        Agg%accum_array_data => mean_accumulation
      ELSEIF (Method == "sum") THEN
        Agg%accum_array_data => sum_accumulation
      ELSEIF (Method == "min") THEN
        Agg%accum_array_data => min_accumulation
      ELSEIF (Method == "max") THEN
        Agg%accum_array_data => max_accumulation
      ELSE
        WRITE(ERROR_UNIT, '(A)') "Agg method: "//Method//" is not valid."
      END IF

    RANK DEFAULT
      WRITE(ERROR_UNIT,'(A)') "Ranks higher than 2 have not been implemented."
      STOP -1
    END SELECT

    ! Now select the period method
    IF (Period == "timestep") THEN
      Agg%trigger_method => timestep_trigger
    ELSEIF (Period == "daily") THEN
      Agg%trigger_method => daily_trigger
    ELSEIF (Period == "monthly") THEN
      Agg%trigger_method => monthly_trigger
    ELSEIF (Period == "yearly") THEN
      Agg%trigger_method => yearly_trigger
    ELSE
      WRITE(ERROR_UNIT,'(A)') "Specified period:" //Period//" is not valid."
      STOP -1
    END IF

    ! Assign the timestep and get the trigger
    Agg%Timestep = Timestep
    CALL Agg%get_trigger(Year - 1, SecsInDay * days_in_year(Year - 1) / Timestep)

  END SUBROUTINE initialise_aggregator

END MODULE aggregator_module
