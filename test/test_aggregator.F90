MODULE aggregator_tests

  USE fortuno_interface_m,  ONLY: CHECK, TEST_LIST, SUITE
  USE aggregator_module
  USE time_module,          ONLY: days_in_month, days_in_year
  USE utils_for_tests,      ONLY: fill_test_data

  IMPLICIT NONE

CONTAINS

  SUBROUTINE test_1D_aggregators()
    !*## Purpose
    !
    ! Test the 1D array aggregator over various time periods.

    REAL, DIMENSION(:,:), ALLOCATABLE :: TestData
    REAL, DIMENSION(:), ALLOCATABLE :: Ans
    INTEGER :: Year, TimeStep, StepsInYear
    INTEGER :: t, p, i
    CHARACTER(LEN=10) :: TimePeriod
    CLASS(Aggregator), ALLOCATABLE :: MeanAgg, SumAgg, MinAgg, MaxAgg
    LOGICAL :: Trigger, Success

    ! Set a timestep length of 6 hours
    Year = 2004
    Timestep = 6 * 3600
    
    ! Create a year of test data of vector length 10
    StepsInYear = days_in_year(Year) * 24 * 3600 / Timestep
    ALLOCATE(TestData(10, StepsInYear))

    DO t = 1, StepsInYear
      DO i = 1, 10
        TestData(i, t) = fill_test_data(Year, t * Timestep, i)
      END DO
    END DO

    ! Initialise aggregators- do some test for each time period
    TimePeriod = "timestep"
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "mean", Timestep, Year, MeanAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "sum", Timestep, Year, SumAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "max", Timestep, Year, MaxAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "min", Timestep, Year, MinAgg)

    ! Iterate until the time period has elapsed
    DO t = 1, StepsInYear
      Trigger = MeanAgg%accumulate_data(TestData(:, t))
      Trigger = SumAgg%accumulate_data(TestData(:, t))
      Trigger = MaxAgg%accumulate_data(TestData(:, t))
      Trigger = MinAgg%accumulate_data(TestData(:, t))
      
      IF (Trigger) THEN
        EXIT
      ENDIF
    END DO

    ! Check the results
    ! There should have been only 1 step before triggering the aggregators
    Success = (t == 1)
    CALL CHECK(SUCCESS)

    ! Check the contents of the aggregators- for the timestep, each should be
    ! the same as the vector of data passed in
    CALL MeanAgg%get_data(Ans)
    Success = (ALL(Ans == TestData(:, 1)))
    CALL CHECK(SUCCESS)

    CALL SumAgg%get_data(Ans)
    Success = (ALL(Ans == TestData(:, 1)))
    CALL CHECK(SUCCESS)

    CALL MaxAgg%get_data(Ans)
    Success = (ALL(Ans == TestData(:, 1)))
    CALL CHECK(SUCCESS)

    CALL MinAgg%get_data(Ans)
    Success = (ALL(Ans == TestData(:, 1)))
    CALL CHECK(SUCCESS)

    ! Check that the new trigger point is set- can jsut do that for one
    CALL MeanAgg%get_trigger(Year, t+1)
    Success = (MeanAgg%Trigger == 1)
    CALL CHECK(SUCCESS)

    ! Now move onto daily aggregator
    TimePeriod = "daily"
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "mean", Timestep, Year, MeanAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "sum", Timestep, Year, SumAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "max", Timestep, Year, MaxAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "min", Timestep, Year, MinAgg)

    ! Iterate until the time period has elapsed
    DO t = 1, StepsInYear
      Trigger = MeanAgg%accumulate_data(TestData(:, t))
      Trigger = SumAgg%accumulate_data(TestData(:, t))
      Trigger = MaxAgg%accumulate_data(TestData(:, t))
      Trigger = MinAgg%accumulate_data(TestData(:, t))
      
      IF (Trigger) THEN
        EXIT
      ENDIF
    END DO

    ! Check the results
    ! There should be 4 steps before triggering
    Success = (t == 4)
    CALL CHECK(SUCCESS)

    ! Check the contents of the aggregators- now we actually have to some
    ! meaningful checking
    CALL MeanAgg%get_data(Ans)
    Success = (ALL(Ans == SUM(TestData(:, 1:t), 2) / t))
    CALL CHECK(SUCCESS)

    CALL SumAgg%get_data(Ans)
    Success = (ALL(Ans == SUM(TestData(:, 1:t), 2)))
    CALL CHECK(SUCCESS)

    CALL MaxAgg%get_data(Ans)
    Success = (ALL(Ans == MAXVAL(TestData(:, 1:t), 2)))
    CALL CHECK(SUCCESS)

    CALL MinAgg%get_data(Ans)
    Success = (ALL(Ans == MINVAL(TestData(:, 1:t), 2)))
    CALL CHECK(SUCCESS)

    ! Check that the new trigger point is set- can jsut do that for one
    CALL MeanAgg%get_trigger(Year, t+1)
    Success = (MeanAgg%Trigger == 4)
    CALL CHECK(SUCCESS)
    
    ! Now move onto monthly aggregator
    TimePeriod = "monthly"
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "mean", Timestep, Year, MeanAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "sum", Timestep, Year, SumAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "max", Timestep, Year, MaxAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "min", Timestep, Year, MinAgg)

    ! Iterate until the time period has elapsed
    DO t = 1, StepsInYear
      Trigger = MeanAgg%accumulate_data(TestData(:, t))
      Trigger = SumAgg%accumulate_data(TestData(:, t))
      Trigger = MaxAgg%accumulate_data(TestData(:, t))
      Trigger = MinAgg%accumulate_data(TestData(:, t))
      
      IF (Trigger) THEN
        EXIT
      ENDIF
    END DO

    ! Check the results
    ! There should be 4 steps per day over 31 days before triggering
    Success = (t == (4 * days_in_month(1, Year)))
    CALL CHECK(SUCCESS)

    ! Check the contents of the aggregators- now we actually have to some
    ! meaningful checking
    CALL MeanAgg%get_data(Ans)
    WRITE(*,*) "Ans:", Ans, "TestData:", SUM(TestData(:, 1:t), 2) / t
    Success = (ALL(Ans == SUM(TestData(:, 1:t), 2) / t))
    CALL CHECK(SUCCESS)

    CALL SumAgg%get_data(Ans)
    Success = (ALL(Ans == SUM(TestData(:, 1:t), 2)))
    CALL CHECK(SUCCESS)

    CALL MaxAgg%get_data(Ans)
    Success = (ALL(Ans == MAXVAL(TestData(:, 1:t), 2)))
    CALL CHECK(SUCCESS)

    CALL MinAgg%get_data(Ans)
    Success = (ALL(Ans == MINVAL(TestData(:, 1:t), 2)))
    CALL CHECK(SUCCESS)

    ! Check that the new trigger point is set- can just do that for one
    CALL MeanAgg%get_trigger(Year, t+1)
    Success = (MeanAgg%Trigger == (4 * days_in_month(2, Year)))
    CALL CHECK(SUCCESS)

    ! Finally the yearly aggregator
    TimePeriod = "yearly"
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "mean", Timestep, Year, MeanAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "sum", Timestep, Year, SumAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "max", Timestep, Year, MaxAgg)
    CALL initialise_aggregator(TestData(:, 1), TRIM(TimePeriod),&
      "min", Timestep, Year, MinAgg)

    ! Iterate until the time period has elapsed
    DO t = 1, StepsInYear
      Trigger = MeanAgg%accumulate_data(TestData(:, t))
      Trigger = SumAgg%accumulate_data(TestData(:, t))
      Trigger = MaxAgg%accumulate_data(TestData(:, t))
      Trigger = MinAgg%accumulate_data(TestData(:, t))
      
      IF (Trigger) THEN
        EXIT
      ENDIF
    END DO

    ! Check the results
    ! There should be 4 steps per day over a year before triggering
    Success = (t == (4 * days_in_year(Year)))
    CALL CHECK(SUCCESS)

    ! Check the contents of the aggregators- now we actually have to some
    ! meaningful checking
    CALL MeanAgg%get_data(Ans)
    Success = (ALL(Ans == SUM(TestData(:, 1:t), 2) / t))
    CALL CHECK(SUCCESS)

    CALL SumAgg%get_data(Ans)
    Success = (ALL(Ans == SUM(TestData(:, 1:t), 2)))
    CALL CHECK(SUCCESS)

    CALL MaxAgg%get_data(Ans)
    Success = (ALL(Ans == MAXVAL(TestData(:, 1:t), 2)))
    CALL CHECK(SUCCESS)

    CALL MinAgg%get_data(Ans)
    Success = (ALL(Ans == MINVAL(TestData(:, 1:t), 2)))
    CALL CHECK(SUCCESS)

    ! Check that the new trigger point is set- can jsut do that for one
    CALL MeanAgg%get_trigger(Year, t+1)
    Success = (MeanAgg%Trigger == (4 * days_in_year(Year+1)))
    CALL CHECK(SUCCESS)

  END SUBROUTINE test_1D_aggregators

  SUBROUTINE test_2D_aggregators()
    !*## Purpose
    !
    ! Test the 1D array aggregator over various time periods.

    REAL, DIMENSION(:,:,:), ALLOCATABLE :: TestData
    REAL, DIMENSION(:,:), ALLOCATABLE :: Ans
    INTEGER :: Year, TimeStep, StepsInYear
    INTEGER :: t, p, i, j
    CHARACTER(LEN=10) :: TimePeriod
    CLASS(Aggregator), ALLOCATABLE :: MeanAgg, SumAgg, MinAgg, MaxAgg
    LOGICAL :: Trigger, Success

    ! Set a timestep length of 6 hours
    Year = 2004
    Timestep = 6 * 3600
    
    ! Create a year of test data of vector length 10
    StepsInYear = days_in_year(Year) * 24 * 3600 / Timestep
    ALLOCATE(TestData(10, 10, StepsInYear))

    DO t = 1, StepsInYear
      DO j = 1, 10
        DO i = 1, 10
          TestData(i, j, t) = fill_test_data(Year, t * Timestep, i, j)
        END DO
      END DO
    END DO

    ! Initialise aggregators- do some test for each time period
    TimePeriod = "timestep"
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "mean", Timestep, Year, MeanAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "sum", Timestep, Year, SumAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "max", Timestep, Year, MaxAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "min", Timestep, Year, MinAgg)

    ! Iterate until the time period has elapsed
    DO t = 1, StepsInYear
      Trigger = MeanAgg%accumulate_data(TestData(:, :, t))
      Trigger = SumAgg%accumulate_data(TestData(:, :, t))
      Trigger = MaxAgg%accumulate_data(TestData(:, :, t))
      Trigger = MinAgg%accumulate_data(TestData(:, :, t))
      
      IF (Trigger) THEN
        EXIT
      ENDIF
    END DO

    ! Check the results
    ! There should have been only 1 step before triggering the aggregators
    Success = (t == 1)
    CALL CHECK(SUCCESS)

    ! Check the contents of the aggregators- for the timestep, each should be
    ! the same as the vector of data passed in
    CALL MeanAgg%get_data(Ans)
    Success = (ALL(Ans == TestData(:, :, 3)))
    CALL CHECK(SUCCESS)

    CALL SumAgg%get_data(Ans)
    Success = (ALL(Ans == TestData(:, :, 3)))
    CALL CHECK(SUCCESS)

    CALL MaxAgg%get_data(Ans)
    Success = (ALL(Ans == TestData(:, :, 3)))
    CALL CHECK(SUCCESS)

    CALL MinAgg%get_data(Ans)
    Success = (ALL(Ans == TestData(:, :, 3)))
    CALL CHECK(SUCCESS)

    ! Check that the new trigger point is set- can jsut do that for one
    CALL MeanAgg%get_trigger(Year, t+1)
    Success = (MeanAgg%Trigger == 1)
    CALL CHECK(SUCCESS)

    ! Now move onto daily aggregator
    TimePeriod = "daily"
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "mean", Timestep, Year, MeanAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "sum", Timestep, Year, SumAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "max", Timestep, Year, MaxAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "min", Timestep, Year, MinAgg)

    ! Iterate until the time period has elapsed
    DO t = 1, StepsInYear
      Trigger = MeanAgg%accumulate_data(TestData(:, :, t))
      Trigger = SumAgg%accumulate_data(TestData(:, :, t))
      Trigger = MaxAgg%accumulate_data(TestData(:, :, t))
      Trigger = MinAgg%accumulate_data(TestData(:, :, t))
      
      IF (Trigger) THEN
        EXIT
      ENDIF
    END DO

    ! Check the results
    ! There should be 4 steps before triggering
    Success = (t == 4)
    CALL CHECK(SUCCESS)

    ! Check the contents of the aggregators- now we actually have to some
    ! meaningful checking
    CALL MeanAgg%get_data(Ans)
    Success = (ALL(Ans == SUM(TestData(:, :, 1:t), 3) / t))
    CALL CHECK(SUCCESS)

    CALL SumAgg%get_data(Ans)
    Success = (ALL(Ans == SUM(TestData(:, :, 1:t), 3)))
    CALL CHECK(SUCCESS)

    CALL MaxAgg%get_data(Ans)
    Success = (ALL(Ans == MAXVAL(TestData(:, :, 1:t), 3)))
    CALL CHECK(SUCCESS)

    CALL MinAgg%get_data(Ans)
    Success = (ALL(Ans == MINVAL(TestData(:, :, 1:t), 3)))
    CALL CHECK(SUCCESS)

    ! Check that the new trigger point is set- can jsut do that for one
    CALL MeanAgg%get_trigger(Year, t+1)
    Success = (MeanAgg%Trigger == 4)
    CALL CHECK(SUCCESS)
    
    ! Now move onto monthly aggregator
    TimePeriod = "monthly"
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "mean", Timestep, Year, MeanAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "sum", Timestep, Year, SumAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "max", Timestep, Year, MaxAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "min", Timestep, Year, MinAgg)

    ! Iterate until the time period has elapsed
    DO t = 1, StepsInYear
      Trigger = MeanAgg%accumulate_data(TestData(:, :, t))
      Trigger = SumAgg%accumulate_data(TestData(:, :, t))
      Trigger = MaxAgg%accumulate_data(TestData(:, :, t))
      Trigger = MinAgg%accumulate_data(TestData(:, :, t))
      
      IF (Trigger) THEN
        EXIT
      ENDIF
    END DO

    ! Check the results
    ! There should be 4 steps per day over 31 days before triggering
    Success = (t == (4 * days_in_month(1, Year)))
    CALL CHECK(SUCCESS)

    ! Check the contents of the aggregators- now we actually have to some
    ! meaningful checking
    CALL MeanAgg%get_data(Ans)
    WRITE(*,*) "Ans:", Ans, "TestData:", SUM(TestData(:, :, 1:t), 2) / t
    Success = (ALL(Ans == SUM(TestData(:, :, 1:t), 3) / t))
    CALL CHECK(SUCCESS)

    CALL SumAgg%get_data(Ans)
    Success = (ALL(Ans == SUM(TestData(:, :, 1:t), 3)))
    CALL CHECK(SUCCESS)

    CALL MaxAgg%get_data(Ans)
    Success = (ALL(Ans == MAXVAL(TestData(:, :, 1:t), 3)))
    CALL CHECK(SUCCESS)

    CALL MinAgg%get_data(Ans)
    Success = (ALL(Ans == MINVAL(TestData(:, :, 1:t), 3)))
    CALL CHECK(SUCCESS)

    ! Check that the new trigger point is set- can jsut do that for one
    CALL MeanAgg%get_trigger(Year, t+1)
    Success = (MeanAgg%Trigger == (4 * days_in_month(2, Year)))
    CALL CHECK(SUCCESS)

    ! Finally the yearly aggregator
    TimePeriod = "yearly"
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "mean", Timestep, Year, MeanAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "sum", Timestep, Year, SumAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "max", Timestep, Year, MaxAgg)
    CALL initialise_aggregator(TestData(:, :, 1), TRIM(TimePeriod),&
      "min", Timestep, Year, MinAgg)

    ! Iterate until the time period has elapsed
    DO t = 1, StepsInYear
      Trigger = MeanAgg%accumulate_data(TestData(:, :, t))
      Trigger = SumAgg%accumulate_data(TestData(:, :, t))
      Trigger = MaxAgg%accumulate_data(TestData(:, :, t))
      Trigger = MinAgg%accumulate_data(TestData(:, :, t))
      
      IF (Trigger) THEN
        EXIT
      ENDIF
    END DO

    ! Check the results
    ! There should be 4 steps per day over 31 days before triggering
    Success = (t == (4 * days_in_year(Year)))
    CALL CHECK(SUCCESS)

    ! Check the contents of the aggregators- now we actually have to some
    ! meaningful checking
    CALL MeanAgg%get_data(Ans)
    Success = (ALL(Ans == SUM(TestData(:, :, 1:t), 3) / t))
    CALL CHECK(SUCCESS)

    CALL SumAgg%get_data(Ans)
    Success = (ALL(Ans == SUM(TestData(:, :, 1:t), 3)))
    CALL CHECK(SUCCESS)

    CALL MaxAgg%get_data(Ans)
    Success = (ALL(Ans == MAXVAL(TestData(:, :, 1:t), 3)))
    CALL CHECK(SUCCESS)

    CALL MinAgg%get_data(Ans)
    Success = (ALL(Ans == MINVAL(TestData(:, :, 1:t), 3)))
    CALL CHECK(SUCCESS)

    ! Check that the new trigger point is set- can jsut do that for one
    CALL MeanAgg%get_trigger(Year, t+1)
    Success = (MeanAgg%Trigger == (4 * days_in_year(Year+1)))
    CALL CHECK(SUCCESS)

  END SUBROUTINE test_2d_aggregators
  
END MODULE aggregator_tests
