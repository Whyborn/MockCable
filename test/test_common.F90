MODULE common_tests
  
  USE fortuno_interface_m, ONLY: CHECK, test_list, suite
  USE common_module, ONLY: sort, approx_equal,&
    find_largest_element_less_than_sorted

  IMPLICIT NONE

CONTAINS

  SUBROUTINE test_sort()
    !*## Purpose
    !
    ! Test the sort function
    !
    !## Method
    !
    ! Generate an array of 100 random elements (with fixed seed) and check that
    ! that the elements are ascending. Also ensure that the created indexer is
    ! correct, by manually sorting the original array and ensuring it is
    ! ascending.

    INTEGER :: SeedSize, i
    INTEGER, DIMENSION(:), ALLOCATABLE :: Seed
    REAL, DIMENSION(100) :: RealArray, UnsortedArray
    INTEGER, DIMENSION(100) :: IntArray, Indexer
    LOGICAL :: Success

    ! Set the seed
    CALL RANDOM_SEED(SIZE=SeedSize)
    ALLOCATE(Seed(SeedSize))
    Seed = 100
    CALL RANDOM_SEED(PUT=Seed)

    ! Now we can assign the arrays
    CALL RANDOM_NUMBER(RealArray)
    UnsortedArray = RealArray
    IntArray = NINT(RealArray * 1000)
    
    ! Perform the sorting and check
    Success = .TRUE.
    CALL sort(RealArray, Indexer)
    DO i = 1, 99
      IF (RealArray(i) > RealArray(i+1)) THEN
        Success = .FALSE.
      END IF
    END DO

    ! Use the indexer to do the sorting and check
    DO i = 1, 100
      RealArray(i) = UnsortedArray(Indexer(i))
    END DO

    DO i = 1, 99
      IF (RealArray(i) > RealArray(i+1)) THEN
        Success = .FALSE.
      END IF
    END DO
    CALL check(Success)
      
    ! Now check the int
    Success = .TRUE.
    CALL sort(IntArray)
    DO i = 1, 99
      IF (IntArray(i) > IntArray(i+1)) THEN
        Success = .FALSE.
      END IF
    END DO
    CALL check(Success)

  END SUBROUTINE test_sort

  SUBROUTINE test_approx_equal()
    !*## Purpose
    !
    ! Test the approx_equal function
    !
    !## Method
    !
    ! Run various tests of approx_equal, with varying the tolerance.
    REAL :: Base, LargeDelta, SmallDelta

    Base = 1.0
    LargeDelta = 1.0e-3
    SmallDelta = 1.0e-5

    CALL check(approx_equal(Base, Base + LargeDelta, 1e-2) == .TRUE.)
    CALL check(approx_equal(Base, Base + SmallDelta) == .TRUE.)
    CALL check(approx_equal(Base, Base - SmallDelta) == .TRUE.)
    CALL check(approx_equal(Base, Base + LargeDelta) == .FALSE.)

  END SUBROUTINE test_approx_equal

!SUBROUTINE test_find_largest_element_less_than_sorted()
  !INTEGER, DIMENSION(10) :: vSimple = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  !INTEGER, DIMENSION(8) :: vDup = [1, 2, 2, 4, 5, 6, 6, 10]
  !INTEGER :: UpperLimit = 3
  !INTEGER :: vSimpleAns, vDupAns

  !vSimpleAns = find_largest_element_less_than_sorted(vSimple, UpperLimit)
  !vDupAns = find_largest_element_less_than_sorted(vDup, UpperLimit)

  !IF (vSimpleAns /= 2) THEN
    !WRITE(*,*) "Find largest test 1 failed"
    !WRITE(*,*) "Computed result is:", vSimpleAns
  !END IF

  !IF (vDupAns /= 3) THEN
    !WRITE(*,*) "Find largest test 2 failed"
  !END IF
!END SUBROUTINE test_find_largest_element_less_than_sorted

END MODULE common_tests
