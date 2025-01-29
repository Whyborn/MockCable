PROGRAM test_common
  
USE common_module, ONLY: sort, approx_equal,&
  find_largest_element_less_than_sorted

IMPLICIT NONE

CALL test_sort()

CALL test_find_largest_element_less_than_sorted()

CONTAINS

SUBROUTINE test_sort()
  INTEGER, DIMENSION(10) :: v = [5, 1, 2, 7, 9, 10, 6, 8, 3, 4]
  INTEGER, DIMENSION(10) :: vAns = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

  INTEGER, DIMENSION(6) :: vDup = [3, 2, 1, 2, 3, 4]
  INTEGER, DIMENSION(6) :: vDupAns = [1, 2, 2, 3, 3, 4]

  CALL sort(v)
  CALL sort(vDup)

  IF (.NOT. ALL(v == vAns)) THEN
    WRITE(*,*) "Sort test 1 failed"
  END IF

  IF (.NOT. ALL(vDup == vDupAns)) THEN
    WRITE(*,*) "Sort test 2 failed"
  END IF

END SUBROUTINE test_sort

SUBROUTINE test_find_largest_element_less_than_sorted()
  INTEGER, DIMENSION(10) :: vSimple = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  INTEGER, DIMENSION(8) :: vDup = [1, 2, 2, 4, 5, 6, 6, 10]
  INTEGER :: UpperLimit = 7
  INTEGER :: vSimpleAns, vDupAns

  vSimpleAns = find_largest_element_less_than_sorted(vSimple, UpperLimit)
  vDupAns = find_largest_element_less_than_sorted(vDup, UpperLimit)

  IF (vSimpleAns /= 6) THEN
    WRITE(*,*) "Find largest test 1 failed"
  END IF

  IF (vDupAns /= 7) THEN
    WRITE(*,*) "Find largest test 2 failed"
  END IF
END SUBROUTINE test_find_largest_element_less_than_sorted

END PROGRAM test_common

  
