MODULE utils_for_tests

  IMPLICIT NONE

CONTAINS

  FUNCTION fill_test_data(Year, Second_, i_, j_, k_) RESULT(test_value)
    !** Purpose
    !
    ! Function used to generate consistent data for use in testing.

    INTEGER :: Year
    INTEGER, INTENT(IN), OPTIONAL :: Second_, i_, j_, k_
    INTEGER :: Second, i, j, k
    REAL :: test_value

    IF (PRESENT(Second_)) THEN
      Second = Second_
    ELSE
      Second = 0
    ENDIF

    IF (PRESENT(i_)) THEN
      i = i_
    ELSE
      i = 0
    ENDIF

    IF (PRESENT(j_)) THEN
      j = j_
    ELSE
      j = 0
    ENDIF

    IF (PRESENT(k_)) THEN
      k = k_
    ELSE
      k = 0
    ENDIF

    test_value = REAL(MOD(Year, 10)) + REAL(MOD(Second, 50)) / 2 + &
      REAL(MOD(i, 20)) / 5 + REAL(MOD(j, 30)) / 10 + REAL(MOD(k, 40)) / 20

  END FUNCTION fill_test_data

END MODULE utils_for_tests
