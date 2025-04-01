MODULE mock_tests

  USE fortuno_serial, ONLY: IS_EQUAL, TEST => SERIAL_CASE_ITEM,&
    CHECK => SERIAL_CHECK, SUITE => serial_suite_item, TEST_LIST
  USE common_tests
  !USE datasetreader_tests
  !USE output_tests

  IMPLICIT NONE

CONTAINS

  FUNCTION tests()
    TYPE(TEST_LIST) :: Tests

    !Tests = TEST_LIST([&
      !TEST("test_sort", test_sort),&
      !TEST("test_approx_equal"),&
    !])
    Tests = TEST_LIST([&
      SUITE("common", TEST_LIST([&
        TEST("test_sort", test_sort),&
        TEST("test_approx_equal", test_approx_equal)&
        ]))&
      ])

  END FUNCTION tests

END MODULE mock_tests

PROGRAM test_mock
  
  USE fortuno_serial, ONLY: EXECUTE_SERIAL_CMD_APP
  USE mock_tests, ONLY: tests

  IMPLICIT NONE

  CALL EXECUTE_SERIAL_CMD_APP(tests())

END PROGRAM test_mock
