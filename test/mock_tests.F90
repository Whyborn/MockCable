MODULE mock_tests

  USE fortuno_interface_m, ONLY: IS_EQUAL, TEST_CASE,&
    CHECK, SUITE
  USE common_tests
  USE time_tests
  !USE datasetreader_tests
  !USE output_tests

  IMPLICIT NONE

CONTAINS

  FUNCTION tests()
    TYPE(TEST_LIST) :: Tests

    Tests = TEST_LIST([&
      SUITE("common", TEST_LIST([&
        TEST_CASE("test_sort", test_sort),&
        TEST_CASE("test_approx_equal", test_approx_equal),&
        TEST_CASE("test_find_largest_less_than", test_find_largest_less_than)&
        ])),&
      SUITE("time", TEST_LIST([&
        TEST_CASE("test_calendars", test_calendars),&
        TEST_CASE("test_to_from_string", test_to_from_string),&
        TEST_CASE("test_intervals", test_intervals)&
        ]))&
      ])

  END FUNCTION tests

END MODULE mock_tests

PROGRAM test_mock
  
  USE fortuno_interface_m, ONLY: EXECUTE_CMD_APP
  USE mock_tests, ONLY: tests

  IMPLICIT NONE

  CALL EXECUTE_CMD_APP(tests())

END PROGRAM test_mock
