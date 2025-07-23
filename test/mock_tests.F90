MODULE mock_tests

  use fortuno_interface_m, only: test_list
  use common_tests, only: common_test_list
  use time_tests, only: time_test_list
  use test_cable_netcdf, only: cable_netcdf_test_list
  !USE datasetreader_tests
  !USE output_tests

  IMPLICIT NONE

CONTAINS

  FUNCTION tests()
    TYPE(TEST_LIST) :: Tests

    Tests = TEST_LIST([&
      common_test_list(),&
      time_test_list(),&
      cable_netcdf_test_list()&
    ])

  END FUNCTION tests

END MODULE mock_tests

PROGRAM test_mock
  
  USE fortuno_interface_m, ONLY: EXECUTE_CMD_APP
  USE mock_tests, ONLY: tests

  IMPLICIT NONE

  CALL EXECUTE_CMD_APP(tests())

END PROGRAM test_mock
