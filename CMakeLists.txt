# Set minimum CMake version to latest version on Gadi:
cmake_minimum_required(VERSION 3.24.2)

project(
    MOCK_CABLE
    LANGUAGES Fortran
)

option(MOCK_CABLE_MPI "Build the MPI executable" OFF)

# third party libs
if(MOCK_CABLE_MPI)
    find_package(MPI REQUIRED COMPONENTS Fortran)
endif()
find_package(PkgConfig REQUIRED)
pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET "netcdf-fortran")

include(FetchContent)
if (MOCK_CABLE_MPI)
    option(FORTUNO_WITH_MPI "Fortuno: whether to build the MPI interface" ON)
    FetchContent_Declare(
        FortunoMPI
        GIT_REPOSITORY https://github.com/fortuno-repos/fortuno
        GIT_TAG main
    )
    FetchContent_MakeAvailable(FortunoMPI)
    set(fortuno_libs Fortuno::fortuno_mpi)
else ()
    option(FORTUNO_WITH_MPI "Fortuno: whether to build the MPI interface" OFF)
    FetchContent_Declare(Fortuno
        GIT_REPOSITORY https://github.com/fortuno-repos/fortuno
        GIT_TAG main
        FIND_PACKAGE_ARGS CONFIG
    )
    FetchContent_MakeAvailable(Fortuno)
    set(fortuno_libs Fortuno::fortuno_serial)
endif ()
enable_testing()

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(MOCK_CABLE_INTEL_Fortran_FLAGS -g -traceback)
    add_compile_options(${MOCK_CABLE_INTEL_Fortran_FLAGS})
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
endif()

add_library(
    common
    src/common_module.F90
    src/datasetreader_module.F90
    src/meteorology_module.F90
    src/time_module.F90
    src/mpi_module.F90
    src/domain_module.F90
    src/output_module.F90
    src/mpi_serial_stub_module.F90
    src/partition.F90
)
target_link_libraries(common PUBLIC PkgConfig::NETCDF $<$<BOOL:${MOCK_CABLE_MPI}>:MPI::MPI_Fortran>)

if(MOCK_CABLE_MPI)
    target_compile_definitions(common PRIVATE __MPI__)
endif()

#add_executable(
    #test-common
    #test/test_common.F90
#)
add_executable(
    mock-tests
    test/mock_tests.F90
    test/test_common.F90
)
if (MOCK_CABLE_MPI)
  target_sources(mock-tests PRIVATE test/fortuno_interface_mpi.f90)
else()
  target_sources(mock-tests PRIVATE test/fortuno_interface_serial.f90)
endif()


# add_executable(
#     test-output
#     test/test_output.F90
# )

# add_executable(
#     test-datasetreader
#     test/test_datasetreader.F90
# )

add_executable(
    mock-cable
    src/mock_cable.F90
)

add_executable(
    simple4by4
    test/simple4by4.F90
)

#target_link_libraries(test-common PRIVATE common PkgConfig::NETCDF)
target_link_libraries(mock-tests PRIVATE common ${fortuno_libs})
#target_link_libraries(test-output PRIVATE common)
#target_link_libraries(test-datasetreader PRIVATE common)
target_link_libraries(mock-cable PRIVATE common)
target_link_libraries(simple4by4 PRIVATE common)

#target_link_libraries(mock-tests PRIVATE common ${fortuno_libs} $<$<BOOL:${MPI}>:MPI::MPI_Fortran> PkgConfig::NETCDF)

#install(TARGETS test-common RUNTIME)
install(TARGETS mock-tests RUNTIME)
#install(TARGETS test-common RUNTIME)
#install(TARGETS test-output RUNTIME)
#install(TARGETS test-datasetreader RUNTIME)
install(TARGETS mock-cable RUNTIME)
install(TARGETS simple4by4 RUNTIME)

# Register tests
foreach (test IN ITEMS
    "common/test_sort"
    "common/test_approx_equal"
    )
    if(MOCK_CABLE_MPI)
        add_test(NAME ${test}
            COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 1 $<TARGET_FILE:mock-tests> ${test}
        )
        set_tests_properties(${test} PROPERTIES PROCESSORS 1)
    else()
        add_test(NAME ${test}
            COMMAND $<TARGET_FILE:mock-tests> ${test}
        )
    endif()
endforeach ()
