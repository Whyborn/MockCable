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

set(MOCK_CABLE_INTEL_Fortran_FLAGS -g -traceback)
add_compile_options(${MOCK_CABLE_INTEL_Fortran_FLAGS})
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

if(MOCK_CABLE_MPI)
    target_compile_definitions(common PRIVATE __MPI__)
    target_link_libraries(common PRIVATE MPI::MPI_Fortran)
endif()

add_executable(
    test-common
    test/test_common.F90
)

add_executable(
    test-output
    test/test_output.F90
)

add_executable(
    test-datasetreader
    test/test_datasetreader.F90
)

add_executable(
    mock-cable
    src/mock_cable.F90
)

add_executable(
    simple4by4
    test/simple4by4.F90
)

target_link_libraries(test-common PRIVATE common PkgConfig::NETCDF)
target_link_libraries(test-output PRIVATE common PkgConfig::NETCDF)
target_link_libraries(test-datasetreader PRIVATE common PkgConfig::NETCDF)
target_link_libraries(mock-cable PRIVATE common PkgConfig::NETCDF)
target_link_libraries(simple4by4 PRIVATE common PkgConfig::NETCDF)

if(MOCK_CABLE_MPI)
    target_link_libraries(test-common PRIVATE MPI::MPI_Fortran)
    target_link_libraries(test-output PRIVATE MPI::MPI_Fortran)
    target_link_libraries(test-datasetreader PRIVATE MPI::MPI_Fortran)
    target_link_libraries(mock-cable PRIVATE MPI::MPI_Fortran)
endif()

install(TARGETS test-common RUNTIME)
install(TARGETS test-output RUNTIME)
install(TARGETS test-datasetreader RUNTIME)
install(TARGETS mock-cable RUNTIME)
install(TARGETS simple4by4 RUNTIME)
