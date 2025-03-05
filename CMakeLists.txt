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

add_library(
    common
    src/common_module.F90
    src/datasetreader_module.F90
    src/meteorology_module.F90
    src/time_module.F90
    src/mpi_module.F90
    src/domain_module.F90
    src/output_module.F90
)

if(MOCK_CABLE_MPI)
    target_compile_definitions(common PRIVATE __MPI__)
endif()

add_executable(
    mock-cable
    src/mock_cable.F90
)
target_link_libraries(mock-cable PRIVATE common PkgConfig::NETCDF MPI::MPI_Fortran)
install(TARGETS mock-cable RUNTIME)

add_executable(
    test-common
    test/test_common.F90
)
target_link_libraries(test-common PRIVATE common PkgConfig::NETCDF MPI::MPI_Fortran)
install(TARGETS test-common RUNTIME)
