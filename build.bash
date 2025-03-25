#!/bin/bash

show_help() {
   cat << EOF
Options:
   -m, --mpi      Compile with MPI
   -h, --help     Show help
EOF
}

. /etc/bashrc
module load cmake/3.24.2 netcdf/4.8.0p intel-compiler-llvm/2025.0.4 openmpi/4.1.3

cmake_args=(-DCMAKE_Fortran_COMPILER=mpif90 -DMOCK_CABLE_MPI="OFF")

# Argument parsing adapted and stolen from http://mywiki.wooledge.org/BashFAQ/035#Complex_nonstandard_add-on_utilities
while [ ${#} -gt 0 ]; do
    case ${1} in
      -m|--mpi)
         cmake_args+=(-DMOCK_CABLE_MPI="ON")
         ;;
      -h|--help)
         show_help
         exit
         ;;
      ?*)
         cmake_args+=("${1}")
   esac
   shift
done

prepend_path PKG_CONFIG_PATH "${NETCDF_BASE}/lib/Intel/pkgconfig"

if module is-loaded openmpi; then
    # This is required so that the openmpi MPI libraries are discoverable
    # via CMake's `find_package` mechanism:
    prepend_path CMAKE_PREFIX_PATH "${OPENMPI_BASE}/include/Intel"
fi


cmake -S . -B build -DMOCK_CABLE_MPI="ON" -DCMAKE_Fortran_COMPILER=mpif90
cmake --build build
cmake --install build --prefix .
