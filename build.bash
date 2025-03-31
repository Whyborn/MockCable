#!/bin/bash

show_help() {
   cat << EOF
Options:
   -m, --mpi      Compile with MPI
   -h, --help     Show help
EOF
}

. /etc/bashrc

cmake_args=(-DMOCK_CABLE_MPI="OFF")
mpi=0

# Argument parsing adapted and stolen from http://mywiki.wooledge.org/BashFAQ/035#Complex_nonstandard_add-on_utilities
while [ ${#} -gt 0 ]; do
    case ${1} in
      -m|--mpi)
         cmake_args+=(-DMOCK_CABLE_MPI="ON")
         mpi=1
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

module load cmake/3.24.2 intel-compiler-llvm/2025.0.4 

if [ ${mpi} -eq 1 ]; then
   module load openmpi/4.1.3 netcdf/4.8.0p
   cmake_args+=(-DCMAKE_Fortran_COMPILER=mpifort)
else
   module load netcdf/4.8.0
fi
 
prepend_path PKG_CONFIG_PATH "${NETCDF_BASE}/lib/Intel/pkgconfig"

if module is-loaded openmpi; then
    # This is required so that the openmpi MPI libraries are discoverable
    # via CMake's `find_package` mechanism:
    prepend_path CMAKE_PREFIX_PATH "${OPENMPI_BASE}/include/Intel"
fi


cmake -S . -B build "${cmake_args[@]}"
cmake --build build
cmake --install build --prefix .
