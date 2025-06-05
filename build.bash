#!/bin/bash

show_help() {
   cat << EOF
Options:
   -m, --mpi      Compile with MPI
   -p, --pio      Compile with PIO
   -h, --help     Show help
EOF
}

. /etc/bashrc

cmake_args=(-DMOCK_CABLE_MPI="OFF" -DMOCK_CABLE_PIO="OFF")
mpi=0
pio=0

# Argument parsing adapted and stolen from http://mywiki.wooledge.org/BashFAQ/035#Complex_nonstandard_add-on_utilities
while [ ${#} -gt 0 ]; do
    case ${1} in
      -m|--mpi)
         cmake_args+=(-DMOCK_CABLE_MPI="ON")
         mpi=1
         ;;
      -p|--pio)
         cmake_args+=(-DMOCK_CABLE_PIO="ON")
         pio=1
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

module purge
module load cmake/3.24.2 intel-compiler-llvm/2025.0.4 

if [ ${mpi} -eq 1 ]; then
   module load openmpi/4.1.7 netcdf/4.8.0p
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

if [ ${pio} -eq 1 ]; then
    prepend_path CMAKE_PREFIX_PATH "/g/data/tm70/sb8430/parallelio_install"
    prepend_path LD_RUN_PATH "/g/data/tm70/sb8430/parallelio_install/lib"
fi

cmake -S . -B build "${cmake_args[@]}"
cmake --build build
cmake --install build --prefix .
