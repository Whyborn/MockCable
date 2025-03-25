#!/bin/bash
. /etc/bashrc
module load cmake/3.24.2 netcdf/4.8.0p intel-compiler-llvm/2025.0.4 openmpi/4.1.3

prepend_path PKG_CONFIG_PATH "${NETCDF_BASE}/lib/Intel/pkgconfig"

if module is-loaded openmpi; then
    # This is required so that the openmpi MPI libraries are discoverable
    # via CMake's `find_package` mechanism:
    prepend_path CMAKE_PREFIX_PATH "${OPENMPI_BASE}/include/Intel"
fi

cmake -S . -B build -DMOCK_CABLE_MPI="ON" -DCMAKE_Fortran_COMPILER=mpif90
cmake --build build
cmake --install build --prefix .
