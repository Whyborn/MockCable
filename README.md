# Mock CABLE

A little test program used for developing some technical routines that will hopefully be used in CABLE in the future. Currently used for testing the new meteorology I/O routines.

## How it works

The MPI is configured a ```ProcessDomain```, which contains information about:

* The local process's starting indices on the gridded domain
* The local process's domain size
* The 2D to 1D mapping for the process's respective chunk.

This way we can pass the starting indices and domain size to the ```START``` and ```COUNT``` arguments of the input routines, and map to (and from) 2D to 1D. To assist with output, there is also a ```GlobalDomain```, which contains the global land grid and longitude/latitude axes. Then when we initialise dimensions for new output files, a single process has all the dimension information and can initialise the axes.

The initialisation process is:

1. Read in the landmask from NetCDF on every process. Chunk up the landmask along the longitude axis by dividing the axis length by the MPI size, and bind the starting indices and chunk sizes to the ```ProcessDomain``` and the global information to the ```GlobalDomain```.
2. Initialise the ```DatasetReader``` which index the NetCDF datasets which span multiple files. Each reader is told where the process's starting index is, and has a storage container of size specified by the chunk sizes.

Then when the ```DatasetReader``` is read, it places the matrix of data relevant to the process in the attached storage container, to be mapped to 1D vectors in a higher level routine.
