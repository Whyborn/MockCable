# Mock CABLE

A little test program used for developing some technical routines that will hopefully be used in CABLE in the future. Currently used for testing the new meteorology I/O routines.

## How it works

Each process is given a ```ProcessDomain```, a derived type with attributes:

* ```ProcessDomainStart```: a length 2 array describing the starting indices for the process on the gridded domain
* ```ProcessDomainSize``` : a length 2 array describing the size of the domain
* ```LongitudeIDs/LatitudeIDs```: a pair of 1D arrays mapping points on the 2D process domain to the 1D vector that would be used within CABLE.
* ```Longitudes/Latitudes```: a pair of 1D arrays describing the absolute latitudes and longitudes of each point, for nearest neighbour/interpolation searches.

For example, with ```ProcessDomainStart = [32, 24]``` and ```ProcessDomainSize = [16, 12]```, the given process would be assigned indices ```[32:48, 24:36]``` from the global domain. The ```LongitudeIDs/LatitudeIDs``` are indices _on the process's domain_, so they would have range ```1 <= LonID <= 16, 1 <= LatID <= 12``` rather than ```32 <= LonID <= 48, 24 <= LatID <= 36```. The domains are built by inspecting a given landmask and the size of the MPI world.

### Inputs

Each time series inputs are assigned ```DatasetReader```, which contains information about the files which will describe a given variable and the process domain information. The ```DatasetReader``` has an attached ```DataStorage```, which has size equal to ```ProcessDomainSize```. When a specific record of data is requested, the ```DataStorage``` is passed as the container and the ```ProcessDomainStart``` as ```COUNT``` to the ```NF90_OPEN``` routine. The 2D array of input data is then mapped to 1D using the ```LongitudeIDs/LatitudeIDs```, and then passed to science routines. 

### Outputs

Outputs are handled differently. The ```output_module``` creates a series of wrappers around the core NetCDF routines. The ```output_module``` is given information about the ```ProcessDomain``` and MPI configuration on initialisation. Rather than calling ```NF90_OPEN```, external routines would call ```initialise_output_file```, which then handles the MPI configuration invisibly. When writing data to the output files, the external routines just call ```put_variable_data/put_record``` for spatial data, and the arrays are written to the correct chunks on the grid using the ```ProcessDomainStart```, even for higher dimensional data, by inspecting the dimensions and setting the correct indices of the ```START``` argument.
