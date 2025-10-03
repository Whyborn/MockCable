MODULE datasetreader_module

USE mpi_module, ONLY: mpi_info_hints_t, mpi_info_set_hints
USE iso_fortran_env, ONLY: ERROR_UNIT, OUTPUT_UNIT
USE time_module, ONLY: days_in_month, is_leapyear, leap_day,&
                       read_time_string, intervals_since
USE common_module, ONLY: sort, get_dimid, handle_ncstat, approx_equal,&
                         find_largest_element_less_than_sorted
use land_decomp_mod, only: land_decomp_t, io_decomp_grid
use cable_netcdf_mod

IMPLICIT NONE

TYPE DatasetReader
  !*## Purpose
  !
  ! The dataset reader is a tool used to handle the ingestion of data from
  ! NetCDF files. The data structure facilitates transparent reading of data
  ! from a given time.

  ! An 'active' flag
  LOGICAL :: HasData = .FALSE.

  ! List of files in the dataset, and their indices
  CHARACTER(LEN=250), DIMENSION(:), ALLOCATABLE :: DatasetFiles
  INTEGER, DIMENSION(:), ALLOCATABLE :: IndexRange

  ! Start point for the dataset
  INTEGER :: StartYear

  ! Size of the timestep- we will compute this by inspecting the time dimension
  ! of the dataset, and check it against the request time step to ensure they
  ! match. The core code refers to this as a REAL, but it's more likely we'll
  ! want it as an integer
  INTEGER :: TimestepSize

  ! NetCDF variable name
  CHARACTER(LEN=10), ALLOCATABLE :: VarName

  ! ID for the current file and variable
  INTEGER :: CurrentFileIndex

  class(cable_netcdf_file_t), allocatable :: file

  class(cable_netcdf_decomp_t), allocatable :: decomp

END TYPE DatasetReader

type(mpi_info_hints_t) :: mpi_info_hints_read

CONTAINS

FUNCTION initialise_datasetreader_at_timestep(TextFileList, VarName,&
    StepSize, land_decomp) RESULT(NewReader)
  !*## Purpose
  !
  ! Initialise a new datasetreader, which contains data on the same time
  ! interval as the simulation.
  !
  !## Method
  !
  ! Take a list of files contained in the passed text file,
  ! and use them to form a reader that transparently indexes the dataset in the
  ! correct location when called with a year and timestep within said year.

  CHARACTER(LEN=*), INTENT(IN) :: TextFileList
  CHARACTER(LEN=*), INTENT(IN) :: VarName
  REAL, INTENT(IN) :: StepSize
  type(land_decomp_t) :: land_decomp

  TYPE(DatasetReader) :: NewReader

  ! Array to hold all the file names contained in the specified file
  CHARACTER(LEN=250), DIMENSION(:), ALLOCATABLE :: ListOfFiles

  ! Get the list of files in the dataset from the text file
  ListOfFiles = get_files_from_text(TextFileList)

  ! Now sort them by their starting data and attach it to the Reader
  NewReader%DatasetFiles = sort_by_start_date(ListOfFiles)

  ! To correctly retrieve indices later, we need to know at what year the
  ! dataset begins.
  CALL identify_start_year(NewReader)

  ! Now we have a sorted list, it's easier to assign indices to each
  ! of the files in the dataset
  NewReader%IndexRange = assign_file_indices(NewReader%DatasetFiles)

  ! Attach the timestep and variables
  NewReader%VarName = VarName
  NewReader%TimestepSize = INT(StepSize)

  ! To avoid any first call annoyances, we will set the first file in the
  ! dataset as the 'active' file and retrieve the desired variable ID.
  CALL mark_as_active(NewReader)

  NewReader%decomp = io_decomp_grid(land_decomp, [land_decomp%land_count], CABLE_NETCDF_FLOAT)

END FUNCTION initialise_datasetreader_at_timestep

FUNCTION get_files_from_text(InputFileList) RESULT(ListOfFiles)
  !*## Purpose
  !
  ! Extract the list of files from the text file and return as an array of
  ! characters.
  !
  !## Method
  !
  ! Count the number of lines in the file, then allocate an array of that size
  ! and write the list of files to the array.

  CHARACTER(LEN=*), INTENT(IN) :: InputFileList

  CHARACTER(LEN=250), DIMENSION(:), ALLOCATABLE :: ListOfFiles

  ! A temporary array of very large size to intially write all the file names to
  ! to save us having to read the file twice
  CHARACTER(LEN=250), DIMENSION(1000) :: TempListOfFiles
  INTEGER :: LineCounter, ios, FileUnit
  CHARACTER(LEN=100) :: ioMessage

  ! The user has given a text file containing the list of files to include
  OPEN(NEWUNIT=FileUnit, FILE=InputFileList, IOSTAT=ios)

  ! We just use 1000 as a 'sufficiently larger number' here- it's the upper
  ! limit to the number of files we could possibly have in a dataset reader
  ReadFilenames: DO LineCounter = 1, 1000
    ! Read a line from the file into the temporary array
    READ(FileUnit, '(A)', IOSTAT=ios, IOMSG=ioMessage)&
      TempListOfFiles(LineCounter)

    IF (ios < 0) THEN
      ! Read reached EOF
      EXIT ReadFilenames
    ELSEIF (ios /= 0) THEN
      ! Something else went wrong
      WRITE(ERROR_UNIT,'(A)') 'Error reading list of files in DatasetReader. '&
        //'Error message is:'//ioMessage
      STOP ios
    END IF
  END DO ReadFileNames

  ! Finished reading the file- decrement LineCounter by 1, since we added 1 on
  ! the EOF read
  LineCounter = LineCounter - 1

  IF (LineCounter == 0) THEN
    WRITE(ERROR_UNIT,'(A)') 'No NetCDF files were found in the text file.'

    STOP 1
  END IF

  ! Now we can allocate the correct amount of memory for the files, and port
  ! from the temporary array to the returned array
  ALLOCATE(ListOfFiles(LineCounter))
  ListOfFiles(:) = TempListOfFiles(1:LineCounter)

END FUNCTION get_files_from_text
  
FUNCTION sort_by_start_date(ListOfFiles) RESULT(SortedFiles)
  !*## Purpose
  !
  ! Sort the NetCDF files contained in the passed list of files chronologically
  ! and return the sorted list.
  !
  !## Method
  !
  ! Use the first value from the time array to determine the chronological order
  ! of the netCDF datasets.

  CHARACTER(LEN=*), DIMENSION(:), ALLOCATABLE, INTENT(IN) :: ListOfFiles

  CHARACTER(LEN=256), DIMENSION(:), ALLOCATABLE :: SortedFiles

  ! We want to store the first time value from the time variable of each file
  INTEGER, DIMENSION(:), ALLOCATABLE :: TimeValues

  ! Iterators
  INTEGER :: FileCounter

  ! Indexer used for sorting
  INTEGER, DIMENSION(:), ALLOCATABLE :: Indexer

  class(cable_netcdf_file_t), allocatable :: file

  ! Allocate the arrays based off the number of files
  ALLOCATE(SortedFiles(SIZE(ListOfFiles)), TimeValues(SIZE(ListOfFiles)))

  ! Retrieve the time indices for each file
  GetStartTimes: DO FileCounter = 1, SIZE(SortedFiles)
    ! Open and inspect the netCDF file
    file = cable_netcdf_open_file(ListOfFiles(FileCounter))
    call file%get_var("time", TimeValues(FileCounter), count=[1])
    call file%close()
  END DO GetStartTimes

  ! Sort the start times, and use the indexer to then sort the files
  CALL sort(TimeValues, Indexer)
  SortFiles: DO FileCounter = 1, SIZE(Indexer)
    SortedFiles(FileCounter) = ListOfFiles(Indexer(FileCounter))
  END DO SortFiles

END FUNCTION sort_by_start_date

SUBROUTINE identify_start_year(Reader)
  !*## Purpose
  !
  ! Identify the first year of the dataset to allow correct indexing during
  ! reads.
  !
  !## Method
  !
  ! Inspect the units attribute of the time variable. This should contain a
  ! string describing the time axis reference. From this reference and the first
  ! time value, we can determine the first year.

  TYPE(DatasetReader), INTENT(INOUT) :: Reader

  ! Integers used for status, IDs and time value
  INTEGER :: ok, tID, StartTime, RefYear, SecondsToYears

  ! String to hold the units attribute
  CHARACTER(LEN=33) :: TimeUnits

  class(cable_netcdf_file_t), allocatable :: file

  file = cable_netcdf_open_file(trim(Reader%DatasetFiles(1)))
  
  call file%get_var("time", StartTime)
  call file%get_att("time", "units", TimeUnits)
  call file%close()

  ! Check that the units are valid
  IF (TimeUnits(1:13) /= 'seconds since') THEN
    WRITE(ERROR_UNIT,*) 'Invalid units for the time attribute.'
    STOP 5
  END IF

  ! We have made the stipulation that the time units MUST be 
  ! 'seconds since YYYY-MM-DD HH:MM:SS', so we can trivially extract the year
  READ(TimeUnits(15:18), *) RefYear
  
  ! Convert the StartTime, which is in seconds, to a number of years (with the
  ! start year determined from the units attribute as the reference year)
  ! For simplicity, we should be able to round StartTime / SecondsInNonLeapYear
  ! and an accurate count for the number of years. We'd need to have 168 leap
  ! years in our calculation interval for this to return the wrong year
  SecondsToYears = NINT(REAL(StartTime) / (3600 * 24 * 365))
  
  ! Finally we can set the start year for the data in our dataset
  Reader%StartYear = RefYear + SecondsToYears

END SUBROUTINE identify_start_year

FUNCTION assign_file_indices(DatasetFiles) RESULT(IndexRange)
  !*## Purpose
  !
  ! Assign the indices associated with each file attached to the DatasetReader.
  !
  !## Method
  !
  ! Iterate through each of the netCDF files attached to the reader, inspect
  ! the size of the time dimension and use this information to assign index
  ! ranges for each file. The index indicates the first index for the given
  ! file.

  CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: DatasetFiles

  INTEGER, DIMENSION(:), ALLOCATABLE :: IndexRange

  ! Iterators, netCDF IDs and dimension sizes
  INTEGER :: FileCounter, DimLength, TimeIndex

  class(cable_netcdf_file_t), allocatable :: file

  ! Allocate memory for the indices
  ALLOCATE(IndexRange(SIZE(DatasetFiles)))

  ! Set the TimeIndex to 1
  TimeIndex = 1

  ! The first index is set to 1
  IndexRange(1) = TimeIndex

  CountFiles: DO FileCounter = 1, (SIZE(DatasetFiles)-1)
    file = cable_netcdf_open_file(DatasetFiles(FileCounter))
    call file%inq_dim_len("time", DimLength)
    call file%close()

    TimeIndex = TimeIndex + DimLength

    IndexRange(FileCounter+1) = TimeIndex
  END DO CountFiles

END FUNCTION assign_file_indices

SUBROUTINE verify_validity(Reader)
  !*## Purpose
  !
  ! Verify that the data attached to the dataset is valid for reading.
  !
  !## Method
  !
  ! Run through the requirements for a dataset to be valid. There requirements
  ! are:
  !
  ! * The time intervals are the same between each file
  ! * Each file has a recognised variable name

  TYPE(DatasetReader), INTENT(IN) :: Reader

  ! Iterator for the files and NetCDF ids
  INTEGER :: FileIter, ncID, tID, DimLength, ok

  class(cable_netcdf_file_t), allocatable :: file

  ! Variables to store the end time of a dataset/start time of next
  ! Initialise interval to some absurd number
  REAL :: EndTime, NextStartTime, Interval

  ! Get the first end time manually, since we have nothing to check against
  file = cable_netcdf_open_file(Reader%DatasetFiles(1))
  call file%inq_dim_len("time", DimLength)
  call file%get_var("time", EndTime, start=[DimLength])
  call file%close()

  ! Now we can iterate through the remainder of the files
  DO FileIter = 2, SIZE(Reader%DatasetFiles)
    file = cable_netcdf_open_file(Reader%DatasetFiles(FileIter))
    call file%inq_dim_len("time", DimLength)

    ! Get the first time from the new file
    call file%get_var("time", NextStartTime, start=[1])

    ! Only check the interval on the second iteration, as we have no interval
    ! to compare against on the first
    IF (FileIter >= 3) THEN
      IF (.NOT. approx_equal(NextStartTime - EndTime, Interval)) THEN
        WRITE(ERROR_UNIT,*) 'The time intervals between each file are not '//&
          'the same.'
        STOP 5
      END IF
    ELSE
      Interval = NextStartTime - EndTime
    END IF

    ! Set the new end time
    call file%get_var("time", EndTime, start=[DimLength])

    call file%close()
  END DO

END SUBROUTINE verify_validity
    
SUBROUTINE mark_as_active(Reader)
  !*## Purpose
  !
  ! Prepare the dataset reader for use by opening an io stream at the first file
  ! in the dataset. Mark the dataset as active so further read operations
  ! proceed as usual.
  !
  !## Method
  !
  ! Open an iostream pointing to the first file in the dataset and save the
  ! associated ncID to the dataset. Inspect this file for the desired variable
  ! and record the associated ID. Mark the dataset as active so will be read
  ! during iterative reads.

  TYPE(DatasetReader), INTENT(INOUT) :: Reader

  ! Status checkers
  INTEGER :: ok

  CALL open_new_file_in_reader(Reader, 1)
  Reader%HasData = .TRUE.

END SUBROUTINE mark_as_active

SUBROUTINE open_new_file_in_reader(Reader, FileIndex)
  !*## Purpose
  !
  ! Set the file at FileIndex to be the currently open file.
  !
  !## Method
  !
  ! Use NetCDF routines to acquire a new ID associated with the file at the
  ! given index.

  INTEGER, INTENT(IN) :: FileIndex

  TYPE(DatasetReader), INTENT(INOUT) :: Reader

  ! Close the old reader
  IF (allocated(Reader%file)) THEN
    call Reader%file%close()
  END IF

  Reader%file = cable_netcdf_open_file(Reader%DatasetFiles(FileIndex))

  Reader%CurrentFileIndex = FileIndex

END SUBROUTINE open_new_file_in_reader

SUBROUTINE get_data(Reader, Year, TimeIndex, values)
  !*## Purpose
  !
  ! Retrieve the data for a specified year and time step within that year.
  !
  !## Method
  !
  ! Use the passed year and timestep to determine the number of records between
  ! the start of the dataset and the current day. Use this index to retrieve
  ! the correct record from the dataset.

  INTEGER, INTENT(IN) :: Year, TimeIndex

  TYPE(DatasetReader), INTENT(INOUT) :: Reader

  REAL, DIMENSION(:) :: values

  ! Iterator, status checker and index in relevant file
  INTEGER :: FileIndex, ok, IndexInDataset, IndexInFile

  ! Bracketing tools
  INTEGER :: LowerBound, UpperBound, Middle

  ! Make sure the reader we've tried to index actually has data attached to it
  IF (.NOT. Reader%HasData) THEN
    WRITE(ERROR_UNIT,*) 'The DatasetReader does not have any data '//&
      'attached to it.'
    STOP 5
  END IF

  ! Count how many intervals (timesteps) between the start year of the dataset,
  ! and the desired step
  IndexInDataset = intervals_since(Reader%StartYear, Reader%TimeStepSize,&
    Year, TimeIndex)

  ! Determine the file, and index in the given file, to read from
  CALL select_file(Reader, IndexInDataset, FileIndex, IndexInFile)

  ! Is it the file that's currently open? If not, open a new file
  IF (FileIndex /= Reader%CurrentFileIndex) THEN
    CALL open_new_file_in_reader(Reader, FileIndex)
  END IF

  call Reader%file%read_darray(Reader%VarName, values, Reader%decomp, frame=IndexInFile)

END SUBROUTINE get_data

SUBROUTINE select_file(Reader, IndexInDataset, FileIndex, IndexInFile)
  !*## Purpose
  !
  ! Determine which file and the index in said file to retrieve data from.
  !
  !## Method
  !
  ! Use a binary search across the array of index ranges, stored with the
  ! reader, to determine which file to read. Once this is known, the index
  ! in the dataset can be determined.
  TYPE(DatasetReader), INTENT(INOUT) :: Reader
  INTEGER, INTENT(IN) :: IndexInDataset
  INTEGER, INTENT(OUT) :: FileIndex
  INTEGER, INTENT(OUT) :: IndexInFile

  ! Throw an error if the FileIndex == 0, since it means something went wrong

  FileIndex = find_largest_element_less_than_sorted(Reader%IndexRange,&
    IndexInDataset)

  IF (FileIndex == 0) THEN
    WRITE(ERROR_UNIT,*) 'Something went wrong when indexing the dataset.'
    STOP 5
  END IF

  IndexInFile = IndexInDataset - Reader%IndexRange(FileIndex) + 1
END SUBROUTINE select_file

SUBROUTINE close_reader(Reader)
  !*## Purpose
  !
  ! Close down the reader at end of use

  TYPE(DatasetReader), INTENT(INOUT) :: Reader

  call Reader%file%close()

END SUBROUTINE close_reader

END MODULE datasetreader_module
