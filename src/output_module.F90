MODULE output_module

  USE iso_fortran_env, ONLY: ERROR_UNIT
#ifdef __MPI__
  USE mpi_f08, ONLY: MPI_INFO_NULL
#endif
  USE netcdf
  USE common_module, ONLY: handle_ncstat
  USE mpi_module, ONLY: mpi_grp_t
  USE domain_module, ONLY: ProcessDomain, GlobalDomain

  IMPLICIT NONE

  PRIVATE :: TemporalAggregation, OnTimestep, Daily, Monthly, Yearly
  ENUM, BIND(C)
    ENUMERATOR :: TemporalAggregation = 0
    ENUMERATOR :: OnTimestep = 1
    ENUMERATOR :: Daily = 2
    ENUMERATOR :: Monthly = 3
    ENUMERATOR :: Yearly = 4
  END ENUM
    
  TYPE NCFile
    !*## Purpose
    !
    ! A derived type used to assist in output routines
    INTEGER :: FileID

    ! Dimensions
    CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: DimNames
    INTEGER, DIMENSION(:), ALLOCATABLE :: DimIDs
    INTEGER, DIMENSION(:), ALLOCATABLE :: DimLengths

    ! Handling for unlimited dimensions
    LOGICAL :: HasUnlimitedDimension
    INTEGER :: UnlimitedDimensionLength = 0

    ! What type is temporal aggregation to use
    INTEGER(KIND(TemporalAggregation)) :: AggregationType = 0

    ! Variables
    TYPE(NCVariable), DIMENSION(:), ALLOCATABLE :: Variables

  END TYPE NCFile

  TYPE NCVariable
    !*## Purpose
    !
    ! A derived type used to assist in output routines
    INTEGER :: VarID
    CHARACTER(LEN=50) :: VarName

    ! Dimension names and IDs
    CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: DimNames
    INTEGER, DIMENSION(:), ALLOCATABLE :: DimIDs

    ! Aggregation counter and trigger
    INTEGER :: AggCounter = 0, Trigger

  END TYPE NCVariable

  TYPE, EXTENDS(NCVariable) :: RealNCVariable
    !*## Purpose
    !
    ! A real instance of an NCVariable

    REAL, DIMENSION(:,:), ALLOCATABLE :: AccumData
  END TYPE RealNCVariable

  TYPE, EXTENDS(NCVariable) :: IntNCVariable
    !*## Purpose
    !
    ! A integer instance of an NCVariable

    INTEGER, DIMENSION(:,:), ALLOCATABLE :: AccumData
  END TYPE IntNCVariable

  INTERFACE initialise_output_file
    MODULE PROCEDURE initialise_output_file_by_name
    MODULE PROCEDURE initialise_output_file_with_dimensions
  END INTERFACE initialise_output_file

  INTERFACE def_variables
    MODULE PROCEDURE def_variables_multiple_dims
    MODULE PROCEDURE def_variables_single_dim
    MODULE PROCEDURE def_variable_multiple_dims
    MODULE PROCEDURE def_variable_single_dim
  END INTERFACE def_variables

  INTERFACE put_variable_data
    MODULE PROCEDURE put_variable_data_rank2
    MODULE PROCEDURE put_variable_data_rank3
  END INTERFACE put_variable_data

  ! This remains an interface because we will need higher rank handling
  ! in future.
  INTERFACE put_record
    MODULE PROCEDURE put_record_rank2
  END INTERFACE put_record

  INTERFACE write_to_record
    MODULE PROCEDURE write_to_record_rank2
  END INTERFACE write_to_record

  ! Store information about the MPI configuration
  TYPE(ProcessDomain), PRIVATE :: ProcDomain
  TYPE(mpi_grp_t), PRIVATE :: mpi_grp
  
  ! The default fill values for each type
  REAL :: DefaultFloatFillVal = NF90_FILL_REAL
  INTEGER :: DefaultIntFillVal = NF90_FILL_INT

CONTAINS

  SUBROUTINE initialise_output_module(ProcDomainIn, mpi_grp_in)
    !*## Purpose
    !
    ! Set up the output module for future writing
    !
    !## Method
    !
    ! Bind local copies of the mpi configuration and local domain.

    TYPE(ProcessDomain), INTENT(INOUT) :: ProcDomainIn
    TYPE(mpi_grp_t), INTENT(INOUT) :: mpi_grp_in

    ProcDomain = ProcDomainIn
    mpi_grp = mpi_grp_in

  END SUBROUTINE initialise_output_module

  FUNCTION initialise_output_file_by_name(FileName) RESULT(OutFile, AggMethod)
    !*## Purpose
    !
    ! Initialise a new output file with the given name.
    !
    !## Method
    !
    ! Use NetCDF routines to open a file in parallel if necessary.

    CHARACTER(LEN=*) :: FileName, AggMethod
    TYPE(NCFile) :: OutFile

    ! Old mode argument is required for NF90_SET_FILL
    INTEGER :: OldMode

#ifdef __MPI__
    CALL handle_ncstat(NF90_CREATE_PAR(FileName,&
      IOR(NF90_CLOBBER, NF90_NETCDF4), mpi_grp%comm%MPI_Val,&
      MPI_INFO_NULL%MPI_Val, OutFile%FileID))
#else
    CALL handle_ncstat(NF90_CREATE(FileName, NF90_CLOBBER, OutFile%FileID))
#endif
  
    ! Set the aggregation period
    IF (PRESENT(AggMethod)) THEN
      IF (AggMethod == "on_timestep") THEN
        OutFile%AggregationType = OnTimestep
      ELSEIF (AggMethod == "daily") THEN
        OutFile%AggregationType = Daily
      ELSEIF (AggMethod == "monthly") THEN
        OutFile%AggregationType = Monthly
      ELSEIF (AggMethod == "yearly") THEN
        OutFile%AggregationType = Yearly
      ELSE
        WRITE(ERROR_UNIT, '(A)') "Invalid option given for aggregation "//&
          "method in "//FileName
      END IF
    ELSE
      OutFile%AggregationType = Daily
    END IF

    ! Set nofill mode
    CALL handle_ncstat(NF90_SET_FILL(OutFile%FileID, NF90_NOFILL, OldMode))
    
    CALL handle_ncstat(NF90_ENDDEF(OutFile%FileID))

  END FUNCTION initialise_output_file_by_name

  FUNCTION initialise_output_file_with_dimensions(FileName, DimNames,&
    DimLengths, AggMethod) RESULT(OutFile)
    !*## Purpose
    !
    ! Initialise a new output file with the given name.
    !
    !## Method
    !
    ! Use NetCDF routines to open a file in parallel if necessary, and then
    ! assign dimensions to it.

    CHARACTER(LEN=*) :: FileName
    CHARACTER(LEN=*), DIMENSION(:) :: DimNames
    INTEGER, DIMENSION(:) :: DimLengths
    CHARACTER(LEN=*), OPTIONAL :: AggMethod
    TYPE(NCFile) :: OutFile

    ! Check that the aggregation method is valid
    IF (PRESENT(AggregationMethod)) THEN
      initialise_output_file_by_name(FileName, AggregationMethod)
    ELSE
      initialise_output_file_by_name(FileName, "daily")
    END IF

    ! Initialise the file
    OutFile = initialise_output_file_by_name(FileName)

    ! Add dimensions
    CALL put_dimensions(OutFile, DimNames, DimLengths)

  END FUNCTION initialise_output_file_with_dimensions

  SUBROUTINE put_dimensions(OutFile, DimNames, DimLengths)
    !*## Purpose
    !
    ! Add the dimensions with the given lengths to the NetCDF file.
    !
    !## Method
    !
    ! Use NetCDF routines to add dimensions to the NetCDF file and the wrapper.

    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: DimNames
    INTEGER, DIMENSION(:), INTENT(IN) :: DimLengths

    ! Iterator for the dimensions
    INTEGER :: DimIter

    ! Make sure that there are the same number of DimNames and DimLengths
    IF (SIZE(DimNames) /= SIZE(DimLengths)) THEN
      WRITE(ERROR_UNIT,*) "Different number of dimension names and lengths"//&
        " given to add_dimensions."
    END IF

    ! Set the file to definition mode
    CALL handle_ncstat(NF90_REDEF(OutFile%FileID))

    ! Allocate memory for the derived type names
    ALLOCATE(OutFile%DimNames(SIZE(DimNames)))
    ALLOCATE(OutFile%DimIDs(SIZE(DimNames)))
    ALLOCATE(OutFile%DimLengths(SIZE(DimLengths)))

    ! Run through the passed dimensions
    DO DimIter = 1, SIZE(DimNames)
      CALL handle_ncstat(NF90_DEF_DIM(OutFile%FileID, TRIM(DimNames(DimIter)),&
        DimLengths(DimIter), OutFile%DimIDs(DimIter)))

      OutFile%DimNames(DimIter) = DimNames(DimIter)
      OutFile%DimLengths(DimIter) = DimLengths(DimIter)

      IF (DimLengths(DimIter) == NF90_UNLIMITED) THEN
        OutFile%HasUnlimitedDimension = .TRUE.
      END IF
    END DO

    ! Set the file back to write mode
    CALL handle_ncstat(NF90_ENDDEF(OutFile%FileID))

  END SUBROUTINE put_dimensions

  SUBROUTINE def_variables_multiple_dims(OutFile, VarNames, VarDims, DataType)
    !*## Purpose
    !
    ! Add a variable to the NetCDF file.
    !
    !## Method
    !
    ! Use NetCDF routines to add the variable to the NetCDF file.

    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: VarNames
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: VarDims
    INTEGER, INTENT(IN) :: DataType

    ! iterators
    INTEGER :: DimIter, FileDimIter, VarIter

    ! Possibly required temporary variables, if a resize is needed
    TYPE(NCVariable), DIMENSION(:), ALLOCATABLE :: TempVariables

    ! We need to set the start point for this set of variables
    INTEGER :: StartPoint

    ! Store inferred DimIDs
    INTEGER, DIMENSION(:), ALLOCATABLE :: VarDimIDs

    ! Set the file to definition mode
    CALL handle_ncstat(NF90_REDEF(OutFile%FileID))

    ! We will want to redefine the length of the file's variables, if it's
    ! already allocated
    IF (ALLOCATED(OutFile%Variables)) THEN
      ! We've been through the process before, so we need to redefine the array
      ! of variable names and IDs, rather than just allocate
      TempVariables = OutFile%Variables

      ! Set the starting point for this set of variables
      StartPoint = SIZE(OutFile%Variables)

      ! Now reallocate the file's variables, with their new size
      DEALLOCATE(OutFile%Variables)
      ALLOCATE(OutFile%Variables(StartPoint + SIZE(VarNames)))

      ! Reset the already-defined set of variable names and IDs
      OutFile%Variables(1:StartPoint) = TempVariables

    ELSE
      ! No variables have been defined for this file yet- just allocate
      ALLOCATE(OutFile%Variables(SIZE(VarNames)))

      StartPoint = 0
    END IF

    ! First determine the dimensions IDs
    ALLOCATE(VarDimIDs(SIZE(VarDims)))
    VariableDims: DO DimIter = 1, SIZE(VarDims)
      FileDims: DO FileDimIter = 1, SIZE(OutFile%DimNames)
        IF (TRIM(VarDims(DimIter)) == TRIM(OutFile%DimNames(FileDimIter))) THEN
          VarDimIDs(DimIter) = OutFile%DimIDs(FileDimIter)
          EXIT FileDims
        END IF
      END DO FileDims
    END DO VariableDims

    ! Now we know the dimension IDs of each of the desired dimensions
    DO VarIter = 1, SIZE(VarNames)
      CALL handle_ncstat(NF90_DEF_VAR(OutFile%FileID, VarNames(VarIter),&
        DataType, VarDimIDs, OutFile%Variables(StartPoint + VarIter)%VarID))

#ifdef __MPI__
      CALL handle_ncstat(NF90_VAR_PAR_ACCESS(OutFile%FileID,&
        OutFile%Variables(StartPoint + VarIter)%VarID, NF90_COLLECTIVE))
#endif

      ! Set up the NCVariable
      ALLOCATE(OutFile%Variables(StartPoint + VarIter)%DimIDs(SIZE(VarNames)),&
        OutFile%Variables(StartPoint + VarIter)%DimNames(SIZE(VarNames)))

      OutFile%Variables(StartPoint + VarIter)%VarName = VarNames(VarIter)
      OutFile%Variables(StartPoint + VarIter)%DimNames = VarDims
      OutFile%Variables(StartPoint + VarIter)%DimIDs = VarDimIDs

      ! Set the default fill value
      IF (DataType == NF90_FLOAT) THEN
        CALL set_fill_value(OutFile, VarNames(VarIter), DefaultFloatFillVal)
      ELSEIF (DataType == NF90_INT) THEN
        CALL set_fill_value(OutFile, VarNames(VarIter), DefaultIntFillVal)
      END IF

    END DO

    ! Set the file back to write mode
    CALL handle_ncstat(NF90_ENDDEF(OutFile%FileID))

  END SUBROUTINE def_variables_multiple_dims

  SUBROUTINE def_variables_single_dim(OutFile, VarNames, VarDim, DataType)
    !*## Purpose
    !
    ! Add single dimensioned variables to the NCFile.
    !
    !## Method
    !
    ! Invoke the full-featured def_variables_multiple_dims function
    
    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: VarNames
    CHARACTER(LEN=*), INTENT(IN) :: VarDim
    INTEGER, INTENT(IN) :: DataType

    CALL def_variables_multiple_dims(OutFile, VarNames, [VarDim], DataType)

  END SUBROUTINE def_variables_single_dim

  SUBROUTINE def_variable_multiple_dims(OutFile, VarName, VarDims, DataType)
    !*## Purpose
    !
    ! Add a single variable to the NCFile.
    !
    !## Method
    !
    ! Invoke the full-featured def_variables_multiple_dims function
    
    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=*), INTENT(IN) :: VarName
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: VarDims
    INTEGER, INTENT(IN) :: DataType

    CALL def_variables_multiple_dims(OutFile, [VarName], VarDims, DataType)

  END SUBROUTINE def_variable_multiple_dims

  SUBROUTINE def_variable_single_dim(OutFile, VarName, VarDim, DataType)
    !*## Purpose
    !
    ! Add a single dimensioned variable to the NCFile.
    !
    !## Method
    !
    ! Invoke the full-featured def_variables_multiple_dims function

    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=*), INTENT(IN) :: VarName
    CHARACTER(LEN=*), INTENT(IN) :: VarDim
    INTEGER, INTENT(IN) :: DataType

    CALL def_variables_multiple_dims(OutFile, [VarName], [VarDim], DataType)

  END SUBROUTINE def_variable_single_dim

  SUBROUTINE put_dimension_data(OutFile, DimName, SourceData)
    !*## Purpose
    !
    ! Set the data for a dimension variable
    !
    !## Method
    !
    ! Check that the dimension name is also a variable name, then attach the
    ! data to the variable.

    TYPE(NCFile), INTENT(IN) :: OutFile
    CHARACTER(LEN=*), INTENT(IN) :: DimName
    CLASS(*), DIMENSION(:), INTENT(IN) :: SourceData

    ! The target variable we're writing to
    TYPE(NCVariable) :: TargetVariable

    ! Checker for validity of dimension name
    LOGICAL :: IsDimension
    INTEGER :: DimIter

    ! Check that it's a dimension
    IsDimension = .FALSE.
    DO DimIter = 1, SIZE(OutFile%DimNames)
      IF (TRIM(DimName) == TRIM(OutFile%DimNames(DimIter))) THEN
        IsDimension = .TRUE.
        EXIT
      END IF
    END DO

    IF (.NOT. IsDimension) THEN
      WRITE(ERROR_UNIT,*) TRIM(DimName)//" is not a dimension in the file."
    END IF

    ! Assign data to the associated variable
    TargetVariable = get_target_variable(OutFile, DimName)

    SELECT TYPE (SourceData)
    TYPE IS (REAL)
      CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
        SourceData))
    TYPE IS (INTEGER)
      CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
        SourceData))
    END SELECT

  END SUBROUTINE put_dimension_data

  SUBROUTINE extend_unlimited_dimension(OutFile, DimName, DimValue)
    !*## Purpose
    !
    ! Append a value to the unlimited dimension.
    !
    !## Method
    !
    ! Extend the size of the unlimited dimension by 1 with a new value, and
    ! track the new size of the unlimited dimension so we can write to it.
    
    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=*), INTENT(IN) :: DimName
    CLASS(*), INTENT(IN) :: DimValue

    ! Checker to ensure dimension is unlimited
    INTEGER :: DimIter

    ! The target variable we're writing to
    TYPE(NCVariable) :: TargetVariable

    ! Check that the specified dimension is unlimited
    DO DimIter = 1, SIZE(OutFile%DimNames)
      IF (TRIM(OutFile%DimNames(DimIter)) == TRIM(DimName)) THEN
        ! Check that the corresponding dimension is unlimited
        IF (.NOT. OutFile%DimLengths(DimIter) == NF90_UNLIMITED) THEN
          WRITE(ERROR_UNIT,*) "Attempted to append to a dimension that is "//&
            "not unlimited."
        END IF
        EXIT
      END IF
    END DO

    ! Assign data to the associated variable
    TargetVariable = get_target_variable(OutFile, DimName)
    SELECT TYPE (DimValue)
    TYPE IS (REAL)
      CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
        DimValue, START=[OutFile%UnlimitedDimensionLength+1]))
    TYPE IS (INTEGER)
      CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
        DimValue, START=[OutFile%UnlimitedDimensionLength+1]))
    END SELECT

    ! Increment the size of the unlimited dimension
    OutFile%UnlimitedDimensionLength = OutFile%UnlimitedDimensionLength + 1

  END SUBROUTINE extend_unlimited_dimension
      
  SUBROUTINE put_variable_data_rank2(OutFile, VarName, SourceData)
    !*## Purpose
    !
    ! Assign data to a specified variable.
    !
    !## Method
    !
    ! Use NetCDF routines to assign data to a variable. This routine assumes
    ! the entire data store is being assigned- to add a record to a variable
    ! with an unlimited dimension, use add_record.

    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=*), INTENT(IN) :: VarName
    CLASS(*), DIMENSION(:,:), INTENT(IN) :: SourceData

    ! The target variable we're writing to
    TYPE(NCVariable) :: TargetVariable

    TargetVariable = get_target_variable(OutFile, VarName)

    ! Is there any world where a rank 2 array would not be describing a spatial
    ! map? For now, assume not, so we know that the dimensions are lon, lat and
    ! should be chunked up accordingly.
    SELECT TYPE(SourceData)
    TYPE IS (REAL)
      CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
        SourceData, START=ProcDomain%ProcessDomainStart))
    TYPE IS (INTEGER)
      CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
        SourceData, START=ProcDomain%ProcessDomainStart))
    END SELECT

  END SUBROUTINE put_variable_data_rank2

  SUBROUTINE put_variable_data_rank3(OutFile, VarName, SourceData)
    !*## Purpose
    !
    ! Assign data to a specified variable.
    !
    !## Method
    !
    ! Use NetCDF routines to assign data to a variable. This routine assumes
    ! the entire data store is being assigned- to add a record to a variable
    ! with an unlimited dimension, use add_record.

    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=*), INTENT(IN) :: VarName
    CLASS(*), DIMENSION(:,:,:), INTENT(IN) :: SourceData
    
    ! The target variable we're writing to
    TYPE(NCVariable) :: TargetVariable

    ! To assist in chunking up of the variable data
    INTEGER, DIMENSION(3) :: DimStarts
    INTEGER :: DimIter

    TargetVariable = get_target_variable(OutFile, VarName)

    ! We need to check which dimensions are our latitudes/longitudes, so we
    ! know which dimension to chunk up for writing
    DimStarts = [1, 1, 1]
    DO DimIter = 1, 3
      IF (TRIM(TargetVariable%DimNames(DimIter)) == "lon") THEN
        DimStarts(DimIter) = ProcDomain%ProcessDomainStart(1)
      ELSEIF (TRIM(TargetVariable%DimNames(DimIter)) == "lat") THEN
        DimStarts(DimIter) = ProcDomain%ProcessDomainStart(2)
      END IF
    END DO

    ! Now we can write the variable
    SELECT TYPE (SourceData)
    TYPE IS (REAL)
      CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
        SourceData, START=DimStarts))
    TYPE IS (INTEGER)
      CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
        SourceData, START=DimStarts))
    END SELECT

  END SUBROUTINE put_variable_data_rank3

  SUBROUTINE put_record_rank2(OutFile, VarName, SourceData, UnlimitedDim)
    !*## Purpose
    !
    ! Add a record to a NetCDF variable that has an umlimited dimension,
    ! usually time.
    !
    !## Method
    !
    ! Determine which dimension is the umlimited dimension, then extend that
    ! dimension variable by appending the DimValue. Write the record to the
    ! same index as the length of variable corresponding to the unlimited
    ! dimension.

    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=*), INTENT(IN) :: VarName
    CLASS(*), DIMENSION(:,:), INTENT(IN) :: SourceData
    CHARACTER(LEN=*), OPTIONAL :: UnlimitedDim

    ! Dimension and target variables
    TYPE(NCVariable) :: DimensionVariable, TargetVariable

    ! Check if UnlimitedDim was provided, if not assume time
    IF (.NOT. PRESENT(UnlimitedDim)) THEN
      UnlimitedDim = "time"
    END IF

    ! Make sure the file has an unlimited dimension
    IF (.NOT. OutFile%HasUnlimitedDimension) THEN
      WRITE(ERROR_UNIT,*) "File has no unlimited dimension, cannot add record"
    END IF

    ! Now assign the variable data
    TargetVariable = get_target_variable(OutFile, VarName)

    SELECT TYPE (SourceData)
    TYPE IS (REAL)
      CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
        SourceData, START=[ProcDomain%ProcessDomainStart(1),&
        ProcDomain%ProcessDomainStart(2), OutFile%UnlimitedDimensionLength],&
        COUNT=[ProcDomain%ProcessDomainSize(1), ProcDomain%ProcessDomainSize(2),&
        1]))
    TYPE IS (INTEGER)
      CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
        SourceData, START=[ProcDomain%ProcessDomainStart(1),&
        ProcDomain%ProcessDomainStart(2), OutFile%UnlimitedDimensionLength],&
        COUNT=[ProcDomain%ProcessDomainSize(1), ProcDomain%ProcessDomainSize(2),&
        1]))
    END SELECT

  END SUBROUTINE put_record_rank2

  SUBROUTINE write_to_record_rank2(OutFile, VarName, SourceData, Year, Step)
    !*## Purpose
    !
    ! A handler for handling variables agnostically of the temporal aggregation
    ! method.
    !
    !## Method
    !
    ! Add the passed SourceData to the accumulation array. When the
    ! accumulation period has elapsed, average the result and write it to file.

    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=*), INTENT(INOUT) :: VarName
    REAL, DIMENSION(:,:), INTENT(IN) :: SourceData
    INTEGER, INTENT(IN) :: Year, Step

    TYPE(RealNCVariable) :: TargetVariable

    TargetVariable = get_target_variable(OutFile, VarName)

    ! If we've just triggered a write, then we need to determine the next
    ! trigger point
    IF (TargetVariable%AggCounter == 0) THEN
      determine_aggregation_period(TargetVariable, Year, Step)
    END IF

    ! Accumulate the variable, and increment the counter
    TargetVariable%AccumData = TargetVariable%AccumData + SourceData
    TargetVariable%AggCounter = TargetVariable%AggCounter + 1

    ! If we reach our trigger value, add to the time dimension, write the
    ! record to file, reset the accumulator and work out the next trigger
    IF (TargetVariable%AggCounter == TargetVariable%WriteTrigger) THEN
      put_record(Outfile, VarName, TargetVariable%AccumData /&
        TargetVariable%AggCounter)
    END IF
  END SUBROUTINE write_to_record_rank2
      
  SUBROUTINE set_fill_value(OutFile, VarName, FillVal)
    !*## Purpose
    !
    ! Set the fill value for the NCVariable.
    !
    !## Method
    !
    ! Set the fill value for the specified variable.

    TYPE(NCFile), INTENT(IN) :: OutFile
    CHARACTER(LEN=*), INTENT(IN) :: VarName
    CLASS(*), INTENT(IN) :: FillVal

    TYPE(NCVariable) :: TargetVariable

    ! Get the target variable, then assign the _FillValue attribute
    TargetVariable = get_target_variable(OutFile, VarName)
    SELECT TYPE (FillVal)
    TYPE IS (REAL)
      CALL handle_ncstat(NF90_DEF_VAR_FILL(OutFile%FileID, TargetVariable%VarID,&
        0, FillVal))
    TYPE IS (INTEGER)
      CALL handle_ncstat(NF90_DEF_VAR_FILL(OutFile%FileID, TargetVariable%VarID,&
        0, FillVal))
    END SELECT

  END SUBROUTINE set_fill_value

  FUNCTION get_target_variable(OutFile, VarName) RESULT(TargetVariable)
    !*## Purpose
    !
    ! Get the target variable from the parent NCFile.
    !
    !## Method
    !
    ! Search the names of the attached variables until the given variable name
    ! is found, then return that NCVariable.

    TYPE(NCFile) :: OutFile
    CHARACTER(LEN=*) :: VarName
    TYPE(NCVariable) :: TargetVariable
    
    INTEGER :: VarIter
    LOGICAL :: FoundVariable

    ! Checker to make sure the operation was a success
    FoundVariable = .FALSE.

    ! Get the relevant variable
    DO VarIter = 1, SIZE(OutFile%Variables)
      IF (OutFile%Variables(VarIter)%VarName == VarName) THEN
        TargetVariable = OutFile%Variables(VarIter)
        FoundVariable = .TRUE.
        EXIT
      END IF
    END DO

    IF (.NOT. FoundVariable) THEN
      WRITE(ERROR_UNIT,*) "Search for variable "//VarName//" failed."
      CALL mpi_grp%abort() 
    END IF

  END FUNCTION get_target_variable

  SUBROUTINE close_file(OutFile)
    !*## Purpose
    !
    ! Close the file handle attached to the output file

    TYPE(NCFile), INTENT(IN) :: OutFile

    CALL handle_ncstat(NF90_CLOSE(OutFile%FileID))

  END SUBROUTINE close_file
END MODULE output_module
