MODULE output_module

  USE netcdf
  USE mpi_module, ONLY: mpi_grp_t
  USE domain_module, ONLY: ProcessDomain, GlobalDomain

  IMPLICIT NONE

  TYPE NCFile
    !*## Purpose
    !
    ! A derived type used to assist in output routines
    INTEGER :: FileID

    ! Dimensions
    CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: DimNames
    INTEGER, DIMENSION(:), ALLOCATABLE :: DimIDs
    INTEGER, DIMENSION(:), ALLOCATABLE :: DimLengths

    ! Variables
    TYPE(NCVariable), DIMENSION(:), ALLOCATABLE :: Variables

  END TYPE NCFile

  TYPE NCVariable
    !*## Purpose
    !
    ! A derived type used to assist in output routines
    INTEGER VarID
    CHARACTER(LEN=20) :: VarName

    ! Dimension names and IDs
    CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: DimNames
    INTEGER, DIMENSION(:), ALLOCATABLE :: DimIDs
  END TYPE NCVariable

  INTERFACE initialise_output_file
    PROCEDURE initialise_output_file_by_name
    PROCEDURE initialise_output_file_with_dimensions
  END INTERFACE initialise_output_file

  INTERFACE add_variables
    PROCEDURE add_variables_multiple_dims
    PROCEDURE add_variables_single_dim
    PROCEDURE add_variable_multiple_dims
    PROCEDURE add_variable_single_dim
  END INTERFACE add_variables

  INTERFACE set_variable_data
    PROCEDURE set_variable_data_real_rank2
    PROCEDURE set_variable_data_real_rank3
    PROCEDURE set_variable_data_int_rank2
    PROCEDURE set_variable_data_int_rank3
  END INTERFACE set_variable_data

  ! Store information about the MPI configuration
  TYPE(ProcessDomain), PRIVATE :: _ProcDomain
  TYPE(mpi_grp_t), PRIVATE :: _mpi_grp

CONTAINS

  SUBROUTINE initialise_output_module(ProcDomain, mpi_grp)
    !*## Purpose
    !
    ! Set up the output module for future writing
    !
    !## Method
    !
    ! Bind local copies of the mpi configuration and local domain.

    TYPE(ProcessDomain), INTENT(IN) :: ProcDomain
    TYPE(mpi_grp_t), INTENT(IN) :: mpi_grp

    _ProcDomain = ProcDomain
    _mpi_grp = mpi_grp

  END SUBROUTINE initialise_output_module

  FUNCTION initialise_output_file_by_name(FileName) RESULT(OutFile)
    !*## Purpose
    !
    ! Initialise a new output file with the given name.
    !
    !## Method
    !
    ! Use NetCDF routines to open a file in parallel if necessary.

    CHARACTER(LEN=200) :: FileName
    TYPE(NCFile) :: OutFile

#ifdef __MPI__
    CALL handle_ncstat(NF90_CREATE(FileName, NF90_CLOBBER, OutFile%FileID,&
        mpi_grp%comm, MPI_INFO_NULL))
#else
    CALL handle_ncstat(NF90_CREATE(FileName, NF90_CLOBBER, OutFile%FileID))
#endif

  END FUNCTION initialise_output_file_by_name

  FUNCTION initialise_output_file_with_dimensions(FileName, DimNames,&
    DimLengths) RESULT(OutFile)
    !*## Purpose
    !
    ! Initialise a new output file with the given name.
    !
    !## Method
    !
    ! Use NetCDF routines to open a file in parallel if necessary, and then
    ! assign dimensions to it.

    CHARACTER(LEN=200) :: FileName
    CHARACTER(LEN=20), DIMENSION(:) :: DimNames
    INTEGER, DIMENSION(:) :: DimLengths
    TYPE(NCFile) :: OutFile

    ! Initialise the file
    OutFile = initialise_output_file_by_name(FileName)

    ! Add dimensions
    CALL set_dimensions(OutFile, DimNames, DimLengths)

  END FUNCTION initialise_output_file_with_dimensions

  SUBROUTINE set_dimensions(OutFile, DimNames, DimLengths)
    !*## Purpose
    !
    ! Add the dimensions with the given lengths to the NetCDF file.
    !
    !## Method
    !
    ! Use NetCDF routines to add dimensions to the NetCDF file and the wrapper.

    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=20), DIMENSION(:), INTENT(IN) :: DimNames
    INTEGER, DIMENSION(:), INTENT(IN) :: DimLengths

    ! Iterator for the dimensions
    INTEGER :: DimIter

    ! Make sure that there are the same number of DimNames and DimLengths
    IF (SIZE(DimNames) /= SIZE(DimLengths)) THEN
      WRITE(ERROR_UNIT,*) "Different number of dimension names and lengths"//&
        " given to add_dimensions."
    END IF

    ! Allocate memory for the derived type names
    ALLOCATE(OutFile%DimNames(SIZE(DimNames)))
    ALLOCATE(OutFile%DimLengths(SIZE(DimLengths)))

    ! Run through the passed dimensions
    DO DimIter = 1, SIZE(DimNames)
      CALL handle_ncstat(NF90_DEF_DIM(OutFile%FileID, TRIM(DimNames(DimIter)),&
        DimLengths(DimIter), OutFile%DimIDs(DimIter)))
    END DO

  END SUBROUTINE set_dimensions

  SUBROUTINE add_variables_multiple_dim(OutFile, VarNames, VarDims, DataType)
    !*## Purpose
    !
    ! Add a variable to the NetCDF file.
    !
    !## Method
    !
    ! Use NetCDF routines to add the variable to the NetCDF file.

    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=20), DIMENSION(:), INTENT(IN) :: VarNames
    CHARACTER(LEN=20), DIMENSION(:), INTENT(IN) :: VarDims
    INTEGER, INTENT(IN) :: DataType

    ! iterators
    INTEGER :: DimIter, FileDimIter, VarIter

    ! Possibly required temporary variables, if a resize is needed
    CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: TempVariables

    ! We need to set the start point for this set of variables
    INTEGER :: StartPoint

    ! Store inferred DimIDs
    INTEGER, DIMENSION(:), ALLOCATABLE :: VarDimIDs

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
      CALL handle_ncstat(OutFile%FileID, VarNames(VarIter), DataType,&
        VarDimIDs, OutFile%Variables(StartPoint + VarIter)%VarID)

      ! Set up the NCVariable
      OutFile%Variables(StartPoint + VarIter)%VarName = VarNames(VarIter)
      OutFile%Variables(StartPoint + VarIter)%DimNames = VarDims
      OutFile%Variables(StartPoint + VarIter)%DimIDs = VarDimIDs
    END DO

  END SUBROUTINE add_variables_multiple_dim

  SUBROUTINE add_variables_single_dim(OutFile, VarNames, VarDim, DataType)
    !*## Purpose
    !
    ! Add single dimensioned variables to the NCFile.
    !
    !## Method
    !
    ! Invoke the full-featured add_variables_multiple_dims function
    
    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=20), DIMENSION(:), INTENT(IN) :: VarNames
    CHARACTER(LEN=20), INTENT(IN) :: VarDim
    INTEGER, INTENT(IN) :: DataType

    CALL add_variables_multiple_dims(OutFile, VarNames, [VarDim], DataType)

  END SUBROUTINE add_variables_single_dim

  SUBROUTINE add_variable_multiple_dims(OutFile, VarName, VarDims, DataType)
    !*## Purpose
    !
    ! Add a single variable to the NCFile.
    !
    !## Method
    !
    ! Invoke the full-featured add_variables_multiple_dims function
    
    TYPE(NCFile), INTENT(INOUT) :: OutFile
    CHARACTER(LEN=20), INTENT(IN) :: VarName
    CHARACTER(LEN=20), DIMENSION(:), INTENT(IN) :: VarDims
    INTEGER, INTENT(IN) :: DataType

    CALL add_variables_multiple_dims(OutFile, [VarName], VarDims, DataType)

  END SUBROUTINE add_variable_multiple_dims

  SUBROUTINE add_variable_single_dim(OutFile, VarName, VarDim, DataType)
    !*## Purpose
    !
    ! Add a single dimensioned variable to the NCFile.
    !
    !## Method
    !
    ! Invoke the full-featured add_variables_multiple_dims function

    CALL add_variables_multiple_dims(OutFile, [VarName], [VarDim], DataType)

  END SUBROUTINE add_variable_single_dim

  SUBROUTINE set_variable_data_real_rank2(OutFile, VarName, SourceData)
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
    CHARACTER(LEN=20), INTENT(IN) :: VarName
    REAL, DIMENSION(:,:), ALLOCATABLE, INTENT(IN) :: SourceData

    ! The target variable we're writing to
    TYPE(NCVariable) :: TargetVariable

    TargetVariable = get_target_variable(OutFile, VarName)

    ! Is there any world where a rank 2 array would not be describing a spatial
    ! map? For now, assume not, so we know that the dimensions are lon, lat and
    ! should be chunked up accordingly.
    CALL handle_ncstat(NF90_PUT_VAR(OutFile%VarID, TargetVariable%VarID,&
      SourceData, START=ProcDomain%ProcessDomainStart))

  END SUBROUTINE set_variable_data_real_rank2

  SUBROUTINE set_variable_data_real_rank3(OutFile, VarName, SourceData)
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
    CHARACTER(LEN=20), INTENT(IN) :: VarName
    REAL, DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: SourceData
    
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
        DimStarts[DimIter] = ProcDomain%ProcessDomainStarts(1)
      ELSEIF (TRIM(TargetVariable%DimNames(DimIter)) == "lat") THEN
        DimStarts[DimIter] = ProcDomain%ProcessDomainStarts(2)
      END IF
    END DO

    ! Now we can write the variable
    CALL handle_ncstat(NF90_PUT_VAR(OutFile%FileID, TargetVariable%VarID,&
      START=DimStarts)

  END SUBROUTINE set_variable_data_real_rank3

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
    CHARACTER(LEN=20) :: VarName
    TYPE(NCVariable) :: TargetVariable

    ! Get the relevant variable
    DO VarIter = 1, SIZE(OutFile%Variables)
      IF (OutFile%Variables(VarIter)%VarName == VarName) THEN
        TargetVariable = OutFile%Variables(VarIter)
        EXIT
      END IF
    END DO

  END FUNCTION get_target_variable
