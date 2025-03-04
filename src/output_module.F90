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
    CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: VarNames
    INTEGER, DIMENSION(:), ALLOCATABLE :: VarIDs

  END TYPE NCFile

  INTERFACE initialise_output_file
    PROCEDURE initialise_output_file_by_name
    PROCEDURE initialise_output_file_with_dimensions
  END INTERFACE initialise_output_file

  INTERFACE add_variables
    PROCEDURE add_variables_multiple_dims
    PROCEDURE add_variables_single_dim
    PROCEDURE add_variable_multiple_dims
    PROCEDURE add_variable_single_dim
  END INTERFACE

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

    ! Possibly required temporary variable name/ID holders
    CHARACTER(LEN=20), DIMENSION(:), ALLOCATABLE :: TempVarNames
    INTEGER, DIMENSION(:), ALLOCATABLE :: TempVarIDs

    ! We need to set the start point for this set of variables
    INTEGER :: StartPoint

    ! Store inferred DimIDs
    INTEGER, DIMENSION(:), ALLOCATABLE :: VarDimIDs

    ! We will want to redefine the length of the file's variables, if it's
    ! already allocated
    IF (ALLOCATED(OutFile%VarNames)) THEN
      ! We've been through the process before, so we need to redefine the array
      ! of variable names and IDs, rather than just allocate
      TempVarNames(:) = OutFile%VarNames
      TempVarIDs(:) = OutFile%VarIDs

      ! Set the starting point for this set of variables
      StartPoint = SIZE(OutFile%VarNames)

      ! Now reallocate the file's variables, with their new size
      DEALLOCATE(OutFile%VarNames, OutFile%VarIDs)
      ALLOCATE(OutFile%VarNames(StartPoint + SIZE(VarNames)))
      ALLOCATE(OutFile%VarIDs(StartPoint + SIZE(VarNames)))

      ! Reset the first set of variable names and IDs
      OutFile%VarNames(1:StartPoint) = TempVarNames
      OutFile%VarIDs(1:StartPoint) = TempVarIDs
    ELSE
      ! No variables have been defined for this file yet- just allocate
      ALLOCATE(OutFile%VarNames(SIZE(VarNames)))
      ALLOCATE(OutFile%VarIDs(SIZE(VarNames)))

      StartPoint = 1
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
        VarDimIDs, OutFile%VarIDs(StartPoint + VarIter))
      OutFile%VarNames(StartPoint + VarIter) = VarNames(VarIter)
    END DO

  END SUBROUTINE add_variables_multiple_dim


