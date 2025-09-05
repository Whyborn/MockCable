MODULE meteorology_module

USE datasetreader_module, ONLY: DatasetReader,&
  initialise_datasetreader_at_timestep, get_data, close_reader
use land_decomp_mod, only: land_decomp_t, io_decomp_grid
use cable_netcdf_mod

IMPLICIT NONE

TYPE MetType
  REAL, DIMENSION(:), ALLOCATABLE :: Rain, Pressure, Temperature, Wind,&
    ShortwaveRad, LongwaveRad
END TYPE MetType

! Set up the variable IDs
INTEGER, PARAMETER :: RainID = 1, TemperatureID = 2, WindID = 3,&
  PressureID = 4, ShortwaveRadID = 5, LongwaveRadID = 6, NumVariables = 6

TYPE(DatasetReader), DIMENSION(NumVariables) :: MetDataReaders

! File to write output to
class(cable_netcdf_file_t), allocatable :: output_file

class(cable_netcdf_decomp_t), allocatable :: decomp

CONTAINS

SUBROUTINE prepare_meteorology(Timestep, Met, land_decomp, write_output)
  !*## Purpose
  !
  ! Prepare the meteorology input routines
  !
  !## Method
  !
  ! Read the met.nml to determine the locations of the meteorology files.

  REAL, INTENT(IN) :: Timestep
  TYPE(MetType), INTENT(OUT) :: Met
  type(land_decomp_t), intent(in) :: land_decomp
  LOGICAL, INTENT(IN) :: write_output

  CHARACTER(LEN=300) :: RainFile, TemperatureFile, WindFile, PressureFile,&
    ShortwaveRadFile, LongwaveRadFile

  INTEGER :: nmlUnit, VarIter, NPoints

  NAMELIST /metnml/ RainFile, TemperatureFile, WindFile, PressureFile,&
  ShortwaveRadFile, LongwaveRadFile
  
  ! Set the default values for the files
  RainFile = ""
  TemperatureFile = ""
  WindFile = ""
  PressureFile = ""
  ShortwaveRadFile = ""
  LongwaveRadFile = ""

  OPEN(NEWUNIT=nmlUnit, FILE='met.nml', STATUS='OLD', ACTION='READ')
  READ(nmlUnit, NML=metnml)
  CLOSE(nmlUnit)

  ! Allocate memory for the readers
  NPoints = land_decomp%land_count
  ALLOCATE(Met%Rain(NPoints), Met%Temperature(NPoints), Met%Wind(NPoints),&
    Met%Pressure(NPoints), Met%ShortwaveRad(NPoints), Met%LongwaveRad(NPoints))

  ! Initialise the dataset readers
  MetDataReaders(RainID) = initialise_datasetreader_at_timestep(&
    RainFile, "Rainf", Timestep, land_decomp)
  MetDataReaders(TemperatureID) = initialise_datasetreader_at_timestep(&
    TemperatureFile, "Tair", Timestep, land_decomp)
  MetDataReaders(WindID) = initialise_datasetreader_at_timestep(&
    WindFile, "wind", Timestep, land_decomp)
  MetDataReaders(PressureID) = initialise_datasetreader_at_timestep(&
    PressureFile, "Psurf", Timestep, land_decomp)
  MetDataReaders(ShortwaveRadID) = initialise_datasetreader_at_timestep(&
    ShortwaveRadFile, "SWdown", Timestep, land_decomp)
  MetDataReaders(LongwaveRadID) = initialise_datasetreader_at_timestep(&
    LongwaveRadFile, "LWdown", Timestep, land_decomp)

  if (.not. write_output) return

  ! Create the file to write the output to
  output_file = cable_netcdf_create_file("meteorology_output.nc")
  call output_file%def_dims(["lon ", "lat ", "time"], [shape(land_decomp%land_mask%mask), CABLE_NETCDF_UNLIMITED])
  
  ! Set the longitude/latitude axes
  call output_file%def_var("lon", ["lon"], CABLE_NETCDF_FLOAT)
  call output_file%def_var("lat", ["lat"], CABLE_NETCDF_FLOAT)
  call output_file%def_var("time", ["time"], CABLE_NETCDF_FLOAT)

  ! Now initialise the meteorology variables
  call output_file%def_var("LWdown", ["lon ", "lat ", "time"], CABLE_NETCDF_FLOAT)
  call output_file%def_var("Rainf",  ["lon ", "lat ", "time"], CABLE_NETCDF_FLOAT)
  call output_file%def_var("Tair",   ["lon ", "lat ", "time"], CABLE_NETCDF_FLOAT)
  call output_file%def_var("wind",   ["lon ", "lat ", "time"], CABLE_NETCDF_FLOAT)
  call output_file%def_var("Psurf",  ["lon ", "lat ", "time"], CABLE_NETCDF_FLOAT)
  call output_file%def_var("SWdown", ["lon ", "lat ", "time"], CABLE_NETCDF_FLOAT)

  ! Put the dimension data
  call output_file%put_var("lon", land_decomp%land_mask%longitude)
  call output_file%put_var("lat", land_decomp%land_mask%latitude)

  decomp = io_decomp_grid(land_decomp, [land_decomp%land_count], CABLE_NETCDF_FLOAT)

END SUBROUTINE prepare_meteorology

SUBROUTINE get_meteorology(Year, Timestep, Met)
  !*## Purpose
  !
  ! Apply the meteorology from a given year and timestep.
  !
  !## Method
  !
  ! Pass the given year and timestep to the meteorology readers to extract the
  ! appropriate record from the dataset.

  INTEGER, INTENT(IN) :: Year, Timestep
  TYPE(MetType), INTENT(INOUT) :: Met

  ! Read the data from file into the DataStorage attached to the readers
  CALL get_data(MetDataReaders(RainID), Year, Timestep, Met%Rain)
  CALL get_data(MetDataReaders(PressureID), Year, Timestep, Met%Pressure)
  CALL get_data(MetDataReaders(TemperatureID), Year, Timestep, Met%Temperature)
  CALL get_data(MetDataReaders(WindID), Year, Timestep, Met%Wind)
  CALL get_data(MetDataReaders(ShortwaveRadID), Year, Timestep, Met%ShortwaveRad)
  CALL get_data(MetDataReaders(LongwaveRadID), Year, Timestep, Met%LongwaveRad)

END SUBROUTINE get_meteorology

SUBROUTINE write_meteorology(Met, Time)
  !*## Purpose
  !
  ! Write out the meteorology data back in matrix format
  !

  TYPE(MetType), INTENT(IN) :: Met
  INTEGER, INTENT(IN) :: Time

  call output_file%write_darray("Rainf", Met%Rain, decomp, frame=Time)
  call output_file%write_darray("Tair", Met%Temperature, decomp, frame=Time)
  call output_file%write_darray("wind", Met%Wind, decomp, frame=Time)
  call output_file%write_darray("Psurf", Met%Pressure, decomp, frame=Time)
  call output_file%write_darray("SWdown", Met%ShortwaveRad, decomp, frame=Time)
  call output_file%write_darray("LWdown", Met%LongwaveRad, decomp, frame=Time)

END SUBROUTINE write_meteorology

SUBROUTINE finalise_meteorology(write_output)
  !*## Purpose
  !
  ! Close all the file handles used in the meteorology
  LOGICAL, INTENT(IN) :: write_output

  INTEGER :: Iter

  if (write_output) CALL output_file%close()

  DO Iter = 1, NumVariables
    CALL close_reader(MetDataReaders(Iter))
  END DO

END SUBROUTINE finalise_meteorology

END MODULE meteorology_module
