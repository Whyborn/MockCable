PROGRAM test_datasetreader

  USE datasetreader_module
  USE time_module


  INTEGER :: Year = 1965, StepIndex = 200

  TYPE(DatasetReader) :: Reader
  CHARACTER(LEN=*), DIMENSION(*), PARAMETER :: RainNames = &
    [CHARACTER(LEN=5) :: 'rain', 'ps', 'Rain', 'Rainf']
  CHARACTER(LEN=300) :: RainFiles

  INTEGER 
  RainFiles = '/home/lachlan/Work/ANU/rp23/experiments/2024-03-12_CABLE4-dev/'&
    //'lw5085/CABLE-as-ACCESS/MetForcing/ACCESS-ESM1p5-to-CABLE_Rainf_196*.nc'

  Reader = initialise_datasetreader_at_timestep(RainFiles, RainNames, 10800.0)

  WRITE(*,*) "Set of files:", Reader%DatasetFiles
  WRITE(*,*) "Index range:", Reader%IndexRange
  WRITE(*,*) "Start year:", Reader%StartYear

  

END PROGRAM test_datasetreader
