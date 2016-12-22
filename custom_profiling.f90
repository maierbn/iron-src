
MODULE Custom_Profiling
IMPLICIT NONE
  PRIVATE
  
  PUBLIC :: CustomProfilingStart
  PUBLIC :: CustomProfilingStop
  PUBLIC :: CustomProfilingMemory
  PUBLIC :: CustomProfilingGetInfo
  PUBLIC :: CustomProfilingGetDuration
  PUBLIC :: CustomProfilingGetMemory
  PRIVATE :: GetDurationIndex
  PRIVATE :: GetMemoryIndex
  PRIVATE :: PrintWarningDuration
  PRIVATE :: PrintWarningMemory
  
  INTEGER, PARAMETER :: IDENTIFIER_LENGTH = 20
  
  CHARACTER(LEN=IDENTIFIER_LENGTH), DIMENSION(50) :: DurationIdentifiers    !< identifiers for duration records
  CHARACTER(LEN=IDENTIFIER_LENGTH), DIMENSION(50) :: MemoryIdentifiers      !< identifier for memory records
  REAL(8), DIMENSION(50) :: Durations
  REAL(8), DIMENSION(50) :: StartTime
  INTEGER(8), DIMENSION(50) :: MemoryConsumptions
  INTEGER(8), DIMENSION(50) :: TimeCount
  INTEGER :: SizeDuration
  INTEGER :: SizeMemory
  
CONTAINS

  !
  !================================================================================================================================
  !
  SUBROUTINE CustomProfilingStart(Identifier)
    ! PARAMETERS
    CHARACTER(LEN=*), INTENT(IN)  :: Identifier !< A custom Identifier that describes the timer
    
    ! LOCAL VARIABLES
    INTEGER :: CurrentIndex
    
    ! find index of identifier
    CurrentIndex = GetDurationIndex(Identifier)
  
    ! If record with this identifier does not yet exist, create new
    IF (CurrentIndex == 0) THEN
      SizeDuration = SizeDuration + 1
      CurrentIndex = SizeDuration
      DurationIdentifiers(CurrentIndex) = Identifier
      Durations(CurrentIndex) = 0.0_8
      TimeCount(CurrentIndex) = 0
    ENDIF
    
    CALL CPU_TIME(StartTime(CurrentIndex))
  
  END SUBROUTINE
  !
  !================================================================================================================================
  !
  SUBROUTINE CustomProfilingStop(Identifier)
    ! PARAMETERS
    CHARACTER(LEN=*), INTENT(IN)  :: Identifier !< A custom Identifier that describes the timer
    
    ! LOCAL VARIABLES
    INTEGER :: CurrentIndex, I
    REAL(8) :: EndTime, Duration
    
    CALL CPU_TIME(EndTime)
    
    ! find index of identifier
    CurrentIndex = GetDurationIndex(Identifier)
  
    IF (CurrentIndex == 0) THEN
      CALL PrintWarningDuration(Identifier)
      RETURN
    ENDIF
    
    Duration = EndTime - StartTime(CurrentIndex)
    Durations(CurrentIndex) = Durations(CurrentIndex) + Duration
    TimeCount(CurrentIndex) = TimeCount(CurrentIndex) + 1
    
  END SUBROUTINE

  !
  !================================================================================================================================
  !
  SUBROUTINE CustomProfilingMemory(Identifier, MemoryConsumption)
    ! PARAMETERS
    CHARACTER(LEN=*), INTENT(IN)  :: Identifier !< A custom Identifier that describes the timer
    INTEGER, INTENT(IN) :: MemoryConsumption  !< MemoryConsumption to record
    
    ! LOCAL VARIABLES
    INTEGER :: I, CurrentIndex
    
    ! find index of identifier
    CurrentIndex = GetMemoryIndex(Identifier)
  
    ! If record with this identifier does not yet exist, create new
    IF (CurrentIndex == 0) THEN
      SizeMemory = SizeMemory + 1
      CurrentIndex = SizeMemory
      MemoryIdentifiers(CurrentIndex) = Identifier
      MemoryConsumptions(CurrentIndex) = 0
    ENDIF
    
    ! Add value to record of memory consumption
    MemoryConsumptions(CurrentIndex) = MemoryConsumptions(CurrentIndex) + MemoryConsumption
  
  END SUBROUTINE
  !
  !================================================================================================================================
  !
  !>Checks if a given Identifier is contained in the records. If yes, return the matching index, if no returns 0
  FUNCTION GetDurationIndex(Identifier)
    CHARACTER(LEN=*), INTENT(IN) :: Identifier
    INTEGER :: GetDurationIndex    !< return value
    INTEGER :: I
  
    GetDurationIndex = 0
    
    DO I=1,SizeDuration
      IF (TRIM(DurationIdentifiers(I)) == Identifier) THEN
      GetDurationIndex = I
      EXIT
      ENDIF
    END DO
  END FUNCTION GetDurationIndex
  !
  !================================================================================================================================
  !
  !>Checks if a given Identifier is contained in the records. If yes, return the matching index, if no returns 0
  FUNCTION GetMemoryIndex(Identifier)
    CHARACTER(LEN=*), INTENT(IN) :: Identifier
    INTEGER :: GetMemoryIndex    !< return value
    INTEGER :: I
  
    GetMemoryIndex = 0
    
    DO I=1,SizeMemory
      IF (TRIM(MemoryIdentifiers(I)) == Identifier) THEN
      GetMemoryIndex = I
      EXIT
      ENDIF
    END DO
  END FUNCTION GetMemoryIndex
  !
  !================================================================================================================================
  !
  FUNCTION CustomProfilingGetInfo()
    CHARACTER(LEN=2000) :: CustomProfilingGetInfo
    CHARACTER(LEN=1000) :: Line
    INTEGER :: I
    
    ! collect timing
    CustomProfilingGetInfo = NEW_LINE('A') // '---------------------Profiling Info-----------------------' // NEW_LINE('A')
    CustomProfilingGetInfo = TRIM(CustomProfilingGetInfo) // "Timing" // NEW_LINE('A')
    DO I = 1,SizeDuration
      WRITE(Line,"(3A,F17.13,A,I10,2A)") "   ", (DurationIdentifiers(I)), ": ", Durations(I), ' s', &
	& TimeCount(I), 'x', NEW_LINE('A')
      CustomProfilingGetInfo = TRIM(CustomProfilingGetInfo) // TRIM(Line)
    ENDDO
    
    ! Collect memory consumption
    CustomProfilingGetInfo = TRIM(CustomProfilingGetInfo) // "Memory Consumption" // NEW_LINE('A')
    DO I = 1,SizeMemory
      WRITE(Line,"(3A,I17,2A)") "   ", (MemoryIdentifiers(I)), ": ", MemoryConsumptions(I), ' B', NEW_LINE('A')
      CustomProfilingGetInfo = TRIM(CustomProfilingGetInfo) // TRIM(Line)
    ENDDO
    
    CustomProfilingGetInfo = TRIM(CustomProfilingGetInfo) // "----------------------------------------------------------" &
      & // NEW_LINE('A')
  END FUNCTION
  !
  !================================================================================================================================
  !
  FUNCTION CustomProfilingGetDuration(Identifier)
    CHARACTER(LEN=*), INTENT(IN)  :: Identifier !< A custom Identifier that describes the timer
    REAL(8) :: CustomProfilingGetDuration
    
    INTEGER :: CurrentIndex
    
    ! find index of identifier
    CurrentIndex = GetDurationIndex(Identifier)
  
    IF (CurrentIndex == 0) THEN
      CALL PrintWarningDuration(Identifier)
      CustomProfilingGetDuration = 0
      RETURN
    ENDIF
    
    CustomProfilingGetDuration = Durations(CurrentIndex)
  END FUNCTION
  !
  !================================================================================================================================
  !
  FUNCTION CustomProfilingGetMemory(Identifier)
    CHARACTER(LEN=*), INTENT(IN)  :: Identifier !< A custom Identifier that describes the timer
    REAL(8) :: CustomProfilingGetMemory
    
    INTEGER :: CurrentIndex
    
    ! find index of identifier
    CurrentIndex = GetMemoryIndex(Identifier)
  
    IF (CurrentIndex == 0) THEN
      CALL PrintWarningMemory(Identifier)
      CustomProfilingGetMemory = 0
      RETURN
    ENDIF
    
    CustomProfilingGetMemory = MemoryConsumptions(CurrentIndex)
  END FUNCTION
  !
  !================================================================================================================================
  !
  SUBROUTINE PrintWarningDuration(Identifier)
    CHARACTER(LEN=*), INTENT(IN)  :: Identifier
    INTEGER :: I
    
    PRINT *, "Warning! Identifier for Duration, '", Identifier, "' does not exist. "
    PRINT *, SizeDuration, " existing records: "
    DO I = 1,SizeDuration
      PRINT *, "  '", DurationIdentifiers(I), "'"
    ENDDO
  END SUBROUTINE
  !
  !================================================================================================================================
  !
  SUBROUTINE PrintWarningMemory(Identifier)
    CHARACTER(LEN=*), INTENT(IN)  :: Identifier
    INTEGER :: I
    
    PRINT *, "Warning! Identifier for Memory, '", Identifier, "' does not exist. "
    PRINT *, SizeMemory, " existing records: "
    DO I = 1,SizeMemory
      PRINT *, "  '", MemoryIdentifiers(I), "'"
    ENDDO
  END SUBROUTINE
  
END MODULE Custom_Profiling
