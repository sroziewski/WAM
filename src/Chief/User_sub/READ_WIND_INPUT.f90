SUBROUTINE READ_WIND_INPUT

! ---------------------------------------------------------------------------- !
!                                                                              !
!   READ_WIND_INPUT - ROUTINE TO READ WINDFIELDS.                              !
!                                                                              !
!     HEINZ GUNTHER    ECMWF   OCTOBER 1989                                    !
!     HEINZ GUNTHER    GKSS    JANUARY 2001                                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       INPUT OF WINDFIELDS.                                                   !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!        FORMATTED READ FROM UNIT IU01, FILE01.                                !
!                                                                              !
!        IN THE WIND ARRAY "U_MAP(:,:)" AND "V_MAP(:,:)"                         !
!        THE POINTS MUST BE ORDERED AS FOLLOWS                                 !
!                       (1,    1   ) <==> SOUTH WEST CORNER                    !
!                       (N_LON, 1   ) <==> SOUTH EAST CORNER                    !
!                       (1,    N_LAT) <==> NORTH WEST CORNER                    !
!                       (N_LON, N_LAT) <==> NORTH EAST CORNER                    !
!                                                                              !
! ---------------------------------------------------------------------------- !
!
!*    EXTERNALS.
!     ----------

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1                     !! TERMINATES PROCESSING.

USE WAM_WIND_MODULE,       ONLY: &
&       SET_WIND_HEADER,         & !! SETS WIND HEADER 
&       SET_WIND_FIELD,          & !! SETS WIND FIELD 
&       PRINT_WIND_STATUS          !! PRINTS WIND MODULE STATUS

! ---------------------------------------------------------------------------- !
!
!*    MODULE VARIABLES.
!     -----------------

USE WAM_FILE_MODULE, ONLY: IU06, ITEST, IU01, FILE01

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!
!*    LOCAL VARIABLES.
!     ----------------

LOGICAL, SAVE  :: FRSTIME = .TRUE.
INTEGER        :: LEN, IOS, J
REAL           :: D_LON, D_LAT, SOUTH, NORTH, WEST, EAST
INTEGER, SAVE  :: N_LON, N_LAT
INTEGER        :: ICODE

CHARACTER*14   :: CDTWIR
REAL, ALLOCATABLE :: U_MAP(:,:), V_MAP(:,:)

! ---------------------------------------------------------------------------- !
!
!*    1. FOR FIRST CALL, DETERMINE DATES.
!        --------------------------------

IF (FRSTIME) THEN
   LEN = LEN_TRIM(FILE01)
   OPEN (UNIT=IU01,FILE=FILE01(1:LEN),FORM='FORMATTED',STATUS='OLD',IOSTAT=IOS)
   IF (IOS.NE.0) THEN
      WRITE (IU06,*) ' ****************************************************'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *       FATAL ERROR IN SUB. READ_WIND_INPUT        *'
      WRITE (IU06,*) ' *       ===================================        *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' * WIND INPUT FILE COULD NOT BE OPENED              *'
      WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
      WRITE (IU06,*) ' *    FILE NAME IS  FILE01 = ', FILE01(1:LEN)
      WRITE (IU06,*) ' *    UNIT IS         IU01 = ', IU01
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' ****************************************************'
      CALL ABORT1
   END IF

   READ (IU01, *, IOSTAT=IOS)                                         &
&        SOUTH, NORTH, WEST, EAST, D_LON, D_LAT, N_LAT, N_LON, ICODE
   PRINT*,  SOUTH, NORTH, WEST, EAST, D_LON, D_LAT, N_LAT, N_LON, ICODE
   IF (IOS.NE.0) THEN
      WRITE (IU06,*) ' ****************************************************'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *       FATAL ERROR IN SUB. READ_WIND_INPUT        *'
      WRITE (IU06,*) ' *       ===================================        *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' * READ ERROR ON WIND FILE.                         *'
      WRITE (IU06,*) ' * FILE HEADER EXPECTED                             *'
      WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
      WRITE (IU06,*) ' *    FILE NAME IS  FILE01 = ', FILE01(1:LEN)
      WRITE (IU06,*) ' *    UNIT IS         IU01 = ', IU01
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' ****************************************************'
      CALL ABORT1
   END IF
   
   CALL SET_WIND_HEADER (WEST=WEST,   SOUTH=SOUTH,    &
&                        EAST=EAST,   NORTH=NORTH,    &
&                        D_LON=D_LON, D_LAT=D_LAT,    &
&                        N_LON=N_LON, N_LAT=N_LAT,    &
&                        CODE=ICODE)
   IF (ITEST.GT.0) CALL PRINT_WIND_STATUS
   FRSTIME = .FALSE.
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!*    2. ALLOCATE WIND INPUT ARRAYS.                                           !
!        ---------------------------                                           !

IF (.NOT.ALLOCATED(U_MAP) ) ALLOCATE(U_MAP(N_LON,N_LAT))
IF (.NOT.ALLOCATED(V_MAP) ) ALLOCATE(V_MAP(N_LON,N_LAT))

! ---------------------------------------------------------------------------- !
!                                                                              !
!*   3. READ WIND FIELD.                                                       !
!       -------------------                                                    !

READ(IU01,'(A14)',IOSTAT=IOS) CDTWIR
print *, '-->CDTWIR: ', CDTWIR
IF (IOS.NE.0) THEN
   WRITE (IU06,*) ' ****************************************************'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *       FATAL ERROR IN SUB. READ_WIND_INPUT        *'
   WRITE (IU06,*) ' *       ===================================        *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' * READ ERROR ON WIND FILE.                         *'
   WRITE (IU06,*) ' * DATE/TIME OF WINDFIELD EXPECTED                  *'
   WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
   WRITE (IU06,*) ' *    FILE NAME IS  FILE01 = ', FILE01(1:LEN)
   WRITE (IU06,*) ' *    UNIT IS         IU01 = ', IU01
   WRITE (IU06,*) ' *    RECORD IS          J = ', J
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' ****************************************************'
   CALL ABORT1
END IF

DO J = 1,N_LAT
   READ(IU01,'(132F8.2)',IOSTAT=IOS) U_MAP(1:N_LON,J)
   IF (IOS.NE.0) THEN
      WRITE (IU06,*) ' ****************************************************'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *       FATAL ERROR IN SUB. READ_WIND_INPUT        *'
      WRITE (IU06,*) ' *       ===================================        *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' * READ ERROR ON WIND FILE.                         *'
      WRITE (IU06,*) ' * U - COMPONENTS EXPECTED                          *'
      WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
      WRITE (IU06,*) ' *    FILE NAME IS  FILE01 = ', FILE01(1:LEN)
      WRITE (IU06,*) ' *    UNIT IS         IU01 = ', IU01
      WRITE (IU06,*) ' *    RECORD IS          J = ', J
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' ****************************************************'
      CALL ABORT1
   END IF
END DO
DO J = 1,N_LAT
   READ(IU01,'(132F8.2)',IOSTAT=IOS) V_MAP(1:N_LON,J)
   IF (IOS.NE.0) THEN
      WRITE (IU06,*) ' ****************************************************'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *       FATAL ERROR IN SUB. READ_WIND_INPUT        *'
      WRITE (IU06,*) ' *       ===================================        *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' * READ ERROR ON WIND FILE.                         *'
      WRITE (IU06,*) ' * V - COMPONENTS EXPECTED                          *'
      WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
      WRITE (IU06,*) ' *    FILE NAME IS  FILE01 = ', FILE01(1:LEN)
      WRITE (IU06,*) ' *    UNIT IS         IU01 = ', IU01
      WRITE (IU06,*) ' *    RECORD IS          J = ', J
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' ****************************************************'
      CALL ABORT1
   END IF
END DO
CALL SET_WIND_FIELD (CDTWIR, U_MAP, V_MAP)

! ---------------------------------------------------------------------------- !
!
!*    2. WRITE TEST OUTPUT.
!        ------------------

IF (ITEST.GT.1) THEN
   WRITE(IU06,*) ' READ_WIND_INPUT -  WIND FIELD FOR THE CDTWIR = ', CDTWIR
   WRITE(IU06,'(1X,24F5.2)') U_MAP(1:MIN(24,N_LON),1:MIN(5,N_LAT))
   WRITE(IU06,*) ' '
   WRITE(IU06,'(1X,24F5.2)') V_MAP(1:MIN(24,N_LON),1:MIN(5,N_LAT))
END IF

DEALLOCATE(U_MAP)
DEALLOCATE(V_MAP)

END SUBROUTINE READ_WIND_INPUT

