SUBROUTINE READ_ICE_INPUT
                                                                              
! ---------------------------------------------------------------------------- !
!                                                                              !
!   READ_ICE_INPUT* - READ AN ICE MAP.                                         !
!                                                                              !
!     HEINZ GUNTHER    GKSS    JANUARY 1995                                    !
!                                                                              !
!     PURPOSE                                                                  !
!     -------                                                                  !
!                                                                              !
!       TO READ AN ICE MAP.                                                    !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!        FORMATTED READ FROM UNIT IU03, FILE03.                                !
!                                                                              !
!        THE ICE MAP ARRAY "ICE_GRID(I,K)" MUST BE IDENTICAL TO THE WAVE       !
!        MODEL GRID. THE POINTS MUST BE ORDERED AS FOLLOWS                     !
!                       (1,  1 ) <==> SOUTH WEST                               !
!                       (NX, 1 ) <==> SOUTH EAST                               !
!                       (1,  NY) <==> NORTH WEST                               !
!                       (NX, NY) <==> NORTH EAST                               !
!                                                                              !
!     REFERENCES                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE                                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !
!
!*    EXTERNALS.
!     ----------

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1                     !! TERMINATES PROCESSING.

USE WAM_ICE_MODULE,       ONLY:  &
&       SET_ICE                    !! ICE INPUT INTO MODULE.

! ---------------------------------------------------------------------------- !
!
!*    MODULE VARIABLES.
!     -----------------

USE WAM_FILE_MODULE,  ONLY: IU06, ITEST, IU03, FILE03

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!
!*    LOCAL VARIABLES.
!     ----------------

INTEGER  :: K, IOS, LEN
INTEGER  :: NX_ICE, NY_ICE
INTEGER, ALLOCATABLE :: ICE_GRID(:,:)
LOGICAL  :: ICE_RUN

! ---------------------------------------------------------------------------- !
!
!*    1. CHECK ICE DATA FILE.
!        --------------------

ICE_RUN = .FALSE.
LEN = LEN_TRIM(FILE03)
IF (LEN.GT.0) INQUIRE (FILE=FILE03(1:LEN), EXIST=ICE_RUN)
IF (.NOT.ICE_RUN) RETURN

! ---------------------------------------------------------------------------- !
!
!*    2. OPEN ICE DATA FILE.
!        -------------------

IOS = 0
OPEN (UNIT=IU03, FILE=FILE03(1:LEN), FORM='FORMATTED', STATUS='OLD', IOSTAT=IOS)

IF (IOS.NE.0) THEN
   WRITE(IU06,*) ' *******************************************'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *   FATAL ERROR IN SUB. READ_ICE_INPUT    *'
   WRITE(IU06,*) ' *   ==================================    *'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' * OPEN ERROR IN ICE FILE                  *'
   WRITE(IU06,*) ' * ERROR CODE IS IOSTAT = ', IOS
   WRITE(IU06,*) ' * UNIT IS         IU03 = ', IU03
   WRITE(IU06,*) ' * UNIT IS       FILE03 = ', FILE03(1:LEN)
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' * PROGRAM ABORTS     PROGRAM ABORTS       *'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *******************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!
!*    3. READ ICE DATA HEADER.
!        ---------------------

READ (UNIT=IU03, FMT='(2I10)', IOSTAT=IOS) NX_ICE, NY_ICE

IF (IOS.NE.0) THEN
   WRITE(IU06,*) ' *******************************************'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *   FATAL ERROR IN SUB. READ_ICE_INPUT    *'
   WRITE(IU06,*) ' *   ==================================    *'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' * READ ERROR IN ICE GRID HEADER RECORD    *'
   WRITE(IU06,*) ' * ERROR CODE IS IOSTAT = ', IOS
   WRITE(IU06,*) ' * UNIT IS         IU03 = ', IU03
   WRITE(IU06,*) ' * UNIT IS       FILE03 = ', FILE03(1:LEN)
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' * PROGRAM ABORTS     PROGRAM ABORTS       *'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *******************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!
!*    4. READ ICE DATA.
!        --------------

IF (.NOT.ALLOCATED(ICE_GRID)) ALLOCATE (ICE_GRID(1:NX_ICE,1:NY_ICE))

DO K=1,NY_ICE
   READ (UNIT=IU03, FMT='(80I1)', IOSTAT=IOS) ICE_GRID(1:NX_ICE,K)
   IF (IOS.NE.0) THEN
      WRITE(IU06,*) ' *******************************************'
      WRITE(IU06,*) ' *                                         *'
      WRITE(IU06,*) ' *   FATAL ERROR IN SUB. READ_ICE_INPUT    *'
      WRITE(IU06,*) ' *   ==================================    *'
      WRITE(IU06,*) ' *                                         *'
      WRITE(IU06,*) ' * READ ERROR ON ICE GRID                  *'
      WRITE(IU06,*) ' * ERROR CODE IS IOSTAT = ', IOS
      WRITE(IU06,*) ' * GRID LINE  IS      K = ', K
      WRITE(IU06,*) ' * UNIT IS         IU03 = ', IU03
      WRITE(IU06,*) ' * UNIT IS       FILE03 = ', FILE03(1:LEN)
      WRITE(IU06,*) ' *                                         *'
      WRITE(IU06,*) ' * PROGRAM ABORTS     PROGRAM ABORTS       *'
      WRITE(IU06,*) ' *                                         *'
      WRITE(IU06,*) ' *******************************************'
      CALL ABORT1
   END IF
END DO

CLOSE (UNIT=IU03, STATUS='KEEP')

! ---------------------------------------------------------------------------- !
!
!*    5. TRANSFER ICE DATA TO MODULE.
!        ----------------------------

CALL SET_ICE (ICE_GRID)

IF (ALLOCATED(ICE_GRID)) DEALLOCATE (ICE_GRID)

END SUBROUTINE READ_ICE_INPUT
