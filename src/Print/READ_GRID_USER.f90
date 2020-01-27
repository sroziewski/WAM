SUBROUTINE READ_GRID_USER

! ---------------------------------------------------------------------------- !
!                                                                              !
!    READ_GRID_USER - ROUTINE TO READ USER INPUT OF PROG PRINT_GRID.           !
!                                                                              !
!     H. GUNTHER     GKSS/ECMWF     NOVEMBER 1989                              !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       READ USER INPUT CONCERNING PERIOD OF INTEREST,TIMESTEPS AND            !
!       OPTIONS. A CONSISTENCY CHECK IS DONE TOO.                              !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       USER INFORMATION IS BEING READ WITH THE PRESUMPTIONS THAT:             !
!         1. EVERY LINE STARTING WITH 'C' IS A COMMENT LINE                    !
!         2. VALUES ARE PUT IN BELOW POSITIONS INDICATED WITH '-'              !
!            (RIGHT-JUSTIFIED, BUT CHARACTER LEFT-JUSTIFIED)                   !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !
!                                                                              !
!       *ABORT1*     - TERMINATES PROCESSING.                                  !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!      EXTERNALS.                                                              !
!     -----------                                                              !

USE WAM_GENERAL_MODULE, ONLY:      &
&       ABORT1                        !! TERMINATES PROCESSING.

USE WAM_OUTPUT_SET_UP_MODULE,ONLY: &
&       SET_OUTPUT_FLAGS,          &  !! COMPUTES A TIME DIFFERENCE.
&       SET_OUTPUT_TIMES              !! COMPUTES A TIME DIFFERENCE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLE                                                       !

USE WAM_PRINT_MODULE, ONLY: IU05, IU06, ITEST, CDATEA, CDATEE, IDELDO,         &
&                           FILE01, CDTFILE, IDFILE
USE WAM_OUTPUT_SET_UP_MODULE,ONLY: TITL, NPOUT, CFLAG

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLE                                                           !

INTEGER                         :: I, IOS, LEN
CHARACTER (LEN=72)              :: LINE
INTEGER                         :: MOUTT, NOUTT
CHARACTER (LEN=14), ALLOCATABLE :: COUTT(:)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. READ USER INPUT                                                       !
!        ---------------                                                       !
CALL F_NEW_DATA
CDATEA = LINE( 2:15)
CDATEE = LINE(18:31)
READ(LINE(34:40),'(I7 )',ERR=4100,IOSTAT=IOS) IDELDO
IF (LINE(43:43).EQ.'M') IDELDO = IDELDO*60
IF (LINE(43:43).EQ.'H') IDELDO = IDELDO*3600

CALL F_NEW_DATA
READ(LINE( 2: 8),'(I7)',ERR=4100,IOSTAT=IOS) MOUTT
IF (MOUTT.GT.0) ALLOCATE (COUTT(1:MOUTT))

NOUTT = 0
LINE(1:4) = 'C   '
DO
   CALL F_NEW_DATA
   IF (LINE(2:4).EQ.'END') EXIT
   NOUTT = NOUTT+1
   IF (NOUTT.LE.MOUTT) COUTT(NOUTT) = LINE( 2:15)
   IF (LINE(18:20).EQ.' ') CYCLE
   NOUTT = NOUTT+1
   IF (NOUTT.LE.MOUTT) COUTT(NOUTT) = LINE(18:31)
   IF (LINE(34:36).EQ.' ') CYCLE
   NOUTT = NOUTT+1
   IF (NOUTT.LE.MOUTT) COUTT(NOUTT) = LINE(34:47)
   IF (LINE(50:52).EQ.' ') CYCLE
   NOUTT = NOUTT+1
   IF (NOUTT.LE.MOUTT) COUTT(NOUTT) = LINE(50:63)
END DO
IF (NOUTT.GT.MOUTT) THEN
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++'
   WRITE(IU06,*) '+                                           +'
   WRITE(IU06,*) '+   WARNING ERROR IN SUB. READ_GRID_USER    +'
   WRITE(IU06,*) '+   ====================================    +'
   WRITE(IU06,*) '+ NUMBER OF OUTPUT TIMES IN INPUT EXCEEDS   +'
   WRITE(IU06,*) '+ DIMENSION MOUTT                = ', MOUTT
   WRITE(IU06,*) '+ NUMBER OF TIMES INPUT IS NOUTT = ', NOUTT
   WRITE(IU06,*) '+ PROGRAM WILL IGNORE THE LAST OUTPUT TIMES +'
   WRITE(IU06,*) '+                                           +'
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++'
   NOUTT = MOUTT
END IF

CALL SET_OUTPUT_TIMES (N=NOUTT, TIME=COUTT)

CFLAG = .FALSE.
DO I=1,30,2
   CALL F_NEW_DATA
   CFLAG(  I) = LINE( 2: 2).EQ.'Y'
   CFLAG(I+1) = LINE(38:38).EQ.'Y'
END DO

CALL F_NEW_DATA
CDTFILE = LINE( 2:15)
READ(LINE(18:24),'(I7 )',ERR=4100,IOSTAT=IOS) IDFILE
IF (LINE(27:27).EQ.'H') IDFILE = IDFILE*3600

CALL F_NEW_DATA
LEN = LEN_TRIM(LINE)
IF (LEN.GT.1) FILE01 = LINE(2:LEN)  !! INTEGRATED PARAMETER FILE (UNFORM. INPUT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. PRINT USER INPUT                                                      !
!        ----------------                                                      !

WRITE(IU06,'(''1'')')
WRITE(IU06,*) ' USER INPUT:'
WRITE(IU06,*) '  '
IF (NOUTT.EQ.0) THEN
   WRITE(IU06,*) ' START  DATE (FORMAT:YYMMDDHHMM) : ', CDATEA,                &
&                                        ' END DATE :', CDATEE
   WRITE(IU06,*) '  '
   WRITE(IU06,*) ' OUTPUT EVERY ',IDELDO ,' SECONDS'
ELSE
   WRITE(IU06,*) ' FIELDS ARE PRINTED AT:'
   WRITE(IU06,'((1X,6(A14,2X)))')  COUTT(1:NOUTT)
END IF
WRITE(IU06,*) '  '
WRITE(IU06,*) ' DATA WILL BE FETCHED FROM:'
WRITE(IU06,*) ' FILE ID IS ..................... ', FILE01
WRITE(IU06,*) ' THE FIRST FILE DATE IS ......... ', CDTFILE
WRITE(IU06,*) ' A NEW FILE WILL BE FETCHED EVERY ', IDFILE, ' SECONDS'
WRITE(IU06,*) '  '
WRITE(IU06,*) ' LIST OF OUTPUTS TO BE GENERATED:'
WRITE(IU06,*) '  '
DO I=1,NPOUT
   IF (CFLAG(I))  WRITE(IU06,'(1X,A50)') TITL(I)
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. CHECK CONSISTENCY OF INPUT DATA                                       !
!        -------------------------------                                       !

IF (NOUTT.EQ.0 .AND. CDATEE.LT.CDATEA) THEN
   WRITE(IU06,*) '*******************************************'
   WRITE(IU06,*) '*                                         *'
   WRITE(IU06,*) '*    FATAL ERROR IN SUB. READ_GRID_USER   *'
   WRITE(IU06,*) '*    ==================================   *'
   WRITE(IU06,*) '* END DATE IS BEFORE START DATE           *'
   WRITE(IU06,*) '* START DATE = ', CDATEA
   WRITE(IU06,*) '* END  DATE  = ', CDATEE
   WRITE(IU06,*) '*                                         *'
   WRITE(IU06,*) '* CORRECT USER INPUT                      *'
   WRITE(IU06,*) '*                                         *'
   WRITE(IU06,*) '* PROGRAM ABORTS.   PROGRAM ABORTS.       *'
   WRITE(IU06,*) '*                                         *'
   WRITE(IU06,*) '*******************************************'
   CALL ABORT1
ENDIF

RETURN

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. READ ERROR MESSAGES                                                   !
!        -------------------                                                   !

 4100 CONTINUE
         WRITE(IU06,*) ' ********************************************'
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' *     FATAL ERROR IN SUB. READ_GRID_USER   *'
         WRITE(IU06,*) ' *     ==================================   *'
         WRITE(IU06,*) ' * READ ERROR ON CHARACTER STRING           *'
         WRITE(IU06,*) ' * CHARACTER STRING IS   LINE = ', LINE
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' *   PROGRAM ABORTS  PROGRAM ABORTS         *'
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' ********************************************'
         CALL ABORT1

! ---------------------------------------------------------------------------- !
!                                                                              !

CONTAINS
   SUBROUTINE F_NEW_DATA   !! FIND A NEW RECORD STARTING WITHOUT 'C'

   IOS = 0
   LINE(1:1) = 'C'
   DO WHILE (LINE(1:1).EQ.'C')
      READ (IU05, '(A)',IOSTAT=IOS) LINE

      IF (IOS.NE.0) THEN
         WRITE(IU06,*) ' ********************************************'
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' *     FATAL ERROR IN SUB. READ_GRID_USER   *'
         WRITE(IU06,*) ' *     ==================================   *'
         WRITE(IU06,*) ' * READ ERROR ON INPUT FILE:                *'
         WRITE(IU06,*) ' * LAST LINE READ IS     LINE = ', LINE
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' *   PROGRAM ABORTS  PROGRAM ABORTS         *'
         WRITE(IU06,*) ' *                                          *'
         WRITE(IU06,*) ' ********************************************'
         CALL ABORT1
      END IF
   END DO

   END SUBROUTINE F_NEW_DATA

END SUBROUTINE READ_GRID_USER


