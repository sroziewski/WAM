SUBROUTINE READ_PREPROC_USER

! ---------------------------------------------------------------------------- !
!                                                                              !
!   READ_PREPROC_USER - ROUTINE TO READ USER INPUT FOR PREPROC.                !
!                                                                              !
!     H.GUNTHER            ECMWF       04/04/1990                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       TO READ USER INPUT OF PROGRAM PREPROC AND TRANSFER INFORMATION         !
!       TO MODULES.                                                            !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       FILE05 IS ASSIGNED TO IU05, DATA ARE READ AND TRANSFERED TO MODULE     !
!       BY CALLS OF SET_XXX SUBROUTINES.                                       !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !

USE WAM_GENERAL_MODULE,  ONLY:  &
&       ABORT1,                 &  !! TERMINATES PROCESSING.
&       ADJUST                     !! CORRECTS LONGITUDE INPUT.

USE WAM_FRE_DIR_MODULE,  ONLY:  &
&       SET_FRE_DIR                !! DEFINE FREQUENCY DIRECTION GRID.

USE WAM_NEST_MODULE, ONLY:          &
&       SET_BOUNDARY_OPTION,    &  !! DEFINES BOUNDARY OPTION
&       SET_NEST                   !! DEFINES A NEST.

USE WAM_GRID_MODULE,     ONLY:  &
&       SET_GRID_DEF,           &  !! DEFINE GRID MODEL GRID AREA.
&       SET_GRID_CORRECTIONS,   &  !! TRANSFER DEPTH CORRECTIONS.
&       SET_HEADER                 !! DEFINE MODEL HEADER.

USE WAM_FILE_MODULE,     ONLY:  &
&       SET_TEST_OPTION,        &  !! DEFINE TEST OPTION.
&       SET_TOPO_FILE,          &  !! DEFINE DEPTH DATA FILE NAME.
&       SET_PREPROC_FILE,       &  !! DEFINE PREPROC OUTPUT FILE NAME.
&       SET_C_PREPROC_FILE         !! DEFINE COARSE GRID PREPROC FILE NAME.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     MODULE VARIABLES.                                                        !
!     -----------------                                                        !

USE WAM_FILE_MODULE,   ONLY: IU06, IU05, FILE05

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER            :: KL                !! NUMBER OF DIRECTIONS.
INTEGER            :: ML                !! NUMBER OF FREQUENCIES.
REAL               :: FR1               !! FIRST FREQUENCY [HZ].

INTEGER            :: NX                !! NUMBER OF LONGITUDES.
INTEGER            :: NY                !! NUMBER OF LATITUDES.
REAL               :: XDELLA            !! LONGITUDE INCREMENT [DEG].
REAL               :: XDELLO            !! LATITUDE  INCREMENT [DEG].
REAL               :: AMOSOP            !! SOUTH LATITUDE [DEG].
REAL               :: AMONOP            !! NORTH LATITUDE [DEG].
REAL               :: AMOWEP            !! WEST LONGITUDE [DEG].
REAL               :: AMOEAP            !! EAST LONGITUDE [DEG].

INTEGER, PARAMETER :: N_NEST = 20
REAL               :: AMOSOC(N_NEST)    !! SOUTH LATITUDE OF NEST [DEG].
REAL               :: AMONOC(N_NEST)    !! NORTH LATITUDE OF NEST [DEG].
REAL               :: AMOWEC(N_NEST)    !! WEST LONGITUDE OF NEST [DEG].
REAL               :: AMOEAC(N_NEST)    !! EAST LONGITUDE OF NEST [DEG].
CHARACTER*20       :: NEST_NAME(N_NEST)

INTEGER            :: ITEST             !! TEST OUTPUT OPTION.

INTEGER, PARAMETER :: IOUTA = 100       !! MAX. NO. OF AREAS (DIMENSION)
INTEGER            :: NOUT  =  0        !! NO. OF DEPTH CORRECTION AREAS.
REAL               :: XOUTS(IOUTA)      !! S - LATITUDE OF AREA [DEG].
REAL               :: XOUTN(IOUTA)      !! N - LATITUDE OF AREA [DEG].
REAL               :: XOUTW(IOUTA)      !! W - LONGITUIDE OF AREA [DEG].
REAL               :: XOUTE(IOUTA)      !! E - LONGITUIDE OF AREA [DEG].
INTEGER            :: NOUTD(IOUTA)      !! DEPTH OF AREA [M].

CHARACTER          :: LINE*80
INTEGER            :: L, IOS

! ---------------------------------------------------------------------------- !
!                                                                              !
!     0. OPEN FILE AND READ HEADER.                                            !
!        --------------------------                                            !

L = LEN_TRIM(FILE05)
IOS = 0
OPEN (UNIT=IU05, FILE=FILE05(1:L), FORM='FORMATTED', STATUS='OLD', IOSTAT=IOS)
IF (IOS.NE.0) THEN
   WRITE (IU06,*) ' ****************************************************'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *     FATAL ERROR IN SUB. READ_PREPROC_USER        *'
   WRITE (IU06,*) ' *     =====================================        *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' * PREPROC INPUT FILE COULD NOT BE OPENED           *'
   WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
   WRITE (IU06,*) ' *    FILE NAME IS  FILE05 = ', FILE05(1:L)
   WRITE (IU06,*) ' *    UNIT IS         IU05 = ', IU05
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' ****************************************************'
   CALL ABORT1
END IF

CALL F_NEW_DATA
CALL SET_HEADER (LINE)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. FREQUENCY AND DIRECTION GRID DEFINITIONS.                             !
!        -----------------------------------------                             !

CALL F_NEW_DATA
READ (LINE,'(1X,I5,1X,I5,1X,F10.8)',IOSTAT=IOS) ML, KL, FR1
IF (IOS.NE.0) CALL ERROR_MESSAGE

CALL SET_FRE_DIR (N_DIR=KL, N_FRE=ML, FR1=FR1)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. OUTPUT GRID DEFINITIONS.                                              !
!        ------------------------                                              !

CALL F_NEW_DATA
READ (LINE,'(6(1X,F10.3))',IOSTAT=IOS) XDELLA, XDELLO,                &
&                                               AMOSOP, AMONOP, AMOWEP, AMOEAP
IF (IOS.NE.0) CALL ERROR_MESSAGE
CALL ADJUST (AMOWEP, AMOEAP)
IF (XDELLO.NE.0.) THEN
   NX = NINT((AMOEAP-AMOWEP)/XDELLO+1.0)       !! NO. OF LONGITUDES
ELSE
   NX = 1
END IF
IF (XDELLA.NE.0.) THEN
   NY = NINT((AMONOP-AMOSOP)/XDELLA+1.0)       !! NO. OF LATITUDES
ELSE
   NY = 1
END IF

CALL SET_GRID_DEF (N_LON=NX, N_LAT=NY, D_LON=XDELLO, D_LAT=XDELLA,             &
&                  SOUTH=AMOSOP, NORTH=AMONOP, WEST=AMOWEP, EAST=AMOEAP)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. OUTPUT GRID CORRECTIONS.                                              !
!        ------------------------                                              !

NOUT = 0
DO
   CALL F_NEW_DATA
   IF (LINE(2:4).EQ.'END') EXIT
   NOUT=NOUT+1
   IF (NOUT.LE.IOUTA) THEN
      READ (LINE,'(4(1X,F10.3),1X,I10)',IOSTAT=IOS)                   &
&            XOUTS(NOUT), XOUTN(NOUT), XOUTW(NOUT), XOUTE(NOUT), NOUTD(NOUT)
      IF (IOS.NE.0) CALL ERROR_MESSAGE
   END IF
END DO

IF (NOUT.GT.IOUTA) THEN
   WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
   WRITE (IU06,*) ' +                                                  +'
   WRITE (IU06,*) ' +     WARNING ERROR IN SUB. READ_PREPROC_USER      +'
   WRITE (IU06,*) ' +     =======================================      +'
   WRITE (IU06,*) ' +                                                  +'
   WRITE (IU06,*) ' + NUMBER OF AREAS TO BE CORRECTED EXCEEDS          +'
   WRITE (IU06,*) ' + DIMENSION IOUTA = ', IOUTA
   WRITE (IU06,*) ' + IOUTA IS DEFINED IN SUB. READ_PREPROC_USER       +'
   WRITE (IU06,*) ' +                                                  +'
   WRITE (IU06,*) ' + THE FIRST IOUTA AREAS ARE ONLY USED.             +'
   WRITE (IU06,*) ' +                                                  +'
   WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
   NOUT = IOUTA
END IF

IF (NOUT.GT.0) THEN
   CALL SET_GRID_CORRECTIONS (N_COR=NOUT, SOUTH=XOUTS, NORTH=XOUTN,            &
&                             WEST=XOUTW, EAST=XOUTE,  D_COR=NOUTD)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. TEST OPTION.                                                          !
!        ------------                                                          !

CALL F_NEW_DATA
READ (LINE,'(1X,I8)',IOSTAT=IOS) ITEST
IF (IOS.NE.0) CALL ERROR_MESSAGE

CALL SET_TEST_OPTION (TEST=ITEST)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. NESTED GRID INFORMATION.                                              !
!        ------------------------                                              !

!     5.1 COARSE GRID OPTION.                                                  !

NOUT = 0
DO
   CALL F_NEW_DATA
   IF (LINE(2:4).EQ.'END') EXIT
   NOUT = NOUT+1
   IF (NOUT.LE.N_NEST) THEN
      READ (LINE,'(4F11.3,1X,A20)', IOSTAT=IOS) AMOSOC(NOUT), AMONOC(NOUT),    &
&                                  AMOWEC(NOUT), AMOEAC(NOUT), NEST_NAME(NOUT)
      IF (IOS.NE.0) CALL ERROR_MESSAGE
   END IF
END DO
IF (NOUT.GT.N_NEST) THEN
   WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
   WRITE (IU06,*) ' +                                                  +'
   WRITE (IU06,*) ' +     WARNING ERROR IN SUB. READ_PREPROC_USER      +'
   WRITE (IU06,*) ' +     =======================================      +'
   WRITE (IU06,*) ' +                                                  +'
   WRITE (IU06,*) ' + NUMBER OF NESTS EXCEEDS DIMENSION N_NEST = ', N_NEST
   WRITE (IU06,*) ' + N_NEST IS DEFINED IN SUB. READ_PREPROC_USER       +'
   WRITE (IU06,*) ' +                                                  +'
   WRITE (IU06,*) ' + THE FIRST N_NEST AREAS ARE ONLY USED.             +'
   WRITE (IU06,*) ' +                                                  +'
   WRITE (IU06,*) ' ++++++++++++++++++++++++++++++++++++++++++++++++++++'
   NOUT = N_NEST
END IF

IF (NOUT.GT.0) THEN
   CALL SET_BOUNDARY_OPTION (C=.TRUE.)
   CALL SET_NEST (NUMBER=NOUT,SOUTH=AMOSOC, NORTH=AMONOC, WEST=AMOWEC,          &
&                 EAST=AMOEAC, NAME=NEST_NAME)
ELSE
   CALL SET_BOUNDARY_OPTION (C=.FALSE.)
END IF

CALL F_NEW_DATA
L = LEN_TRIM(LINE)
IF (L.GT.1) THEN
   CALL SET_BOUNDARY_OPTION (F=.TRUE.)
   CALL SET_C_PREPROC_FILE (LINE(2:L)) !! COARSE PREPROC FILE NAME
ELSE
   CALL SET_BOUNDARY_OPTION (F=.FALSE.)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. INPUT AND OUTPUT FILE NAMES.                                          !
!        ----------------------------                                          !

CALL F_NEW_DATA
L = LEN_TRIM(LINE)
IF (L.GT.1) CALL SET_TOPO_FILE (LINE(2:L))     !! DEPTH DATA FILE NAME

CALL F_NEW_DATA
L = LEN_TRIM(LINE)
IF (L.GT.1) CALL SET_PREPROC_FILE (LINE(2:L))  !! PREPROC OUTPUT FILE NAME

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. RETURN.                                                               !
!        -------                                                               !

CLOSE (UNIT=IU05)

RETURN

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. INCLUDED FUNCTIONS.                                                   !
!        -------------------                                                   !

CONTAINS

   SUBROUTINE F_NEW_DATA      !! FIND A NEW RECORD STARTING WITHOUT 'C'

   IOS = 0
   LINE(1:1) = 'C'
   DO WHILE (LINE(1:1).EQ.'C')
      READ (IU05, '(A)',IOSTAT=IOS) LINE
      IF (IOS.NE.0) THEN
         WRITE (IU06,*) ' ****************************************************'
         WRITE (IU06,*) ' *                                                  *'
         WRITE (IU06,*) ' *     FATAL ERROR IN SUB. READ_PREPROC_USER        *'
         WRITE (IU06,*) ' *     =====================================        *'
         WRITE (IU06,*) ' *                                                  *'
         WRITE (IU06,*) ' * READ ERROR ON INPUT FILE:                        *'
         WRITE (IU06,*) ' * LAST LINE READ IS     LINE = ', LINE
         WRITE (IU06,*) ' * ERROR NO. IS        IOSTAT = ', IOS
         WRITE (IU06,*) ' *                                                  *'
         WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
         WRITE (IU06,*) ' *                                                  *'
         WRITE (IU06,*) ' ****************************************************'
         CALL ABORT1
      END IF
   END DO

   END SUBROUTINE F_NEW_DATA

   SUBROUTINE ERROR_MESSAGE
      WRITE(IU06,*) ' *********************************************'
      WRITE(IU06,*) ' *                                           *'
      WRITE(IU06,*) ' *   FATAL ERROR IN SUB. READ_PREPROC_USER   *'
      WRITE(IU06,*) ' *   =====================================   *'
      WRITE(IU06,*) ' *  READ ERROR ON CHARACTER STRING           *'
      WRITE(IU06,*) ' *                     IOSTAT = ', IOS
      WRITE(IU06,*) ' *  CHARACTER STRING IS  LINE = ', LINE
      WRITE(IU06,*) ' *                                           *'
      WRITE(IU06,*) ' *    PROGRAM ABORTS  PROGRAM ABORTS         *'
      WRITE(IU06,*) ' *                                           *'
      WRITE(IU06,*) '*********************************************'
      CALL ABORT1
   END SUBROUTINE ERROR_MESSAGE

END SUBROUTINE READ_PREPROC_USER
