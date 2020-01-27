SUBROUTINE READ_WAM_USER

! ---------------------------------------------------------------------------- !
!
!   READ_WAM_USER - ROUTINE TO READ THE WAM USER INPUT.
!
!     H. GUNTHER     GKSS/ECMWF     NOVEMBER 1989
!     H. GUNTHER     GKSS           NOVEMBER 2001
!
!*    PURPOSE.
!     --------
!
!       READS USER INPUT CONCERNING PERIOD OF INTEREST,TIMESTEPS,
!       MODEL OPTIONS, AND FILE NAMES ETC 
!       IS DONE TOO.
!
!     METHOD.
!     -------
!
!        USER INFORMATION IS BEING READ WITH THE PRESUMPTIONS THAT:
!         1. EVERY LINE STARTING WITH 'C' IS A COMMENT LINE
!         2. VALUES ARE PUT IN BELOW POSITIONS INDICATED WITH '-'
!            (RIGHT-JUSTIFIED)
!         THE USER INPUT SPECIFICATIONS ARE TRANSFERED TO DIFFERENT MODULES
!         BY CALLS OF SET_XXXX SUBROUTINES.
!
!     REFERENCE.
!     ----------
!
!       NONE.
!
! ---------------------------------------------------------------------------- !
!
!*     EXTERNALS.
!     -----------

USE WAM_GENERAL_MODULE,   ONLY:    &
&       ABORT1                       !! TERMINATE PROCESSING.

USE WAM_OUTPUT_SET_UP_MODULE,    ONLY:    &
&       SET_OUTPUT_FLAGS,          & !! SET OUTPUT FLAGS.
&       SET_OUTPUT_SITES,          & !! SET OUTPUT SITES FOR SPECTRA.
&       SET_OUTPUT_TIMES             !! SET OUTPUT TIMES.

USE WAM_WIND_MODULE,      ONLY:    &
&       SET_WIND_TIMESTEPS           !! SET WIND TIMESTEPS.

USE WAM_NEST_MODULE,  ONLY:    &
&       SET_BOUNDARY_OPTION          !! SET THE BOUNDARY OPTIONS.

USE WAM_TIMOPT_MODULE,    ONLY:    &
&       SET_INTEGRATION_PERIOD,    & !! SET INTEGRATION PERIOD. 
&       SET_INTEGRATION_TIMESTEPS, & !! SET INTEGRATION TIMESTEPS.
&       SET_MODEL_OPTION,          & !! SET MODEL OPTIONS.
&       SET_START_OPTION             !! SET START OPTION.

USE WAM_COLDSTART_MODULE,    ONLY: &
&       SET_C_START_PAR               !! SETS PARAMETER FOR INITIAL SPECTRA.

USE WAM_FILE_MODULE,      ONLY:  &
&       SET_FILE_OPTION,         & !! SETS FILE TIMESTEP AND RESTART OPTION.
&       SET_TEST_OPTION,         & !! SETS TEST OPTION.
&       SET_WIND_FILE,           & !! WIND DATA FILE (FORM. INPUT)
&       SET_B_INPUT_FILE,        & !! BOUNDARY VALUE INPUT FILE IDENTIFIER
&       SET_ICE_FILE,            & !! ICE DATA FILE (FORM. INPUT)
&       SET_CURRENT_FILE,        & !! DEFINE CURRENT DATA FILE NAME.
&       SET_PREPROC_FILE,        & !! GRID DATA FILE (UNFORM. INPUT)
&       SET_RESTART_FILE,        & !! RESTART FILE (UNFORM. INPUT/OUTPUT)
&       SET_B_OUTPUT_FILE,       & !! BOUNDARY DATA FILE (UNFORM. OUTPUT)
&       SET_MAP_FILE,            & !! INTEGRATED DATA FILE (UNFORM. OUTPUT)
&       SET_SPECTRA_FILE           !! SPECTRA DATA FILE (UNFORM. OUTPUT)

! ---------------------------------------------------------------------------- !
!
!     MODULE VARIABLES.
!     -----------------

USE WAM_FILE_MODULE,  ONLY: IU05, FILE05, IU06

! ---------------------------------------------------------------------------- !
!
!     LOCAL VARIABLES.
!     ----------------

IMPLICIT NONE

CHARACTER*14 :: CDATEA  !! START DATE OF RUN.
CHARACTER*14 :: CDATEE  !! END DATE OF RUN.

INTEGER :: IOPTI        !! START OPTION:
                        !! = -1 HOT START.                              
                        !! =  0 WIND INDEPENDENT INITIAL VALUES.                              
                        !! =  1 WIND DEPENDENT INITIAL VALUES AND                             
                        !!      ENERGY EQUAL ZERO IF WINDSPEED IS ZERO                        
                        !! =  2 WIND DEPENDENT INITIAL VALUES AND                             
                        !!      ENERGY COMPUTED FROM GIVEN PARAMETERS IF                      
                        !!      WINDSPEED IS ZERO.                                            

REAL    :: ALFA         !! PHILLIPS' PARAMETER  (NOT USED IF IOPTI = 1)
REAL    :: FM           !! PEAK FREQUENCY (HZ) AND/OR MAXIMUM FREQUENCY
REAL    :: GAMMA        !! OVERSHOOT FACTOR
REAL    :: SA           !! LEFT PEAK WIDTH
REAL    :: SB           !! RIGHT PEAK WIDTH
REAL    :: THETAQ       !! WAVE DIRECTION (DEG) (NOT USED IF IOPTI = 1)
REAL    :: FETCH        !! FETCH IN METRES (IF ZERO THEN 0.5 OF THE                        
                        !! LATITUDE INCREMENT IS USED.)

INTEGER :: ICASE        !! = 1 SPHERICAL,  OTHERWISE CARTESIAN PROPAGATION.
INTEGER :: ISHALLO      !! = 1 DEEP,  OTHERWISE SHALLOW WATER MODEL. 
INTEGER :: IREFRA       !! = 0 REFRACTION IS NOT USED,                                            
                        !! = 1 DEPTH REFRACTION IS USED,                                          
                        !! = 2 DEPTH AND CURRENT REFRACTION IS USED.
INTEGER :: ISBR         !! = 0 WAVE BREAKING IS NOT USED,                                            
                        !! = 1 WAVE BREAKING IS USED,                                          
INTEGER :: IBOUNC       !! = 1 COARSE GRID MODEL.
INTEGER :: IBOUNF       !! = 1 FINE GRID MODEL.
INTEGER :: ITEST        !! TEST OUTPUT UP TO LEVEL.
INTEGER :: IREST        !! = 1 RESTART FILE IS SAVED.

INTEGER :: IDELPRO      !! PROPAGATION TIMESTEP.
INTEGER :: IDELT        !! SOURCE TIME STEP.
INTEGER :: IDELWI       !! TIMESTEP ON INPUT WIND FILE.
INTEGER :: IDELWO       !! OUTPUT WIND TIMESTEP.
INTEGER :: IDELRES      !! TIMESTEP TO SAVE OUTPUT AND RESTART FILES.

INTEGER :: IDELINT      !! TIMESTEP OF INTEGRATED PARAMETER OUTPUT.
INTEGER :: IDELSPT      !! TIMESTEP OF SPECTRA PARAMETER OUTPUT.

INTEGER :: MOUTT
INTEGER :: NOUTT        !! NUMBER OF SPECIFIED OUTPUT TIMES. 
CHARACTER*14, ALLOCATABLE :: COUTT(:)    !! SPECIFIED OUTPUT TIMES.

INTEGER :: MOUTP
INTEGER :: NOUTP        !! NUMBER OF SPECIFIED OUTPUT SITES FOR SPECTRA. 
REAL,         ALLOCATABLE :: OUTLAT(:)   !! LATITUDES OF OUTPUT SITES.
REAL,         ALLOCATABLE :: OUTLONG(:)  !! LONGITUDES OF OUTPUT SITES.
CHARACTER*20, ALLOCATABLE :: NAME (:)    !! NAMES OF OUTPUT SITES.


INTEGER, PARAMETER :: NPOUT=34
LOGICAL :: FFLAG(NPOUT)        !! FILE OUTPUT FLAG.
LOGICAL :: PFLAG(NPOUT)        !! PRINTER OUTPUT FLAG.

LOGICAL      :: LERROR
CHARACTER    :: LINE*80
INTEGER      :: IOS, J, I, IDELMAX, L

INTEGER  :: IASSI        !! ASSIMILATION FLAG
                         !!  = 1  FOR ASSIMILATION
                         !! OTHERWISE NO ASSIMILATION 
REAL     :: DIST         !! RADIUS OF INFLUENCE IN DEGREES
REAL     :: SIGOBS       !! MEASUREMENT SCATTER.
REAL     :: SIGMOD       !! MODEL SCATTER. 
CHARACTER*14 :: CDATAA   !! START DATE OF ASSIMILATION.
CHARACTER*14 :: CDATAE   !! END DATE OF ASSIMILATION.
INTEGER :: IDELASS       !! ASSIMILATION TIMESTEP.

! ---------------------------------------------------------------------------- !
!
!*    1. OPEN USER INPUT FILE.
!        ---------------------

L = LEN_TRIM(FILE05)
IOS = 0
OPEN (UNIT=IU05, FILE=FILE05(1:L), FORM='FORMATTED', STATUS='OLD', IOSTAT=IOS)
IF (IOS.NE.0) THEN
   WRITE (IU06,*) ' ****************************************************'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *     FATAL ERROR IN SUB. READ_WAM_USER            *'
   WRITE (IU06,*) ' *     =================================            *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' * WAM INPUT FILE COULD NOT BE OPENED               *'
   WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
   WRITE (IU06,*) ' *    FILE NAME IS  FILE05 = ', FILE05(1:L)
   WRITE (IU06,*) ' *    UNIT IS         IU05 = ', IU05
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' ****************************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!
!*    2. MODEL PERIOD.
!        -------------

CALL F_NEW_DATA
READ(LINE(2:15),'(A14)',ERR=4100,IOSTAT=IOS) CDATEA
READ(LINE(18:31),'(A14)',ERR=4100,IOSTAT=IOS) CDATEE
CALL SET_INTEGRATION_PERIOD (B=CDATEA, E=CDATEE)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. PRESET OPTIONS.                                                       !
!        ---------------                                                       !

CALL F_NEW_DATA
READ (LINE,'(I8,I9)',ERR=4100,IOSTAT=IOS) IOPTI, ITEST
CALL SET_START_OPTION (IOPTI)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. JONSWAP PARAMETERS AND FETCH.                                         !
!        -----------------------------                                         !

CALL F_NEW_DATA
READ (LINE,'(F11.5,5F12.5)',ERR=4100,IOSTAT=IOS) ALFA, FM, GAMMA, SA, SB, THETAQ

CALL F_NEW_DATA
READ (LINE,'(F11.1)',ERR=4100,IOSTAT=IOS) FETCH
CALL SET_C_START_PAR (ALFA, FM, GAMMA, SA, SB, THETAQ, FETCH)

! ---------------------------------------------------------------------------- !
!
!*    3. MODEL OPTIONS.
!        --------------

CALL F_NEW_DATA
READ(LINE( 2: 8),'(I7)',ERR=4100,IOSTAT=IOS) ICASE
READ(LINE(11:17),'(I7)',ERR=4100,IOSTAT=IOS) ISHALLO
READ(LINE(20:26),'(I7)',ERR=4100,IOSTAT=IOS) IREFRA
READ(LINE(29:35),'(I7)',ERR=4100,IOSTAT=IOS) ISBR
READ(LINE(38:44),'(I7)',ERR=4100,IOSTAT=IOS) IBOUNC
READ(LINE(47:53),'(I7)',ERR=4100,IOSTAT=IOS) IBOUNF
READ(LINE(56:62),'(I7)',ERR=4100,IOSTAT=IOS) ITEST
READ(LINE(65:71),'(I7)',ERR=4100,IOSTAT=IOS) IREST

CALL SET_MODEL_OPTION (CS=ICASE, DS=ISHALLO, R=IREFRA, B=ISBR)
CALL SET_BOUNDARY_OPTION (C=IBOUNC.EQ.1, F=IBOUNF.EQ.1)
CALL SET_TEST_OPTION (TEST=ITEST)

! ---------------------------------------------------------------------------- !
!
!*    4. MODEL TIMESTEPS.
!        ----------------

CALL F_NEW_DATA
READ(LINE( 2: 8),'(I7)',ERR=4100,IOSTAT=IOS) IDELPRO
IF (LINE(10:10).EQ.'M') IDELPRO = IDELPRO*60
IF (LINE(10:10).EQ.'H') IDELPRO = IDELPRO*3600
READ(LINE(13:19),'(I7)',ERR=4100,IOSTAT=IOS) IDELT
IF (LINE(21:21).EQ.'M') IDELT = IDELT*60
IF (LINE(21:21).EQ.'H') IDELT = IDELT*3600
READ(LINE(24:30),'(I7)',ERR=4100,IOSTAT=IOS) IDELWO
IF (LINE(32:32).EQ.'M') IDELWO = IDELWO*60
IF (LINE(32:32).EQ.'H') IDELWO = IDELWO*3600
READ(LINE(35:41),'(I7)',ERR=4100,IOSTAT=IOS) IDELWI
IF (LINE(43:43).EQ.'M') IDELWI = IDELWI*60
IF (LINE(43:43).EQ.'H') IDELWI = IDELWI*3600

CALL SET_INTEGRATION_TIMESTEPS (P=IDELPRO, S=IDELT)
CALL SET_WIND_TIMESTEPS (IN=IDELWI , OUT=IDELWO)

CALL F_NEW_DATA
READ(LINE( 2: 8),'(I7)',ERR=4100,IOSTAT=IOS) IDELINT
IF (LINE(10:10).EQ.'M') IDELINT = IDELINT*60
IF (LINE(10:10).EQ.'H') IDELINT = IDELINT*3600
READ(LINE(13:19),'(I7)',ERR=4100,IOSTAT=IOS) IDELSPT
IF (LINE(21:21).EQ.'M') IDELSPT = IDELSPT*60
IF (LINE(21:21).EQ.'H') IDELSPT = IDELSPT*3600
READ(LINE(24:30),'(I7)',ERR=4100,IOSTAT=IOS) IDELRES
IF (LINE(32:32).EQ.'M') IDELRES = IDELRES*60
IF (LINE(32:32).EQ.'H') IDELRES = IDELRES*3600

CALL SET_OUTPUT_TIMES (INT=IDELINT, SPE=IDELSPT)
CALL SET_FILE_OPTION  (STEP=IDELRES, REST=IREST)

CALL F_NEW_DATA
READ(LINE( 2: 8),'(I7)',ERR=4100,IOSTAT=IOS) MOUTT
IF (MOUTT.GT.0) ALLOCATE (COUTT(1:MOUTT))

NOUTT = 0
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
   WRITE(IU06,*) '+    WARNING ERROR IN SUB. READ_WAM_USER    +'
   WRITE(IU06,*) '+    ===================================    +'
   WRITE(IU06,*) '+ NUMBER OF OUTPUT TIMES IN INPUT EXCEEDS   +'
   WRITE(IU06,*) '+ DIMENSION MOUTT                = ', MOUTT
   WRITE(IU06,*) '+ NUMBER OF TIMES INPUT IS NOUTT = ', NOUTT
   WRITE(IU06,*) '+ PROGRAM WILL IGNORE THE LAST OUTPUT TIMES +'
   WRITE(IU06,*) '+                                           +'
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++'
   NOUTT = MOUTT
END IF

CALL SET_OUTPUT_TIMES (N=NOUTT, TIME=COUTT)

! ---------------------------------------------------------------------------- !
!
!*    5. MODEL OUTPUT SELECTION.
!        -----------------------

PFLAG = .FALSE.
FFLAG = .FALSE.
DO I=1,NPOUT,2
   CALL F_NEW_DATA
   PFLAG(  I) = LINE( 2: 2).EQ.'Y'
   FFLAG(  I) = LINE( 4: 4).EQ.'Y'
   PFLAG(I+1) = LINE(40:40).EQ.'Y'
   FFLAG(I+1) = LINE(42:42).EQ.'Y'
END DO
CALL SET_OUTPUT_FLAGS (PF=PFLAG, FF=FFLAG)

CALL F_NEW_DATA
READ(LINE( 2: 8),'(I7)',ERR=4100,IOSTAT=IOS) MOUTP
IF (MOUTP.GT.0) THEN
   ALLOCATE (OUTLONG(1:MOUTP))
   ALLOCATE (OUTLAT(1:MOUTP))
   ALLOCATE (NAME(1:MOUTP))
END IF

NOUTP = 0
DO
   CALL F_NEW_DATA
   IF (LINE(2:4).EQ.'END') EXIT
   NOUTP = NOUTP + 1
   IF (NOUTP.LE.MOUTP) THEN
      READ(LINE( 2: 9),'(F8.0)',ERR=4100,IOSTAT=IOS) OUTLONG(NOUTP)
      IF (OUTLONG(NOUTP).LT.0.) OUTLONG(NOUTP) = OUTLONG(NOUTP) + 360.
      READ(LINE(12:19),'(F8.0)',ERR=4100,IOSTAT=IOS) OUTLAT(NOUTP)
      NAME(NOUTP) = LINE(22:41)
   END IF
END DO

IF (NOUTP.GT.MOUTP) THEN
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++'
   WRITE(IU06,*) '+                                           +'
   WRITE(IU06,*) '+    WARNING ERROR IN SUB. READ_WAM_USER    +'
   WRITE(IU06,*) '+    ===================================    +'
   WRITE(IU06,*) '+ NUMBER OF OUTPUT SITES IN INPUT EXCEEDS   +'
   WRITE(IU06,*) '+ DIMENSION MOUTP                = ', MOUTP
   WRITE(IU06,*) '+ NUMBER OF SITES INPUT IS NOUTP = ', NOUTP
   WRITE(IU06,*) '+ PROGRAM WILL IGNORE THE LAST OUTPUT SITES +'
   WRITE(IU06,*) '+                                           +'
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++'
   NOUTP = MOUTP
END IF
CALL SET_OUTPUT_SITES (N=NOUTP, LONG=OUTLONG, LAT=OUTLAT, NA=NAME)

! ---------------------------------------------------------------------------- !
!
!*    5. MODEL FILES.
!        ------------

CALL F_NEW_DATA
CALL SET_WIND_FILE (LINE(2:80))      !! WIND DATA FILE (FORM. INPUT)

CALL F_NEW_DATA
CALL SET_B_INPUT_FILE (LINE(2:80))   !! BOUNDARY VALUE INPUT FILE IDENTIFIER

CALL F_NEW_DATA
CALL SET_ICE_FILE (LINE(2:80))       !! ICE DATA FILE (FORM. INPUT)

CALL F_NEW_DATA
CALL SET_PREPROC_FILE (LINE(2:80))   !! GRID DATA FILE (UNFORM. INPUT)

CALL F_NEW_DATA
CALL SET_CURRENT_FILE (LINE(2:80))   !! CURRENT DATA FILE NAME

CALL F_NEW_DATA
CALL SET_RESTART_FILE (LINE(2:80))   !! RESTART FILE (UNFORM. INPUT/OUTPUT)

CALL F_NEW_DATA
CALL SET_B_OUTPUT_FILE (LINE(2:80))  !! BOUNDARY DATA FILE (UNFORM. OUTPUT)

CALL F_NEW_DATA
CALL SET_MAP_FILE (LINE(2:80))       !! INTEGRATED DATA FILE (UNFORM. OUTPUT)

CALL F_NEW_DATA
CALL SET_SPECTRA_FILE (LINE(2:80))   !! SPECTRA DATA FILE (UNFORM. OUTPUT)

! ---------------------------------------------------------------------------- !
!
!*    6. CLOSE INPUT FILE AND RETURN.
!        ----------------------------

CLOSE (UNIT=IU05, STATUS="KEEP")

RETURN

! ---------------------------------------------------------------------------- !
!
!*    7. READ ERROR MESSAGES.
!        --------------------

 4100 CONTINUE
   WRITE(IU06,*) '*******************************************'
   WRITE(IU06,*) '*                                         *'
   WRITE(IU06,*) '*     FATAL ERROR IN SUB. READ_WAM_USER   *'
   WRITE(IU06,*) '*     =================================   *'
   WRITE(IU06,*) '*                                         *'
   WRITE(IU06,*) '* READ ERROR ON CHARACTER STRING          *'
   WRITE(IU06,*) '* CHARACTER STRING IS   LINE = ', LINE
   WRITE(IU06,*) '*                                         *'
   WRITE(IU06,*) '*   PROGRAM ABORTS  PROGRAM ABORTS        *'
   WRITE(IU06,*) '*                                         *'
   WRITE(IU06,*) '*******************************************'
   CALL ABORT1

! ---------------------------------------------------------------------------- !
!
!*    8. INCLUDED FUNCTIONS.
!        -------------------

CONTAINS

   SUBROUTINE F_NEW_DATA        !! FIND A NEW RECORD STARTING WITHOUT 'C'

   LINE(1:1) = 'C'
   DO WHILE (LINE(1:1).EQ.'C')
      READ (IU05, '(A)',IOSTAT=IOS) LINE

      IF (IOS.EQ.0) CYCLE
      WRITE(IU06,*) ' ***********************************************'
      WRITE(IU06,*) ' *                                             *'
      WRITE(IU06,*) ' *     FATAL ERROR IN SUB. READ_WAM_USER       *'
      WRITE(IU06,*) ' *     ====================================    *'
      WRITE(IU06,*) ' * READ ERROR ON INPUT FILE:                   *'
      WRITE(IU06,*) ' * LAST LINE READ IS     LINE = ', LINE
      WRITE(IU06,*) ' * ERROR NO. IS        IOSTAT = ', IOS
      WRITE(IU06,*) ' *                                             *'
      WRITE(IU06,*) ' *      PROGRAM ABORTS  PROGRAM ABORTS         *'
      WRITE(IU06,*) ' *                                             *'
      WRITE(IU06,*) ' ***********************************************'
      CALL ABORT1

   END DO

   END SUBROUTINE F_NEW_DATA

END SUBROUTINE READ_WAM_USER
