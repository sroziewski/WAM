MODULE WAM_GENERAL_MODULE

! ---------------------------------------------------------------------------- !
!          THIS MODULE COLLECTS ALL GLOBAL CONSTANTS AND GENERAL SUBROUTINES   !
!                       USED IN THE WAM MODEL PROGRAMS                         !
!                                                                              !
!    JUNE 2005:                                                                !
!       CHARNOCK CONSTANT CHANGED FROM ALPHA = 0.0100 TO ALPHA = 0.0095        !
!       (Jean Bidlot, Peter Janssen and Saleh Abdalla: A revised formulation   !
!       for ocean wave dissipation in CY29R1. ECMWF MEMORANDUM RESEARCH        !
!       DEPARTMENT:April 7, 2005 File: R60.9/JB/0516                           !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_FILE_MODULE, ONLY: IU06

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!
!    1. WAM MODEL GLOBAL CONSTANTS.

REAL, PARAMETER :: G = 9.806         !! ACCELERATION OF GRAVITY [M/S**2]
REAL, PARAMETER :: PI = 3.1415927    !! PI.
REAL, PARAMETER :: ZPI = 2.*PI       !! 2.* PI.
REAL, PARAMETER :: DEG = 180./PI     !! COVERTION FROM RADIANS TO DEGREE
REAL, PARAMETER :: RAD = PI/180.     !! COVERTION FROM DEGREE TO RADIANS 
REAL, PARAMETER :: CIRC = 40000000.  !! EARTH CIRCUMFERENCE [M].
REAL, PARAMETER :: R = CIRC/ZPI      !! EARTH RADIUS [M].
REAL, PARAMETER :: EPS = 360.*EPSILON(360.) !! ERROR LIMIT FOR DEGREES.

! ---------------------------------------------------------------------------- !
!
!    2. PARAMETERS FOR COUPLING.
!       ------------------------

REAL, PARAMETER :: ROAIR = 1.225        !! AIR DENSITY
REAL, PARAMETER :: ROWATER = 1000.      !! WATER DENSITY
REAL, PARAMETER :: XEPS = ROAIR/ROWATER
REAL, PARAMETER :: XINVEPS = 1./XEPS
REAL, PARAMETER :: BETAMAX = 1.20       !! PARAMETER FOR WIND INPUT.
REAL, PARAMETER :: ZALP    = 0.0110     !! SHIFTS GROWTH CURVE.
REAL, PARAMETER :: ALPHA   = 0.0095     !! CHARNOCK CONSTANT.
REAL, PARAMETER :: XKAPPA  = 0.40       !! VON KARMAN CONSTANT.
REAL, PARAMETER :: XNLEV   = 10.0       !! WINDSPEED REF. LEVEL 

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE ABORT1                        !! TERMINATE PROCESSING.
   MODULE PROCEDURE ABORT1
END INTERFACE

INTERFACE ADJUST                        !! CORRECT BORDERS OF INTERVALS.
   MODULE PROCEDURE ADJUST_S            !! SCALAR 
   MODULE PROCEDURE ADJUST_V            !! VECTOR. 
END INTERFACE

INTERFACE DIFDATE                       !! COMPUTE TIME DIFFERENCE.
   MODULE  PROCEDURE DIFDATE
END INTERFACE

INTERFACE CHECK_MULTIPLE                !! CHECKS INTEGER MULTIPLE
   MODULE  PROCEDURE CHECK_MULTIPLE
END INTERFACE
PUBLIC :: CHECK_MULTIPLE

INTERFACE INCDATE                       !! UPDATE DATE TIME GROUP.
   MODULE  PROCEDURE INCDATE
END INTERFACE

INTERFACE MAKE_BOX                       !! MAKE BOX IN A GRID.  
   MODULE  PROCEDURE MAKE_BOX
END INTERFACE
PUBLIC MAKE_BOX

INTERFACE OPEN_FILE                     !! OPEN A FILE.
   MODULE  PROCEDURE OPEN_FILE
END INTERFACE

INTERFACE PRINT_ARRAY                   !! FORMATED OUTPUT OF AN ARRAY. 
   MODULE  PROCEDURE PRINT_ARRAY_C
   MODULE  PROCEDURE PRINT_ARRAY_R
END INTERFACE

INTERFACE PRINT_SPECTRUM                !! PRINTS A SPECTRUM. 
   MODULE  PROCEDURE PRINT_SPECTRUM
END INTERFACE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     B.  EXPLICIT INTERFACES.                                                 !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE
      
! ---------------------------------------------------------------------------- !

   SUBROUTINE INITMDL                    !! INITIALIZES THE WAM MODEL.
   END SUBROUTINE INITMDL

! ---------------------------------------------------------------------------- !

   SUBROUTINE PRINT_PREPROC_OUTPUT  !! PRINT PREPROC_OUTPUT.
   END SUBROUTINE PRINT_PREPROC_OUTPUT 
      
! ---------------------------------------------------------------------------- !

   SUBROUTINE READ_BOUNDARY_INPUT     !! READ BOUNDARY VALUE INPUT FILE.
   END SUBROUTINE READ_BOUNDARY_INPUT
   
! ---------------------------------------------------------------------------- !

   SUBROUTINE READ_CURRENT            !! READS CURRENT INPUT FILE.
   END SUBROUTINE READ_CURRENT

! ---------------------------------------------------------------------------- !

   SUBROUTINE READ_ICE_INPUT           !! READ AN ICE MAP.
   END SUBROUTINE READ_ICE_INPUT

! ---------------------------------------------------------------------------- !

   SUBROUTINE READ_PREPROC_USER        !! READ USER INPUT FOR PREPROC.
   END SUBROUTINE READ_PREPROC_USER
   
! ---------------------------------------------------------------------------- !

   SUBROUTINE READ_TOPOGRAPHY           !! READ TOPOGRAPHY INPUT FILE
   END SUBROUTINE READ_TOPOGRAPHY

! ---------------------------------------------------------------------------- !

   SUBROUTINE READ_WAM_USER              !! READ USER INPUT FOR WAM.
   END SUBROUTINE READ_WAM_USER
   
! ---------------------------------------------------------------------------- !

   SUBROUTINE READ_WIND_INPUT            !! READ A WIND FIELD
   END SUBROUTINE READ_WIND_INPUT

! ---------------------------------------------------------------------------- !

   SUBROUTINE WAMODEL                !! TIME INTEGRATION OF WAVE FIELDS.
   END SUBROUTINE WAMODEL
   
! ---------------------------------------------------------------------------- !

   SUBROUTINE WAVEMDL          !! SUPERVISES EXECUTION OF THE WAVE MODEL.
   END SUBROUTINE WAVEMDL
   
END INTERFACE 

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE ABORT1

! ---------------------------------------------------------------------------- !
!                                                                              !
!   ABORT1 - STOP PROCESSING                                                   !
!                                                                              !
!     H. GUNTHER     GKSS    SEPTEMBER 2000                                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!     CLOSE ALL INPUT AND OUTPUT UNITS AND STOP                                !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!      NONE                                                                    !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
! 
!     LOCAL VARIABLES.
!     ----------------

LOGICAL :: DA
INTEGER :: I

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1.0 CHECK FILE STATUS AND CLOSE FILE IF FILE IS ASSIGNED.                !
!         ------------------------------------------------------               !

DO I=1,99
   INQUIRE (UNIT=I, EXIST=DA)
   IF (DA) CLOSE (UNIT=I)
END DO

STOP
END SUBROUTINE ABORT1

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE ADJUST_S (WEST, EAST)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   ADJUST_S - ROUTINE TO CORRECT BORDERS OF INTERVALS (SCALAR VERSION).       !
!                                                                              !
!     H.GUNTHER            ECMWF       04/04/1990                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       ADJUSTS INTERVAL BORDERS GIVEN IN DEGREE.                              !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE INTERVAL BORDERS ARE CHANGED TO FULLFILL:                          !
!         0. .LE. EAST  .AND. EAST .LT. 360. .AND. WEST .LE. EAST              !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
! 
!     INTERFACE VARIABLES.
!     --------------------

REAL, INTENT(INOUT) :: WEST  !! LEFT INTERVAL BORDER IN DEGREE.
REAL, INTENT(INOUT) :: EAST  !! RIGHT INTERVAL BORDER IN DEGREE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!  1. CORRECT BORDERS.                                                         !
!     ----------------                                                         !

IF (WEST.LT.0.) THEN
   WEST = WEST+360.
   IF (WEST.LT.0.) WEST = WEST+360.
END IF
IF (WEST.GE.360.) THEN
   WEST = WEST-360.
   IF (WEST.GE.360.) WEST = WEST-360.
END IF
IF (EAST.LT.0.) THEN
   EAST = EAST+360.
   IF (EAST.LT.0.) EAST = EAST+360.
END IF
IF (EAST.GE.360.) THEN
   EAST = EAST-360.
   IF (EAST.GE.360.) EAST = EAST-360.
END IF
IF (WEST.GT.EAST) WEST = WEST-360.

END SUBROUTINE ADJUST_S

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE ADJUST_V (WEST, EAST)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   ADJUST_V - ROUTINE TO CORRECT BORDERS OF INTERVALS (VECTOR VERSION).       !
!                                                                              !
!     H.GUNTHER            ECMWF       04/04/1990                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       ADJUSTS INTERVAL BORDERS GIVEN IN DEGREE.                              !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE INTERVAL BORDERS ARE CHANGED TO FULLFILL:                          !
!         0. .LE. EAST  .AND. EAST .LT. 360. .AND. WEST .LE. EAST              !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
! 
!     INTERFACE VARIABLES.
!     --------------------

REAL, INTENT(INOUT) :: WEST(:)  !! LEFT INTERVAL BORDER IN DEGREE.
REAL, INTENT(INOUT) :: EAST(:)  !! RIGHT INTERVAL BORDER IN DEGREE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!  1. CORRECT BORDERS.                                                         !
!     ----------------                                                         !

WHERE (WEST.LT.0.) WEST = WEST+360.
WHERE (WEST.LT.0.) WEST = WEST+360.
WHERE (WEST.GE.360.) WEST = WEST-360.
WHERE (WEST.GE.360.) WEST = WEST-360.

WHERE (EAST.LT.0.) EAST = EAST+360.
WHERE (EAST.LT.0.) EAST = EAST+360.
WHERE (EAST.GE.360.) EAST = EAST-360.
WHERE (EAST.GE.360.) EAST = EAST-360.

WHERE (WEST.GT.EAST) WEST = WEST-360.

END SUBROUTINE ADJUST_V

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE DIFDATE (CDATE1, CDATE2, ISHIFT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   DIFDATE - TO COMPUTE TIME DIFFERENCE.                                      !
!                                                                              !
!     H. GUNTHER   GKSS/ECMWF  NOVEMBER 1989                                   !
!     H. GUNTHER   GKSS   NOVEMBER 1999    FT90 AND CENTURY AND SECONDS.       !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTE THE SECONDS BETWEEN THE INPUT DATES.                           !
!       DATES HAVE TO BE IN CONSECUTIVE YEARS.                                 !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
!      REFERENCES.                                                             !
!      -----------                                                             !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
! 
!    INTERFACE VARIABLES.
!    --------------------

CHARACTER (LEN=14), INTENT(IN)  :: CDATE1 !! DATE TIME GROUP (YYYYMMDDHHMMSS).
CHARACTER (LEN=14), INTENT(IN)  :: CDATE2 !! DATE TIME GROUP (YYYYMMDDHHMMSS).
INTEGER,            INTENT(OUT) :: ISHIFT !! DIFFERENCE IN SECONDS 
                                          !! (CDATE2-CDATE1).

! ---------------------------------------------------------------------------- !
! 
!    LOCAL VARIABLES.
!    ----------------

INTEGER, SAVE      ::  MON(12) =(/31,28,31,30,31,30,31,31,30,31,30,31/)

INTEGER            ::  YEAR1, MONTH1, DAY1, HOUR1, MINUTE1, SECOND1
INTEGER            ::  YEAR2, MONTH2, DAY2, HOUR2, MINUTE2, SECOND2

INTEGER            :: M, MDAY
CHARACTER (LEN=14) :: CDT1, CDT2
LOGICAL            :: SWITCH

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1.0 CHANGE DATE TIME GROUPS TO ENSURE THAT THE SECOND IS LARGER.         !
!         ------------------------------------------------------------         !

IF (CDATE1 .GT. CDATE2) THEN
   CDT1 = CDATE2
   CDT2 = CDATE1
   SWITCH = .TRUE.
ELSE IF (CDATE2 .GT. CDATE1) THEN
   CDT1 = CDATE1
   CDT2 = CDATE2
   SWITCH = .FALSE.
ELSE
   ISHIFT = 0
   RETURN
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2.0 SPLITE DATE TIME GROUP INTO SECONDS, MINUTE, HOUR, DAY, MONTH, YEAR. !
!         -------------------------------------------------------------------- !

READ (CDT1,'(I4,5I2)') YEAR1, MONTH1, DAY1, HOUR1, MINUTE1, SECOND1
READ (CDT2,'(I4,5I2)') YEAR2, MONTH2, DAY2, HOUR2, MINUTE2, SECOND2


! ---------------------------------------------------------------------------- !
!
!     3.0 COMPUTE DIFFERENCE BETWEEN DAY, HOUR ,MINITE AND SECOND.
!         --------------------------------------------------------

ISHIFT = (((DAY2-DAY1)*24+HOUR2-HOUR1)*60+MINUTE2-MINUTE1)*60+SECOND2-SECOND1

! ---------------------------------------------------------------------------- !
! 
!   4.0 ADD DIFFERENCE FROM MONTH.
!       --------------------------

IF (YEAR2.GT.YEAR1) THEN

!   4.1 START AND END MONTH ARE IN DIFFERENT YEARS.

   DO M = MONTH1,12
      MDAY = MON(M)
      IF (M.EQ.2 .AND. MOD(YEAR1,4).EQ.0) MDAY = 29
      ISHIFT =ISHIFT + MDAY*24*3600
   END DO
   DO M = 1,MONTH2-1
      MDAY =MON(M)
      IF (M.EQ.2 .AND. MOD(YEAR2,4).EQ.0) MDAY = 29
      ISHIFT = ISHIFT + MDAY*24*3600
   END DO

ELSE

!   4.2 START AND END MONTH ARE IN THE SAME YEAR.

   DO M = MONTH1,MONTH2-1
      MDAY = MON(M)
      IF (M.EQ.2 .AND. MOD(YEAR1,4).EQ.0) MDAY = 29
      ISHIFT = ISHIFT + MDAY*24*3600
   END DO

END IF

! ---------------------------------------------------------------------------- !
! 
!   5.0 CHANGE SIGN OF DIFFERENCE IF DATES WERE EXCHANGED.
!       --------------------------------------------------

IF (SWITCH) ISHIFT = -ISHIFT

END SUBROUTINE DIFDATE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

FUNCTION CHECK_MULTIPLE (X,Y) RESULT(Z)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   CHECK_MULTIPLE - CHECKS WHETHER Y IS AN INTEGER MULTIPLE OF X.             !
!                                                                              !
!     H. GUNTHER     GKSS      FEBRUARY 2004                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

IMPLICIT NONE

REAL, INTENT(IN) :: X    !!
REAL, INTENT(IN) :: Y    !!
LOGICAL          :: Z    !! .TRUE. IF Y IS AN INTEGER MULTIPLE OF X.

! ---------------------------------------------------------------------------- !

IF (X.NE.0.) THEN
   Z = (ABS(Y-REAL(NINT(Y/X))*X).LE.10.*ABS(Y)*EPSILON(Y))
ELSE
   Z = .FALSE.
END IF

END FUNCTION CHECK_MULTIPLE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE INCDATE (CDATE, ISHIFT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   INCDATE - TO UPDATE DATE TIME GROUP                                        !
!                                                                              !
!     L. BERTOTTI, P.JANSSEN.                                                  !
!                                                                              !
!     H. GUNTHER   ECMWF  NOVEMBER 1989    NEGATIVE INCREMENTS.                !
!     H. GUNTHER   GKSS   NOVEMBER 2001    FT90 AND CENTURY AND SECONDS.       !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       UPDATING DATE TIME GROUP.                                              !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
!     REFERENCES.                                                              !
!     -----------                                                              !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
! 
!     INTERFACE VARIABLES.
!     --------------------

CHARACTER (LEN=14), INTENT(INOUT) :: CDATE  !! DATE TIME GROUP (YYYYMMDDHHMMSS).
INTEGER,            INTENT(IN)    :: ISHIFT !! TIME INCREMENT IN SECONDS.

! ---------------------------------------------------------------------------- !
! 
!     LOCAL VARIABLES.
!     ----------------

INTEGER, SAVE ::  MON(12) =(/31,28,31,30,31,30,31,31,30,31,30,31/)

INTEGER ::  YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
INTEGER ::  DELT, MDAY

! ---------------------------------------------------------------------------- !
! 
!   1.0 RETURN IF TIME INCREMENT IS ZERO.
!       ---------------------------------

DELT = ISHIFT
IF (ABS(DELT).EQ.0) RETURN

! ---------------------------------------------------------------------------- !
!                                                                              !
!*    2.0 SPLITE DATE TIME GROUP INTO SECONDS, MINUTE, HOUR, DAY, MONTH, YEAR. !
!         -------------------------------------------------------------------- !

READ (CDATE,'(I4,5I2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2.0 ADD AND CHECK SECONDS.                                               !
!         ----------------------                                               !
!                                                                              !
!   2.1 IF SECONDS ARE BETWEEN 0 AND 60 RETURN
!       --------------------------------------

SECOND = SECOND + DELT
IF (SECOND.GE.0. .AND. SECOND.LT.60.) THEN
   WRITE(CDATE,'(I4.4,5I2.2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
   RETURN
END IF

!*  2.2 NEW MIMUTES AND SECONDS.
!       ------------------------

DELT = MODULO(SECOND,60)
MINUTE = MINUTE +(SECOND-DELT)/60
SECOND = DELT

! ---------------------------------------------------------------------------- !
! 
!   3.0 CHECK MINUTES.
!       --------------

IF (MINUTE.GE.60) THEN
   HOUR = HOUR + MINUTE/60        !! MINUTES > 59 ==> NEW HOURS.
ELSE IF (MINUTE.LT.0) THEN
   HOUR = HOUR + (MINUTE-59)/60   !! MINUTES < 0  ==> NEW HOURS.
ELSE
   WRITE (CDATE,'(I4.4,5I2.2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
   RETURN                         !! ALL DONE  ==>  RETURN
END IF 
MINUTE = MODULO(MINUTE,60)        !! NEW MINUTES.

! ---------------------------------------------------------------------------- !
! 
!   4.0 CHECK HOURS.
!       ------------

IF (HOUR.GE.24) THEN
   DAY =  DAY + HOUR/24           !! HOURS > 23 ==> NEW DAYS.
ELSE IF (HOUR.LT.0) THEN
   DAY =  DAY + (HOUR-23)/24      !! HOURS < 0  ==> NEW DAYS.
ELSE
   WRITE (CDATE,'(I4.4,5I2.2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
   RETURN                         !! ALL DONE  ==>  RETURN
END IF 
HOUR = MODULO(HOUR,24)            !! NEW HOURS.

! ---------------------------------------------------------------------------- !
! 
!   5.0 CHECK DAYS.
!       ------------
! 
!   5.1 IF DAYS ARE GREATER THAN DAYS OF MONTH. NEW DAY AND MONTH AND YEAR.
!       -------------------------------------------------------------------

MDAY = MON(MONTH)
IF (MONTH.EQ.2 .AND. MOD(YEAR,4).EQ.0) MDAY = 29

DO WHILE (DAY > MDAY)
   DAY =  DAY - MDAY
   MONTH = MONTH+1
   IF (MONTH.GE.13) THEN
      YEAR = YEAR+1
      MONTH = MONTH-12
   END IF
   MDAY = MON(MONTH)
   IF (MONTH.EQ.2 .AND. MOD(YEAR,4).EQ.0) MDAY = 29
END DO

!   5.2 IF DAYS ARE LESS THAN 1. NEW DAY AND MONTH AND YEAR.
!       ----------------------------------------------------

DO WHILE ( DAY < 1)
   MONTH = MONTH-1
   IF (MONTH.EQ.0) THEN
      MONTH = 12
      YEAR = YEAR-1
   END IF
   MDAY = MON(MONTH)
   IF(MONTH.EQ.2 .AND. MOD(YEAR,4).EQ.0) MDAY = 29
   DAY = DAY + MDAY
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6.0 COMPOSE NEW DATE TIME GROUP.                                         !
!         ----------------------------                                         !

WRITE (CDATE,'(I4.4,5I2.2)') YEAR, MONTH, DAY, HOUR, MINUTE, SECOND

END SUBROUTINE INCDATE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE MAKE_BOX (N_POINT, WEST, SOUTH, EAST, NORTH, D_LAT, D_LON,          &
&                    LATITUDE, LONGITUDE)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   MAKE_BOX - MAKE BOX IN A GRID.                                             !
!                                                                              !
!     R. PORTZ     MPI         15/01/1991                                      !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       COMPUTE LATITUDES AND LONGITUDES OF ALL GRID POINTS AT A BOX BOUNDARY. !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE BOX BOUNDARY IS SCANNED ALONG LATITUDES FROM WEST TO EAST          !
!       MOVING FROM SOUTH TO NORTH.                                            !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

IMPLICIT NONE

INTEGER, INTENT(IN)  :: N_POINT      !! NUMBER OF BOUNDARY POINTS.
REAL   , INTENT(IN)  :: WEST         !! WESTERN  LONGITUDE OF BOX.
REAL   , INTENT(IN)  :: SOUTH        !! SOUTHERN LATITUDE  OF BOX.
REAL   , INTENT(IN)  :: EAST         !! EASTERN  LONGITUDE OF BOX.
REAL   , INTENT(IN)  :: NORTH        !! NORTHERN LATITUDE  OF BOX.
REAL   , INTENT(IN)  :: D_LAT        !! LATITUDE INCREMENT  [DEG].
REAL   , INTENT(IN)  :: D_LON        !! LONGITUDE INCREMENT [DEG].
REAL   , INTENT(OUT) :: LATITUDE (N_POINT)  !! LATITUDES  OF BOUNDARY POINTS.
REAL   , INTENT(OUT) :: LONGITUDE(N_POINT)  !! LONGITUDES OF BOUNDARY POINTS.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER  :: NLNGB, NLATB, K, I

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COMPUTED THE SQUARE BOX                                               !
!        -----------------------                                               !

NLNGB =  NINT((EAST - WEST) / D_LON) + 1
NLATB = (NINT((NORTH - SOUTH) / D_LAT) - 1) * 2

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. DIMENSION CHECK: SIZE(LATITUDE) > N_POINT                               !
!        ---------------------------------------                               !

IF ((NLNGB * 2) + NLATB .GT. N_POINT) THEN
   WRITE(IU06,*) ' *************************************************'
   WRITE(IU06,*) ' *                                               *'
   WRITE(IU06,*) ' *         FATAL ERROR IN SUB. MAKE_BOX          *'
   WRITE(IU06,*) ' *         ============================          *'
   WRITE(IU06,*) ' *  NUMBER OF BOUNDARY POINTS EXCEEDS DIMENSION. *'
   WRITE(IU06,*) ' *  DIMENSION IS      N_POINT = ', N_POINT
   WRITE(IU06,*) ' *  NUMBER OF POINTS IS         ', (NLNGB * 2) + NLATB
   WRITE(IU06,*) ' *                                               *'
   WRITE(IU06,*) ' *************************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. COMPUTES THE BOUNDARY POINTS FOR THE FIRST AND THE LAST LATITUDE.     !
!        -----------------------------------------------------------------     !

K = NLATB + NLNGB
DO I = 1, NLNGB
   LATITUDE(I)    = SOUTH
   LONGITUDE(I)   = WEST + REAL(I-1) * D_LON
   LATITUDE(K+I)  = NORTH
   LONGITUDE(K+I) = LONGITUDE(I)
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. COMPUTED THE EAST AND THE WEST BOUNDARY POINT FOR EACH LATITUDE.      !
!        ----------------------------------------------------------------      !

K = 0
DO I = 2,NLATB,2
   K = K + 1
   LATITUDE(NLNGB+I-1)  = SOUTH + REAL(K) * D_LAT
   LONGITUDE(NLNGB+I-1) = WEST
   LATITUDE(NLNGB+I)    = LATITUDE(NLNGB+I-1)
   LONGITUDE(NLNGB+I)   = EAST
END DO

END SUBROUTINE MAKE_BOX

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE OPEN_FILE (IU06, IUNIT, FILEID, CDATE, STAT, IFAIL, FORMA)

! ---------------------------------------------------------------------------- !
!
!   OPEN_FILE - OPEN A FILE.
!
!     HEINZ GUNTHER       ECMWF       OCTOBER 1989
!     HEINZ GUNTHER       ECMWF       OCTOBER 1990  MIGRATION TO YMP
!                                                   NEW FILE NAMES
!     H. GUNTHER    GKSS              NOVEMBER 1999  NEW DATES AND FT90.
!
!     PURPOSE.
!     --------
!
!         INCLUDE DATE AND TIME IN A FILE MANE AND ASSIGN THE FILE TO A UNIT. 
!
!     METHOD.
!     -------
!
!        THE FILENAME IS BUILT FROM THE FILEID FOLLOWED BY 'YYYYMMDDHHMMSS' 
!        WHERE YYYYMMDDHHMMSS IS THE TIME OF THE LAST FIELD STORED IN THE FILE.
!        THE FULL PATH NAME IS  USERID/PATH/RUNID/FILENAME
!
!     REFERENCES.
!     -----------
!
!         NONE
!
! ---------------------------------------------------------------------------- !
! 
!     INTERFACE VARIABLES.
!     --------------------

IMPLICIT NONE

INTEGER,            INTENT(IN)  :: IU06       !! PROTOCOL UNIT
INTEGER,            INTENT(IN)  :: IUNIT      !! FORTRAN UNIT FOR FILE.
CHARACTER (LEN=*),  INTENT(IN)  :: FILEID     !! FILE IDENTIFIER
CHARACTER (LEN=14), INTENT(IN)  :: CDATE      !! DATE (YYYYMMDDHHMMSS) OF FILE.
CHARACTER (LEN=*),  INTENT(IN)  :: STAT       !! FILE STATUS (NEW, OLD, UNKNOWN)
INTEGER,            INTENT(OUT) :: IFAIL      !! ERROR FLAG    = 0 NO ERROR
                                              !!               ­ 0 OPEN ERROR
CHARACTER (LEN=*),  INTENT(IN), OPTIONAL :: FORMA !! FORMATTED OR UNFORMATED

! ---------------------------------------------------------------------------- !
!
!     LOCAL VARIABLES.
!     ----------------

CHARACTER (LEN=255) :: FILENA
INTEGER             :: LEN, LEF
CHARACTER (LEN=11)  :: FORMAH*11

! ---------------------------------------------------------------------------- !
!
!     1. CONSTRUCT FULL FILE NAME.
!        -------------------------

FILENA = ' '
LEF   = LEN_TRIM(FILEID)
LEN = 1
IF (LEF.NE.0) THEN
   FILENA(LEN:LEN+LEF-1) = FILEID(1:LEF)
   LEN = LEN+LEF
END IF
FILENA(LEN:LEN+13) = CDATE
LEN = LEN+13
FORMAH = 'UNFORMATTED'
IF (PRESENT(FORMA)) FORMAH = FORMA

! ---------------------------------------------------------------------------- !
!
!     2. OPEN THE FILE.
!        --------------

IFAIL = 0
OPEN (UNIT=IUNIT, FILE=FILENA(1:LEN), FORM=FORMAH, STATUS=STAT, IOSTAT=IFAIL)
IF (IFAIL.NE.0) THEN
   WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++'
   WRITE(IU06,*) ' +                                           +'
   WRITE(IU06,*) ' +      WARNING ERROR IN --OPEN_FILE--       +'
   WRITE(IU06,*) ' +      ==============================       +'
   WRITE(IU06,*) ' +                                           +'
   WRITE(IU06,*) ' + COULD NOT OPEN FILE                       +'
   WRITE(IU06,*) ' + FILENAME                         : ', FILENA
   WRITE(IU06,*) ' + ERROR CODE FROM OPEN IS IOSTAT = : ', IFAIL
   WRITE(IU06,*) ' +                                           +'
   WRITE(IU06,*) ' +++++++++++++++++++++++++++++++++++++++++++++'
ELSE   
   WRITE(IU06,*) ' SUB. OPEN_FILE: A FILE WAS CONNECTED TO UNIT =', IUNIT,     &
&                ' FILE NAME IS: ',FILENA(1:LEN)
END IF   

END SUBROUTINE OPEN_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_ARRAY_C (IUOUT, CDATE, TITL, ARRAY,                           &
                          AMOWEP, AMOSOP, AMOEAP, AMONOP)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     PRINT_ARRAY - FORMATED OUTPUT OF AN ARRAY. (CHARACTER VERSION)           !
!                                                                              !
!     H. GUNTHER       ECMWF    NOVEMBER 1989                                  !
!     H. GUNTHER       GKSS     FEBRARY  2002   CHANGED TO FT90                !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       FORMATED OUTPUT OF AN ARRAY.                                           !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       A TWO DIMENSIONAL ARRAY IS PRINTED WITH A MAXIMUM OF                   !
!       NPTS COLUMNS PER PAGE.                                                 !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
! 
!*    INTERFACE VARIABLES.
!     --------------------

INTEGER,            INTENT(IN)  :: IUOUT          !! OUTPUT UNIT.
CHARACTER (LEN=14), INTENT(IN)  :: CDATE          !! DATE (YYYYMMDDHHMMSS).
CHARACTER (LEN=*) , INTENT(IN)  :: TITL           !! HEADER TO BE PRINTED.
CHARACTER (LEN=1) , INTENT(IN)  :: ARRAY(:,:)     !! ARRAY TO BE PRINTED.
REAL,               INTENT(IN)  :: AMOWEP         !! WEST LONGITUDE (DEGREE).
REAL,               INTENT(IN)  :: AMOSOP         !! SOUTH LATITUDE (DEGREE).
REAL,               INTENT(IN)  :: AMOEAP         !! EAST LONGITUDE (DEGREE).
REAL,               INTENT(IN)  :: AMONOP         !! NORTH LATITUDE (DEGREE).

! ---------------------------------------------------------------------------- !
!
!*    LOCAL VARIABLES.
!     ----------------


INTEGER    :: NGX                     !! FIRST DIMENSION  USED.
INTEGER    :: NGY                     !! SECOND DIMENSION USED.
INTEGER    :: I, NPAGE, IA, IE, L, LEN, LEN1
REAL       :: DLAMA

INTEGER, PARAMETER :: NPTS = 120

! ---------------------------------------------------------------------------- !
!
!*    1. COMPUTE LATITUDES AND LONGITUDES.
!         --------------------------------

NGX = SIZE(ARRAY,1)
NGY = SIZE(ARRAY,2)
IF (NGX.GT.1) THEN
   DLAMA = (AMOEAP-AMOWEP)/REAL(NGX-1)
ELSE
   DLAMA = 0.
END IF

! ---------------------------------------------------------------------------- !
!
!*    2. PRINT ARRAY.
!        ------------

NPAGE = (NGX+NPTS-1)/NPTS
LEN = LEN_TRIM(TITL)
LEN1 = LEN_TRIM(CDATE)
DO L = 1,NPAGE
   IA = (L-1)*NPTS
   IE = MIN(IA+NPTS,NGX)
   IA = IA + 1
   WRITE (IUOUT,'(''1'',4X,A,2X,A,5X,''PAGE '',I2,/)') CDATE(1:LEN1),          &
&                                                      TITL(1:LEN), L
   WRITE (IUOUT,'(2X,''LONGITUDE IS FROM '',F7.2,'' TO '',F7.2)')              &
&                        AMOWEP+REAL(IA-1)*DLAMA, AMOWEP+REAL(IE-1)*DLAMA
   WRITE (IUOUT,'(2X,''LATITUDE  IS FROM '',F7.2,'' TO '',F7.2)') AMONOP, AMOSOP
   WRITE (IUOUT,*) ' '
   WRITE (IUOUT,'(2X,130I1)') (MOD(I,10),I=IA,IE)
   DO I = NGY,1,-1
      WRITE (IUOUT,'(1X,I1,130A1)') MOD(I,10),ARRAY(IA:IE,I)
   END DO
   WRITE (IUOUT,'(2X,130I1)') (MOD(I,10),I=IA,IE)
END DO

END SUBROUTINE PRINT_ARRAY_C

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_ARRAY_R (IUOUT, CDATE, TITL, ARRAY,                           &
                          AMOWEP, AMOSOP, AMOEAP, AMONOP, SCALE)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     PRINT_ARRAY - FORMATED OUTPUT OF AN ARRAY.                               !
!                                                                              !
!     H. GUNTHER       ECMWF    NOVEMBER 1989                                  !
!     H. GUNTHER       GKSS     FEBRARY  2002   CHANGED TO FT90                !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       FORMATED OUTPUT OF AN ARRAY.                                           !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       A TWO DIMENSIONAL ARRAY IS PRINTED WITH A MAXIMUM OF                   !
!       NPTS COLUMNS PER PAGE.                                                 !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
! 
!*    INTERFACE VARIABLES.
!     --------------------

INTEGER,            INTENT(IN)  :: IUOUT          !! OUTPUT UNIT.
CHARACTER (LEN=14), INTENT(IN)  :: CDATE          !! DATE (YYYYMMDDHHMMSS).
CHARACTER (LEN=*) , INTENT(IN)  :: TITL           !! HEADER TO BE PRINTED.
REAL,               INTENT(IN)  :: ARRAY(:,:)     !! ARRAY TO BE PRINTED.
REAL,               INTENT(IN)  :: AMOWEP         !! WEST LONGITUDE (DEGREE).
REAL,               INTENT(IN)  :: AMOSOP         !! SOUTH LATITUDE (DEGREE).
REAL,               INTENT(IN)  :: AMOEAP         !! EAST LONGITUDE (DEGREE).
REAL,               INTENT(IN)  :: AMONOP         !! NORTH LATITUDE (DEGREE).
REAL, OPTIONAL,     INTENT(IN)  :: SCALE          !! SCALING FACTOR.

! ---------------------------------------------------------------------------- !
!
!*    LOCAL VARIABLES.
!     ----------------


INTEGER    :: NGX                     !! FIRST DIMENSION  USED.
INTEGER    :: NGY                     !! SECOND DIMENSION USED.
INTEGER    :: IARRAY(SIZE(ARRAY,1),SIZE(ARRAY,2)), ILON(SIZE(ARRAY,1))
REAL       :: YLAT(SIZE(ARRAY,2))
INTEGER    :: I, J, NPAGE, ISTART, IEND, NP, LEN, LEN1
REAL       :: DLAMA, DPHIA

INTEGER, PARAMETER :: NPTS = 30

! ---------------------------------------------------------------------------- !
!
!*    1. COMPUTE LATITUDES AND LONGITUDES.
!         --------------------------------

NGX = SIZE(ARRAY,1)
NGY = SIZE(ARRAY,2)
IF (NGX.GT.1) THEN
   DLAMA = (AMOEAP-AMOWEP)/REAL(NGX-1)
ELSE
   DLAMA = 0.
END IF
DO I = 1,NGX
   ILON(I) = NINT(AMOWEP + (I-1)*DLAMA)
END DO
IF (NGX.GT.1) THEN
   DPHIA = (AMONOP-AMOSOP)/REAL(NGY-1)
ELSE
   DPHIA = 0.
END IF
DO J = 1,NGY
   YLAT(J) = AMOSOP + REAL(J-1)*DPHIA
END DO

! ---------------------------------------------------------------------------- !
!
!*    2. SCALE DATA ARRAY.
!        -----------------

IF (PRESENT(SCALE)) THEN
   IARRAY = NINT(SCALE*ARRAY)
ELSE
   IARRAY = NINT(ARRAY)
END IF

! ---------------------------------------------------------------------------- !
!
!*    3. PRINT ARRAY.
!        ------------

NPAGE = (NGX+NPTS-1)/NPTS
LEN  = LEN_TRIM(TITL)
LEN1 = LEN_TRIM(CDATE)

ISTART = -NPTS+1
IEND   = ISTART+NPTS-1
DO NP = 1,NPAGE
   WRITE (IUOUT,'(''1'',4X,A,2X,A,5X,''PAGE '',I2,/)') CDATE(1:LEN1),          &
&                                  TITL(1:LEN), NP
   ISTART = ISTART+NPTS
   IEND   = MIN(IEND+NPTS,NGX)
   WRITE (IUOUT,'(7X,''I='',30I4)') (I,I=ISTART,IEND)
   WRITE (IUOUT,'(5X,''LON='',30I4)') ILON(ISTART:IEND)
   WRITE (IUOUT,'(''   J LAT'',/)')
   DO J = NGY,1,-1
      WRITE (IUOUT,'(1X,I2,F5.1,1X,30I4)') J,YLAT(J),IARRAY(ISTART:IEND,J)
   END DO
END DO

END SUBROUTINE PRINT_ARRAY_R

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_SPECTRUM (IUNIT, CDATE, LONG, LAT, TITL, FR, THETA, SPEC,     &
&                          U10, UDIR, US, HS, PPER, MPER, TM1, TM2, MDIR, SPRE)

! ---------------------------------------------------------------------------- !
! 
!   PRINT_SPECTRUM -  PRINTS A SPECTRUM.
! 
!         M. DE LAS HERAS  KNMI/PCM  FEBRUARY  1990
!
!     PURPOSE.
!     --------
!
!        PRINT A WAVE MODEL SPECTRUM.
!
!     METHOD.
!     -------
!
!       NONE.
!
!     REFERENCE.
!     ----------
!
!       NONE.
!
! ---------------------------------------------------------------------------- !
!
!     INTERFACE VARIABLES.
!     --------------------

INTEGER,            INTENT(IN) :: IUNIT     !! OUTPUT UNIT.
CHARACTER (LEN=14), INTENT(IN) :: CDATE     !! DATE OF SPECTRUM (YYYYMMDDHHMMSS).
REAL,               INTENT(IN) :: LONG      !! LONGITUDE OF SPECTRUM (DEGREE).
REAL,               INTENT(IN) :: LAT       !! LATITUDE OF SPECTRUM (DEGREE).
CHARACTER (LEN=40), INTENT(IN) :: TITL      !! TITLE.
REAL,               INTENT(IN) :: SPEC(:,:) !! SPECTRUM.
REAL,               INTENT(IN) :: FR(:)     !! FREQUENCY ARRAY IN HERTZ.
REAL,               INTENT(IN) :: THETA(:)  !! DIRECTION ARRAY IN RAD.
REAL,               INTENT(IN) :: U10       !! WIND SPEED U10 (METRES/SECOND).
REAL,               INTENT(IN) :: UDIR      !! WIND DIRECTION (DEGREES).
REAL,               INTENT(IN) :: US        !! FRICTION VELOCITY (METRES/SECOND).
REAL,               INTENT(IN) :: HS        !! SIGNIFICANT WAVE HEIGHT (METRES).
REAL,               INTENT(IN) :: PPER      !! PEAK PERIOD (S).
REAL,               INTENT(IN) :: MPER      !! MEAN WAVE PERIOD (S).
REAL,               INTENT(IN) :: TM1       !! TM1 PERIOD (S).
REAL,               INTENT(IN) :: TM2       !! TM2 PERIOD (S).
REAL,               INTENT(IN) :: MDIR      !! MEAN WAVE DIRECTION (DEGREE).
REAL,               INTENT(IN) :: SPRE      !! DIRECTIONAL SPREAD (DEGREE).

! ---------------------------------------------------------------------------- !
! 
!     LOCAL VARIABLES.
!     ----------------

INTEGER, PARAMETER :: IPDIR = 12   !! NUMBER OF DIRECTIONS PRINTED PER LINE.

INTEGER            :: KL              !! NUMBER OF DIRECTIONS.
INTEGER            :: ML              !! NUMBER OF FREQUENCIES.
INTEGER            :: IPE, IP, M, LEN
REAL               :: DELTH
REAL               :: ANG(SIZE(THETA))     !! DIRECTIONS IN DEGREE.
REAL               :: ODSPEC(SIZE(FR))     !! 1-D SPECTRUM.  (M*M/HERTZ)
CHARACTER (LEN=50) :: FORM1, FORM2, FORM3  !! VARIABLE FORMATS.

! ---------------------------------------------------------------------------- !
! 
!     1. INITIALISE DIRECTIONS.
!        ----------------------

KL = SIZE(THETA)
ML = SIZE(FR)
DELTH = ZPI/REAL(KL)
ANG = THETA*DEG

! ---------------------------------------------------------------------------- !
!
!     2. COMPUTE 1-D SPECTRUM.
!        ---------------------

ODSPEC = SUM(SPEC, DIM=1)
ODSPEC = ODSPEC*DELTH

! ---------------------------------------------------------------------------- !
!
!*    3. PRINT SPECTRUM.
!        ---------------

WRITE(IUNIT,'(''1'',A40,'' DATE: '',A14,     &
&             ''   LONG.: '',F7.2,'' LAT.: '',F6.2)') TITL, CDATE, LONG, LAT
WRITE(IUNIT,'('' U10 = '',F5.2,''M/S  UDIR = '',F5.0,''DEG  USTAR = '',F5.2,   &
&             ''M/S'')') U10, UDIR, US
WRITE(IUNIT,'(''  HS = '',F5.2,''M    PPER = '',F5.2,''S     MPER = '',F5.2,   &
&             ''S  TM1 = '',F5.2,''S  TM2 = '',F5.2,''S  MDIR = '',F5.0,       &
&             ''DEG  SPREAD = '',F5.0,''DEG'')')  HS, PPER, MPER, TM1, TM2,    &
&                           MDIR, SPRE

IPE = 0
IF (KL.GT.IPDIR) THEN
   FORM1 = '(1X,''DIR (DEG)'',T11,12F8.1,''  DIR (DEG)'')'
   WRITE (FORM1(21:22),'(I2)') IPDIR
   FORM2 = '(1X,F7.4,2X,12F8.3,F9.4)'
   WRITE (FORM2(13:14),'(I2)') IPDIR
   FORM3 = '(1X,''FREQ (HZ)'',T11, 96X,''  FREQ (HZ)'')'
   WRITE (FORM3(21:23),'(I3)') IPDIR*8
   DO IP = 1,KL-IPDIR,IPDIR
      IPE = IP+IPDIR-1
      WRITE(IUNIT,FORM1) ANG(IP:IPE)
      WRITE(IUNIT,FORM3)
      WRITE(IUNIT,FORM2) (FR(M),SPEC(IP:IPE,M),FR(M),M=1,ML)
      WRITE(IUNIT,FORM1) ANG(IP:IPE)
      WRITE(IUNIT,'(1X)')
   END DO
END IF

IP = IPE+1
IPE = KL
LEN = IPE-IP+1
FORM1 = '(1X,''DIR (DEG)'',T11,12F8.1,''  DIR (DEG)'')'
WRITE (FORM1(21:22),'(I2)') LEN
FORM2 = '(1X,F7.4,2X,12F8.3,F9.4,F10.3)'
WRITE (FORM2(13:14),'(I2)') LEN
FORM3 = '(1X,''FREQ (HZ)'',T11, 96X,''  FREQ (HZ) 1-DSPEC'')'
WRITE (FORM3(21:23),'(I3)') LEN*8
WRITE (IUNIT,FORM1) ANG(IP:IPE)
WRITE (IUNIT,FORM3)
WRITE (IUNIT,FORM2) (FR(M),SPEC(IP:IPE,M),FR(M),ODSPEC(M),M=1,ML)
WRITE (IUNIT,FORM1) ANG(IP:IPE)

END SUBROUTINE PRINT_SPECTRUM

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_GENERAL_MODULE
