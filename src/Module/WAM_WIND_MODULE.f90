MODULE WAM_WIND_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE STORES THE WIND INPUT GRID SPECFICATIONS, THE WIND FIELDS      !
!   WHICH ARE PASSED FROM PREWIND TO WAM-MODELL.                               !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  & !! TERMINATES PROCESSING.
&       ADJUST,                  & !! ADJUST LONGITUDES.
&       DIFDATE,                 & !! TIME DIFFERENCE.
&       INCDATE,                 & !! INCREMENTS DATE TIME GROUP.
&       READ_WIND_INPUT            !! READ WINDS.

USE WAM_GRID_MODULE,      ONLY:  &
&       INTERPOLATION_TO_GRID      !! INTERPOLATE TO WAM POINTS.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE, ONLY: PI, ZPI, G, RAD, ROAIR, XKAPPA, EPS
USE WAM_GRID_MODULE,    ONLY: NSEA
USE WAM_MODEL_MODULE,   ONLY: U10, UDIR
USE WAM_TIMOPT_MODULE,  ONLY: CDA, CDATEA, CDATEE, IDELPRO, IDELT
USE WAM_FILE_MODULE,    ONLY: IU06, ITEST

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE
PRIVATE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INPUT WIND GRID SPECFICATIONS DATE AND WIND FIELDS.                   !
!        ---------------------------------------------------                   !

INTEGER   :: KROW  =-1       !! NUMBER OF ROWS     IN WIND INPUT GRID.
INTEGER   :: KCOL  =-1       !! NUMBER OF COLUMNES IN WIND INPUT GRID.
LOGICAL   :: IWPER =.FALSE.  !! .TRUE. IF PERIODIC GRID.
INTEGER   :: ICODE = 3       !! WIND CODE 1 = USTAR;  2 = USTRESS; 3 = U10
REAL      :: DLAM  =-1.      !! STEPSIZE BETWEEN LONGITUDES IN DEG.
REAL      :: DPHI  =-1.      !! STEPSIZE BETWEEN LATITUDES  IN DEG.
REAL      :: RLATS =-1.      !! MOST SOUTHERN LATITUDE.
REAL      :: RLATN =-1.      !! MOST NORTHERN LATITUDE.
REAL      :: RLONL =-1.      !! LEFT MOST LONGITUDE.
REAL      :: RLONR =-1.      !! RIGHT MOST LONGITUDE.
CHARACTER (LEN=14) :: CDTWIR=' '   !! DATE OF LAST WIND DATA READ FROM WIND INPUT.

REAL, ALLOCATABLE, DIMENSION(:,:)  :: UWND  !! W-E CURRENT COMPONENT.
REAL, ALLOCATABLE, DIMENSION(:,:)  :: VWND  !! S-N CURRENT COMPONENT.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. WIND TIMESTEPS.                                                       !
!        ---------------                                                       !

INTEGER   :: IDELWI = -1  !! INPUT WIND TIMESTEP INTO PREWIND IN SECONDS.
INTEGER   :: IDELWO = -1  !! OUTPUT WIND TIMESTEP OF PREWIND IN SECONDS
                          !! EQUAL TO INPUT WIND TIMESTEP INTO WAMODEL.

PUBLIC :: IDELWI, IDELWO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. WIND FIELDS PREPARED BY PREWIND FOR WAM-MODEL.                        !
!        (FIRST INDEX IS POINTS, SECOND IS TIME)                               !
!        ----------------------------------------------                        !

INTEGER  :: MPWIND = -1     !! NUMBER OF WINDFIELDS.

REAL,               ALLOCATABLE, DIMENSION(:,:) :: USM   !! WIND SPEEDS.
REAL,               ALLOCATABLE, DIMENSION(:,:) :: DSM   !! WIND DIRECTIONS.
CHARACTER (LEN=14), ALLOCATABLE, DIMENSION(:)   :: CDTM  !! DATE/TIME GROUPS.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE GET_WIND                       !! GETS WINDS FORM THIS MODULE.
   MODULE PROCEDURE GET_WIND
END INTERFACE
PUBLIC GET_WIND

INTERFACE PREPARE_WIND                   !! PREPARES WIND DATA FOR WAVE MODEL.
   MODULE PROCEDURE PREPARE_WIND
END INTERFACE
PUBLIC PREPARE_WIND

INTERFACE PRINT_WIND_STATUS              !! PRINTS WIND STATUS.
   MODULE PROCEDURE PRINT_WIND_STATUS
END INTERFACE
PUBLIC PRINT_WIND_STATUS

INTERFACE SET_WIND_FIELD                 !! SETS WIND FIELD.
   MODULE PROCEDURE SET_WIND_FIELD
END INTERFACE
PUBLIC SET_WIND_FIELD

INTERFACE SET_WIND_HEADER                !! SETS WIND HEADER.
   MODULE PROCEDURE SET_WIND_HEADER
END INTERFACE
PUBLIC SET_WIND_HEADER

INTERFACE SET_WIND_TIMESTEPS             !! SETS WIND TIMESTEPS.
   MODULE PROCEDURE SET_WIND_TIMESTEPS
END INTERFACE
PUBLIC SET_WIND_TIMESTEPS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE GETWND          !! READS AND PROCESSES ONE WINDFIELD.
   MODULE PROCEDURE GETWND
END INTERFACE
PRIVATE GETWND

INTERFACE NOTIM           !! STEERING SUB IF TIME INTERPOLATION IS NOT WANTED.
   MODULE PROCEDURE NOTIM
END INTERFACE
PRIVATE NOTIM

INTERFACE TIMIN           !! STEERING SUB IF TIME INTERPOLATION IS WANTED.
   MODULE PROCEDURE TIMIN
END INTERFACE
PRIVATE TIMIN

INTERFACE WAMWND          !! TRANSFORMS INPUT WINDS TO WAM POINTS.
   MODULE PROCEDURE WAMWND
END INTERFACE
PRIVATE WAMWND

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE GET_WIND (CDATEWO)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   GET_WIND - GETS NEW WIND DATA                                              !
!                                                                              !
!     P.A.E.M. JANSSEN  KNMI/ECMWF  SEPTEMBER 1994                             !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TRANSFER WINDS FROM WAM_WIND_MODULE TO THE WAM_MODEL_MODULE.           !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       COPY NEW WINDS.                                                        !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE                                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

CHARACTER (LEN=14),INTENT(INOUT) :: CDATEWO !! DATE TO USE OF NEW WIND FIELD.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER   :: IT

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. NEW WINDS FROM TRANSFER ARRAYS.                                       !
!        -------------------------------                                       !

DO IT = 1, MPWIND
   IF (CDTM(IT).GT.CDATEWO) THEN
      U10  = USM(:,IT)
      UDIR = DSM(:,IT)
      CDA  = CDTM(IT)
      EXIT
   END IF
END DO
IF (ITEST.GE.2) THEN
   WRITE(IU06,*) '       SUB. NEWWIND: WIND DATE/TIME IS  CDA = ', CDA
END IF

CALL INCDATE(CDATEWO,IDELWO)

END SUBROUTINE GET_WIND

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PREPARE_WIND

! ---------------------------------------------------------------------------- !
!                                                                              !
!   PREPARE_WIND - PREPARES WIND DATA FOR WAVE MODEL.                          !
!                                                                              !
!     P.GROENWOUD     DELFT HYDRAULICS LABORATORY  OKTOBER 1986                !
!                                                                              !
!     E. BAUER        MPI         FEB 1987   VERSION FOR CDC 205 HAMBURG.      !
!                                                                              !
!     S. HASSELMANN   MPI         MAY 1987   COMBINED CDC 205 AND CRAY         !
!                                            VERSIONS.                         !
!     W. BRUEGGEMANN  MPI      AUGUST 1988   SIMPLIFIED PROGRAM.               !
!                                                                              !
!     L. ZAMBRESKY    ECMWF      JUNE 1988   MODIFIED EXTENSIVELY FOR          !
!                                            COUPLING TO SPECTRAL MODEL.       !
!                                                                              !
!     H. GUNTHER      ECMWF      JUNE 1990   MODIFIED FOR CYCLE_4.             !
!     H. GUNTHER      GKSS  SEPTEMBER 2000   FT90.                             !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       EVALUATE WIND SPEED AND DIRECTION AT WAVE MODEL GRID POINTS.           !
!                                                                              !
!     INTERFACE.                                                               !
!     ----------                                                               !
!                                                                              !
!       *UNIT* *DESCRIPTION*                                                   !
!                                                                              !
!          IU01    INPUT WIND DATA (SUB READ_WIND).                            !
!          IU06    PRINTER OUTPUT (SUB INITMDL).                               !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       INPUT WIND FIELDS WHICH CAN BE COMPONENTS OR MAGNITUDE AND DIRECTION   !
!       OF USTAR, U10, USTRESS ARE TRANSFORMED TO FRICTION VELOCITIES.         !
!       THE INPUT FIELDS HAVE TO BE ON A LAT /LONG GRID.                       !
!       SEE SUB READ_WIND FOR FORMATS AND HEADER INFORMATION, WHICH HAVE TO BE !
!       GIVEN TO THE PROGRAM.                                                  !
!                                                                              !
!       A DOUBLE LINEAR INTERPOLATION IN SPACE IS PERFORMED ONTO THE MODEL     !
!       BLOCKS.                                                                !
!       IF THE WIND OUTPUT TIMSTEP IS LESS THAN THE INPUT TIMESTEP             !
!       A LINEAR INTERPOLATION IN TIME IS PERFORMED.                           !
!       ALL WIND FIELDS ARE STORED IN WAM_WIND_MODULE.                         !
!                                                                              !
!     REFERENCE.                                                               !
!     -----------                                                              !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER                       :: IDELWH
CHARACTER (LEN=14)            :: CDTWIE, CDTWIS
CHARACTER (LEN=14), PARAMETER :: ZERO = ' '

! ---------------------------------------------------------------------------- !
!                                                                              !
!                                                                              !
!     1. BEGIN AND END DATES OF WIND FIELDS TO BE PROCESSED.                   !
!        ---------------------------------------------------                   !

IF (CDA.EQ.ZERO) THEN

!        IF START FROM PRESET FIELDS DO FIRST FIELD IN ADDITION.               !

   CDTWIS = CDATEA
   CDTWIE = CDATEA
ELSE
   CDTWIS = CDA
   CALL INCDATE (CDTWIS,IDELWO)
   CDTWIE = CDA
END IF

IDELWH = MAX(IDELPRO,IDELWI,IDELT)
CALL INCDATE (CDTWIE,IDELWH)
IF (CDTWIE.GE.CDATEE) CDTWIE = CDATEE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. PROCESS WIND FIELDS.                                                  !
!        --------------------                                                  !

IF (IDELWO.GE.IDELWI) THEN

!     2.1 NO TIME INTERPOLATION.                                               !
!         ----------------------                                               !

   IF (ITEST.GE.2) THEN
      WRITE (IU06,*) '   SUB. PREPARE_WIND: WIND REQUEST'
      WRITE (IU06,*) '     NO TIME INTERPOLATION'
      WRITE (IU06,*) '     START OF PERIOD IS    CDTWIS = ',CDTWIS
      WRITE (IU06,*) '     END   OF PERIOD IS    CDTWIE = ',CDTWIE
      WRITE (IU06,*) '     WIND INPUT TIME STEP  IDELWI = ',IDELWI
      WRITE (IU06,*) '     WIND OUTPUT TIME STEP IDELWO = ',IDELWO
   END IF

   CALL NOTIM (CDTWIS, CDTWIE)

ELSE

!     2.2 TIME INTERPOLATION.                                                  !
!         -------------------                                                  !
!                                                                              !
   IF (ITEST.GE.2) THEN
      WRITE (IU06,*) '   SUB. PREPARE_WIND: WIND REQUEST'
      WRITE (IU06,*) '     TIME INTERPOLATION'
      WRITE (IU06,*) '     START OF PERIOD IS    CDTWIS = ',CDTWIS
      WRITE (IU06,*) '     END   OF PERIOD IS    CDTWIE = ',CDTWIE
      WRITE (IU06,*) '     WIND INPUT TIME STEP  IDELWI = ',IDELWI
      WRITE (IU06,*) '     WIND OUTPUT TIME STEP IDELWO = ',IDELWO
   END IF

   CALL TIMIN (CDTWIS, CDTWIE)

END IF

END SUBROUTINE PREPARE_WIND

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_WIND_STATUS

WRITE (IU06,*) '  '
WRITE (IU06,*) ' ------------------------------------------------- '
WRITE (IU06,*) '              WIND MODULE STATUS:'
WRITE (IU06,*) ' ------------------------------------------------- '
WRITE (IU06,*) '  '
WRITE (IU06,*) ' MODEL WIND INPUT TIME STEP .................: ', IDELWI,' SECS'
WRITE (IU06,*) ' MODEL WIND OUTPUT TIME STEP.................: ', IDELWO,' SECS'
IF (IDELWO.GT.0 .AND. IDELWI.GT.0) THEN
   IF (IDELWO.GE.IDELWI) THEN
      WRITE(IU06,*) ' WIND FIELDS ARE NOT INTERPOLATED IN TIME'
   ELSE
      WRITE(IU06,*) ' WIND FIELDS ARE INTERPOLATED IN TIME'
   END IF
END IF
WRITE (IU06,*) '  '
WRITE (IU06,*) ' DATE OF LAST WIND FIELD GIVEN TO MODULE.....: ', CDTWIR
WRITE (IU06,*) ' DATE OF LAST WIND FIELD GIVEN TO WAVE MODEL.: ', CDA
WRITE (IU06,*) '  '
WRITE (IU06,*) ' WIND GRID SPECIFICATION ARE:'
WRITE (IU06,*) ' NUMBER OF COLUMNS IN GRID...................: ', KCOL
WRITE (IU06,*) ' NUMBER OF ROWS    IN GRID...................: ', KROW
WRITE (IU06,*) ' SOUTHERN MOST LATITUDE......................: ', RLATS
WRITE (IU06,*) ' NORTHERN MOST LATITUDE......................: ', RLATN
WRITE (IU06,*) ' WESTERN MOST LONGITUDE......................: ', RLONL
WRITE (IU06,*) ' EASTERN MOST LONGITUDE......................: ', RLONR
WRITE (IU06,*) ' LATITUDE  STEP..............................: ', DPHI
WRITE (IU06,*) ' LONGITUDE STEP..............................: ', DLAM
IF (IWPER) THEN
   WRITE (IU06,*) ' THE GRID IS PERIODIC IN EAST-WEST DIRECTION'
ELSE
   WRITE (IU06,*) ' THE GRID IS NOT PERIODIC IN EAST-WEST DIRECTION'
END IF
WRITE (IU06,*) ' WIND CODE 1 USTAR;  2 USTRESS; 3 U10........: ', ICODE
WRITE (IU06,*) '  '
WRITE (IU06,*) ' NUMBER OF WIND FIELDS IN TRANSFER ARRAYS....: ', MPWIND
IF (MPWIND.GT.0 .AND. ITEST.GT.2) THEN
   WRITE (IU06,*) ' DATES OF WIND FIELD IN TRANSFER ARRAYS ARE:'
   WRITE (IU06,'(5(3X,A14,2X))') CDTM
END IF
WRITE (IU06,*) '  '

END SUBROUTINE PRINT_WIND_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_WIND_FIELD (CDT, U_MAP, V_MAP, CODE)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SET_WIND_FIELD - SET INPUT WIND FIELD IN WAM_WIND MODULE.                  !
!                                                                              !
!     H. GUENTHER  GKSS  JANUARY 2002                                          !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TRANSFER WINDS TO WAM_WIND_MODULE.                                     !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       CHECK CONSISTENCY AND COPY NEW WINDS. IF WINDS ARE GIVEN AS MAGNITUDE  !
!       AND DIRECTION, THEY ARE CHANGED TO VECTOR COMPONENTS.                  !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE                                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

CHARACTER (LEN=14),INTENT(IN) :: CDT        !! DATE/TIME OF WIND FIELD.
REAL,              INTENT(IN) :: U_MAP(:,:) !! U-COMP. OR SPEED OF WINDS [M/S].
REAL,              INTENT(IN) :: V_MAP(:,:) !! V-COMP. [M/S] OR DIRECTION [DEG]
INTEGER, INTENT(IN), OPTIONAL :: CODE       !! 1 = SPEED AND DIRECTION
                                            !! OTHERWISE: COMPONENTS

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COPY DATE.                                                            !
!        ----------                                                            !

CDTWIR = CDT

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. CHECK CONSISTENCY.                                                    !
!        ------------------                                                    !

IF (KCOL.NE.SIZE(U_MAP,1) .OR. KROW.NE.SIZE(U_MAP,2) .OR.                      &
&   KCOL.NE.SIZE(V_MAP,1) .OR. KROW.NE.SIZE(V_MAP,2) ) THEN
   WRITE (IU06,*) ' **********************************************************'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *          FATAL  ERROR IN SUB. SET_WIND_FIELD           *'
   WRITE (IU06,*) ' *          ===================================           *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' * GRID SPECIFICATINOS ARE NOT CONSISTENT                 *'
   WRITE (IU06,*) ' * OR WIND HEADER IS NOT DEFINED:                         *'
   WRITE (IU06,*) ' * GRID SIZES AS DEFINED IN MODULE ARE:                   *'
   WRITE (IU06,*) ' * NO. OF LONGITUDES    KCOL = ', KCOL
   WRITE (IU06,*) ' * NO. OF LATITUDE      KROW = ', KROW
   WRITE (IU06,*) ' * DIMENSIONS OF U WIND ARRAY ARE :                       *'
   WRITE (IU06,*) ' * NO. OF LONGITUDES 1. DIMENSION = ', SIZE(U_MAP,1)
   WRITE (IU06,*) ' * NO. OF LATITUDE   2. DIMENSION = ', SIZE(U_MAP,2)
   WRITE (IU06,*) ' * DIMENSIONS OF V WIND ARRAY ARE :                       *'
   WRITE (IU06,*) ' * NO. OF LONGITUDES 1. DIMENSION = ', SIZE(V_MAP,1)
   WRITE (IU06,*) ' * NO. OF LATITUDE   2. DIMENSION = ', SIZE(V_MAP,2)
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' **********************************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. COPY WIND FIELDS.                                                     !
!        -----------------                                                     !

IF (ALLOCATED(UWND)) DEALLOCATE(UWND)
IF (ALLOCATED(VWND)) DEALLOCATE(VWND)
ALLOCATE (UWND(KCOL,KROW))
ALLOCATE (VWND(KCOL,KROW))

IF (PRESENT(CODE)) THEN
   IF (CODE.EQ.1) THEN
      UWND = U_MAP*SIN(V_MAP*RAD)
      VWND = U_MAP*COS(V_MAP*RAD)
   ELSE
      UWND = U_MAP
      VWND = V_MAP
   END IF
ELSE
   UWND = U_MAP
   VWND = V_MAP
END IF

END SUBROUTINE SET_WIND_FIELD

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_WIND_HEADER (WEST, SOUTH, EAST, NORTH, D_LON, D_LAT,            &
&                           N_LON, N_LAT, CODE)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SET_WIND_HEADER - SET INPUT GRID FOR WIND FIELDS IN WAM_WIND MODULE.       !
!                                                                              !
!     H. GUENTHER  GKSS  JANUARY 2002                                          !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TRANSFER THE INPUT GRID DEFINITIONS FOR WIND FIELDS TO WAM_WIND_MODULE.!
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       CHECK CONSISTENCY AND COPY DEFINITIONS.                                !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE                                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

REAL,    INTENT(IN)           :: WEST    !! WEST LONGITUDE OF GRID [DEG].
REAL,    INTENT(IN)           :: SOUTH   !! SOUTH LATITUDE OF GRID [DEG].
REAL,    INTENT(IN), OPTIONAL :: EAST    !! EAST LONGITUDE OF GRID [DEG].
REAL,    INTENT(IN), OPTIONAL :: NORTH   !! NORTH LATITUDE OF GRID [DEG].
REAL,    INTENT(IN), OPTIONAL :: D_LON   !! LONGITUDE INCREMENT OF GRID [DEG].
REAL,    INTENT(IN), OPTIONAL :: D_LAT   !! LATITUDE INCREMENT OF GRID [DEG].
INTEGER, INTENT(IN), OPTIONAL :: N_LON   !! NUMBER OF LONGITUDES IN GRID.
INTEGER, INTENT(IN), OPTIONAL :: N_LAT   !! NUMBER OF LATITUDES IN GRID.
INTEGER, INTENT(IN), OPTIONAL :: CODE    !! WIND CODE 1 = USTAR;  2 = USTRESS;
                                         !! 3 = U10 (DEFAULT).
! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

LOGICAL  :: ERROR=.FALSE.   !! ERROR FLAG

! ---------------------------------------------------------------------------- !
!                                                                              !
!     0. CLEAR GRID DEFINITIONS.                                               !
!        ----------------------_                                               !

KROW  =-1       !! NUMBER OF ROWS     IN WIND INPUT GRID.
KCOL  =-1       !! NUMBER OF COLUMNES IN WIND INPUT GRID.
IWPER =.FALSE.  !! .TRUE. IF PERIODIC GRID.
ICODE = 3       !! WIND CODE 1 = USTAR;  2 = USTRESS; 3 = U10
DLAM  =-1.      !! STEPSIZE BETWEEN LONGITUDES IN DEG.
DPHI  =-1.      !! STEPSIZE BETWEEN LATITUDES  IN DEG.
RLATS =-1.      !! MOST SOUTHERN LATITUDE.
RLATN =-1.      !! MOST NORTHERN LATITUDE.
RLONL =-1.      !! LEFT MOST LONGITUDE.
RLONR =-1.      !! RIGHT MOST LONGITUDE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COPY GRID DEFINITIONS.                                                !
!        ----------------------                                                !

RLONL = WEST                               !! WEST LONGITUDE OF GRID [DEG].
IF (PRESENT(EAST)) THEN
   RLONR = EAST                            !! EAST LONGITUDE OF GRID [DEG].
   CALL ADJUST (RLONL, RLONR)
   IF (PRESENT(D_LON) .AND. PRESENT(N_LON)) THEN
      DLAM  = D_LON                        !! LONGITUDE INCREMENT [DEG].
      KCOL = N_LON                         !! NO. OF LONGITUDES IN FILE
   ELSE IF (PRESENT(D_LON)) THEN
      DLAM  = D_LON                        !! LONGITUDE INCREMENT [DEG].
      IF (DLAM.GT.0.) KCOL = NINT((RLONR-RLONL)/DLAM+1.0)  !! NO. OF LONGITUDES.
   ELSE IF (PRESENT(N_LON)) THEN
      KCOL = N_LON                         !! NO. OF LONGITUDES IN FILE
      IF (KCOL.GT.1) DLAM = (RLONR - RLONL)/REAL(KCOL-1) !! LONGITUDE INCREMENT.
   ELSE
      ERROR = .TRUE.
   END IF
ELSE
   IF (PRESENT(D_LON) .AND. PRESENT(N_LON)) THEN
      DLAM  = D_LON                         !! LONGITUDE INCREMENT [DEG].
      KCOL  = N_LON                         !! NO. OF LONGITUDES IN FILE.
      RLONR = WEST  + REAL(N_LON-1)*D_LON   !! EAST LONGITUDE OF GRID [DEG].
      CALL ADJUST (RLONL, RLONR)
   ELSE
      ERROR = .TRUE.
   END IF
END IF
IF (.NOT.ERROR) IWPER = ABS(EAST+DLAM-360.-WEST).LT.EPS   !! PERIODIC?

RLATS = SOUTH                               !! SOUTH LATITUDE OF GRID [DEG].
IF (PRESENT(NORTH)) THEN
   RLATN = NORTH                            !! NORTH LATITUDE OF GRID [DEG].
   IF (PRESENT(D_LAT) .AND. PRESENT(N_LAT)) THEN
      DPHI  = D_LAT                         !! LATITUDE INCREMENT [DEG].
      KROW  = N_LAT                         !! NO. OF LONGITUDES IN FILE.
   ELSE IF (PRESENT(D_LAT)) THEN
      DPHI  = D_LAT                         !! LATITUDE INCREMENT [DEG].
      IF (DPHI.GT.0.) KROW = NINT((RLATN-RLATS)/DPHI+1.0) !! NO. OF LATITUDES.
   ELSE IF (PRESENT(N_LAT)) THEN
      KROW = N_LAT                          !! NO. OF LATITUDES IN FILE.
      IF (KROW.GT.1) DPHI = (RLATN-RLATS)/REAL(KROW-1) !! LATITUDE INCREMENT.
   ELSE
      ERROR = .TRUE.
   END IF
ELSE
   IF (PRESENT(D_LAT) .AND. PRESENT(N_LAT)) THEN
      DPHI  = D_LAT                         !! LATITUDE INCREMENT [DEG].
      KROW  = N_LAT                         !! NO. OF LONGITUDES IN FILE.
      RLATN = SOUTH + REAL(N_LAT-1)*D_LAT   !! NORTH LATITUDE OF GRID [DEG].
   ELSE
      ERROR = .TRUE.
   END IF
END IF

IF (PRESENT(CODE)) THEN
   ICODE = CODE
ELSE
   ICODE = 3
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. CHECK CONSISTENCY.                                                    !
!        ------------------                                                    !
IF (ABS(RLATS + REAL(KROW-1)*DPHI-RLATN).GT.EPS .OR.                           &
&   ABS(RLONL + REAL(KCOL-1)*DLAM-RLONR).GT.EPS .OR.                           &
&   KROW.LT.2 .OR. KCOL.LT.2 .OR. ERROR) THEN

   WRITE (IU06,*) ' **********************************************************'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *        FATAL ERROR IN SUB. SET_WIND_HEADER             *'
   WRITE (IU06,*) ' *        ===================================             *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' * GRID SPECIFICATINOS ARE NOT CONSISTENT OR INSUFFICIENT *'
   WRITE (IU06,*) ' * USER PROVIDED GRID DEFINITIONS ARE:                    *'
   WRITE (IU06,*) ' * LONGITUDE            WEST = ', WEST
   WRITE (IU06,*) ' * LATITUDE            SOUTH = ', SOUTH
   IF (PRESENT(EAST )) WRITE (IU06,*) ' * LONGITUDE            EAST = ', EAST
   IF (PRESENT(NORTH)) WRITE (IU06,*) ' * LATITUDE            NORTH = ', NORTH
   IF (PRESENT(D_LON)) WRITE (IU06,*) ' * LONGITUDE INCREMENT D_LON = ', D_LON
   IF (PRESENT(D_LAT)) WRITE (IU06,*) ' * LATITUDE  INCREMENT D_LAT = ', D_LAT
   IF (PRESENT(N_LON)) WRITE (IU06,*) ' * NO. OF LONGITUDES   N_LON = ', N_LON
   IF (PRESENT(N_LAT)) WRITE (IU06,*) ' * NO. OF LATITUDE     N_LAT = ', N_LAT
   CALL PRINT_WIND_STATUS
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' **********************************************************'
   CALL ABORT1
END IF

IF (ICODE.LT.1 .OR. ICODE.GT.3) THEN
   WRITE (IU06,*) ' **********************************************************'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *        FATAL ERROR IN SUB. SET_WIND_HEADER             *'
   WRITE (IU06,*) ' *        ===================================             *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' * WIND CODE OUT OF RANGE. CODE MUST BE:                  *'
   WRITE (IU06,*) ' * 1 = USTAR;  2 = USTRESS; OR 3 = U10 (DEFAULT).         *'
   WRITE (IU06,*) ' * BUT CODE IS: ', ICODE
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' **********************************************************'
   CALL ABORT1
END IF

END SUBROUTINE SET_WIND_HEADER

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_WIND_TIMESTEPS (IN, OUT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SET_WIND_TIMESTEPS - SET WIND TIMESTEPS IN WAM_WIND MODULE.                !
!                                                                              !
!     H. GUENTHER  GKSS  JANUARY 2002                                          !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TRANSFER THE WIND TIMESTEPS TO WAM_WIND_MODULE.                        !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       CHECK CONSISTENCY AND COPY DEFINITIONS.                                !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE                                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER, INTENT(IN)           :: IN    !! WIND INPUT TIME STEP.
INTEGER, INTENT(IN), OPTIONAL :: OUT   !! WIND OUTPUT TIME STEP.

IDELWI = IN
IF (PRESENT(OUT)) THEN
   IDELWO = OUT
ELSE
   IDELWO = IN
END IF

!         CHECK WIND INPUT AND WIND OUTPUT TIMESTEP.                           !
!         ------------------------------------------                           !

IF (IN.LE.0 .OR. IDELWO.LE.0) THEN
   WRITE(IU06,*) '***********************************************************'
   WRITE(IU06,*) '*                                                         *'
   WRITE(IU06,*) '*       FATAL ERROR IN SUB. SET_WIND_TIMESTEPS            *'
   WRITE(IU06,*) '*       ========================================          *'
   WRITE(IU06,*) '* WIND INPUT OR OUTPUT TIME STEP IS NOT POSITIVE.         *'
   WRITE(IU06,*) '* WIND INPUT TIMESTEP    IN = ', IDELWI, ' SECONDS'
   WRITE(IU06,*) '* WIND OUTPUT TIMESTEP  OUT = ', IDELWO, ' SECONDS'
   WRITE(IU06,*) '*                                                         *'
   WRITE(IU06,*) '*        PROGRAM ABORTS         PROGRAM ABORTS            *'
   WRITE(IU06,*) '*                                                         *'
   WRITE(IU06,*) '***********************************************************'
   CALL ABORT1
END IF
IF (IDELWI.LT.IDELWO) THEN
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
   WRITE(IU06,*) '+                                                         +'
   WRITE(IU06,*) '+       WARNING ERROR IN SUB. SET_WIND_TIMESTEPS          +'
   WRITE(IU06,*) '+       ========================================          +'
   WRITE(IU06,*) '+ WIND INPUT TIME STEP IS LESS THAN WIND OUTPUT STEP      +'
   WRITE(IU06,*) '+ WIND INPUT TIMESTEP    IN = ', IDELWI, ' SECONDS'
   WRITE(IU06,*) '+ WIND OUTPUT TIMESTEP  OUT = ', IDELWO, ' SECONDS'
   WRITE(IU06,*) '+                                                         +'
   WRITE(IU06,*) '+ WIND INPUT AND OUTPUT CHANGED TO THE MULIPLE OF THE     +'
   WRITE(IU06,*) '+ INPUT NEAREST TO WIND OLD OUTPUT TIME STEP              +'
   WRITE(IU06,*) '+ THE MODEL MAY IGNORE WIND FIELDS                        +'
   IDELWI = IDELWI*MAX(NINT(REAL(IDELWO)/REAL(IDELWI)),1)
   IDELWO = IDELWI
   WRITE(IU06,*) '+ NEW INPUT  TIMESTEP   IS: ',IDELWI, ' SECONDS'
   WRITE(IU06,*) '+ NEW OUTPUT TIMESTEP   IS: ',IDELWO, ' SECONDS'
   WRITE(IU06,*) '+                                                         +'
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
END IF
IF (MOD(IDELWI,IDELWO).NE.0) THEN
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
   WRITE(IU06,*) '+                                                         +'
   WRITE(IU06,*) '+       WARNING ERROR IN SUB. SET_WIND_TIMESTEPS          +'
   WRITE(IU06,*) '+       ========================================          +'
   WRITE(IU06,*) '+ WIND INPUT AND WIND OUTPUT TIME STEPS DO NOT HAVE       +'
   WRITE(IU06,*) '+ INTEGER RATIO.                                          +'
   WRITE(IU06,*) '+ WIND INPUT TIMESTEP    IN = ', IDELWI, ' SECONDS'
   WRITE(IU06,*) '+ WIND OUTPUT TIMESTEP  OUT = ', IDELWO, ' SECONDS'
   WRITE(IU06,*) '+ OUTPUT TIMESTEP IS CHANGED TO NEAREST MULTIPLE OF THE   +'
   WRITE(IU06,*) '+ INPUT TIME STEP.                                        +'
   IDELWO = IDELWI/MAX(NINT(REAL(IDELWI)/REAL(IDELWO)),1)
   WRITE(IU06,*) '+ NEW OUTPUT TIMESTEP   IS: ',IDELWO, ' SECONDS'
   WRITE(IU06,*) '+                                                         +'
   WRITE(IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
END IF

END SUBROUTINE SET_WIND_TIMESTEPS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     G. PRIVAT MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE GETWND (US, DS, CDTWIS)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   GETWND - ROUTINE TO READ AND PROCESS ONE WINDFIELD.                        !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!        READ A WINDFIELD FROM THE WINDFILE (SEARCH FOR IT) AND                !
!        CALCULATES THE WIND VELOCITY  AND DIRECTION FOR ALL WAM BLOCKS.       !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       NONE.                                                                  !
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

REAL,          INTENT(OUT)    :: US(:)   !! MAGNITUDE OF USTAR.
REAL,          INTENT(OUT)    :: DS(:)   !! DIRECTION OF USTAR.
CHARACTER (LEN=14),INTENT(IN) :: CDTWIS  !! DATE OF WIND FIELD TO BE LOOKED FOR.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. READ WIND DATA AND CHECK DATE.                                        !
!        ------------------------------                                        !

DO
   CALL READ_WIND_INPUT

   IF (CDTWIR.EQ.CDTWIS) EXIT
   IF (CDTWIR.GT.CDTWIS) THEN
         WRITE (IU06,*) ' ******************************************'
         WRITE (IU06,*) ' *                                        *'
         WRITE (IU06,*) ' *      FATAL ERROR SUB. GETWND           *'
         WRITE (IU06,*) ' *      =======================           *'
         WRITE (IU06,*) ' * WIND DATE READ IS LATER THAN EXPECTED  *'
         WRITE (IU06,*) ' * DATE READ IS     CDTWIR = ', CDTWIR
         WRITE (IU06,*) ' * DATE EXPECTED IS CDTWIS = ', CDTWIS
         WRITE (IU06,*) ' *                                        *'
         WRITE (IU06,*) ' *   PROGRAM ABORTS  PROGRAM ABORTS       *'
         WRITE (IU06,*) ' *                                        *'
         WRITE (IU06,*) ' ******************************************'
         CALL ABORT1
   END IF
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. INTERPOLATE AND BLOCK WINDFIELD                                       !
!        -------------------------------                                       !

CALL WAMWND (US, DS)

END SUBROUTINE GETWND

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE NOTIM (CDTWIS, CDTWIE)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   NOTIM - STEERING SUB IF TIME INTERPOLATION IS NOT WANTED.                  !
!                                                                              !
!     H. GUNTHER    ECMWF    MAY 1990     MODIFIED FOR SUB VERSION.            !
!     H. GUNTHER    ECMWF    DECEMBER 90  MODIFIED FOR CYCLE_4.                !
!     H. GUNTHER      GKSS    SEPTEMBER 2000   FT90.                           !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       NOTIM NO TIME INTERPOLATION: PROCESS WINDFIELDS.                       !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       NO TIME INTERPOLATION:                                                 !
!       WINDFIELDS ARE PROCESSED EVERY IDELWI SECONDS (U,V),                   !
!       THE WINDS INTERPOLATED IN SPACE ONLY.                                  !
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

CHARACTER (LEN=14), INTENT(IN)  :: CDTWIS  !! DATE OF FIRST WIND FIELD.
CHARACTER (LEN=14), INTENT(IN)  :: CDTWIE  !! DATE OF LAST WIND FIELD.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER :: MP, IWPERIOD

CHARACTER (LEN=14)            :: CDTWIH
CHARACTER (LEN=14), PARAMETER :: ZERO = ' '

REAL :: US(1:NSEA)  !! OUTPUT WIND FIELD ARRAY (FRICTION VELOCITY).
REAL :: DS(1:NSEA)  !! OUTPUT WIND FIELD ARRAY (DIRECTION).

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INITIALISE WIND REQUEST DATE AND PROCESS FIRST WINDFIELD IF COLD START!
!        ----------------------------------------------------------------------!

CDTWIH = CDTWIS
IF (CDA.EQ.ZERO) THEN
   CDA = CDTWIS
   CALL GETWND (U10, UDIR, CDA)
   IF (ITEST.GE.3) THEN
      WRITE(IU06,'(''      SUB. NOTIM: FIRST WIND FIELD SAVED'')')
   END IF
   IF (CDA.EQ.CDTWIE) RETURN
   CALL INCDATE (CDTWIH,IDELWO)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. NUMBER OF ADDITIONAL WIND FIELDS TO BE GENERATED.                     !
!        -------------------------------------------------                     !

CALL DIFDATE(CDTWIH, CDTWIE, IWPERIOD)
MPWIND = IWPERIOD/IDELWO+1

IF (ITEST.GE.3) THEN
    WRITE(IU06,*) '       SUB. NOTIM:  NUMBER OF ADDITIONAL WIND FIELDS IS ',  &
&                 ' MPWIND = ', MPWIND
    WRITE(IU06,*) '                    FIELDS ARE SAVED IN MODULE WAM_WIND'
END IF
IF (.NOT. ALLOCATED(USM) ) ALLOCATE (USM(1:NSEA,MPWIND))
IF (.NOT. ALLOCATED(DSM) ) ALLOCATE (DSM(1:NSEA,MPWIND))
IF (.NOT. ALLOCATED(CDTM)) ALLOCATE (CDTM(MPWIND))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. LOOP OVER OUTPUT WIND TIMES.                                          !
!        ----------------------------                                          !

MP = 0
DO WHILE (CDTWIH.LE.CDTWIE)

!     3.1 READ ONE WIND FIELD AND TRANSFORM TO BLOCKS.                         !
!         --------------------------------------------                         !

   CALL GETWND (US, DS, CDTWIH)
   MP = MP + 1

!     3.2 SAVE IN MODULE WAM_WIND.                                             !
!         ------------------------                                             !

   USM(:,MP)  = US
   DSM(:,MP)  = DS
   CDTM(MP) = CDTWIH

   IF (ITEST.GE.3) THEN
      WRITE(IU06,*) '       SUB. NOTIM: NEW WIND FILES AT CDTWIH = ', CDTWIH
   END IF

!     3.3 UPDATE WIND FIELD REQUEST TIME.                                      !
!         -------------------------------                                      !

   CALL INCDATE (CDTWIH,IDELWO)
END DO

END SUBROUTINE NOTIM

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE TIMIN (CDTWIS, CDTWIE)

! ---------------------------------------------------------------------------- !
!                                                                              !
!                                                                              !
!   TIMIN - STEERING MODULE IF TIME INTERPOLATION IS WANTED.                   !
!                                                                              !
!     H. GUNTHER    ECMWF    MAY 1990         MODIFIED FOR SUB VERSION.        !
!     H. GUNTHER    ECMWF    DECEMBER 90      MODIFIED FOR CYCLE_4.            !
!     H. GUNTHER    GKSS     SEPTEMBER 2000   FT90.                            !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TIME INTERPOLATION: PROCESS WINDFIELDS.                                !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       WINDFIELDS ARE READ IN EVERY IDELWI SECONDS (U,V),                     !
!       INTERPOLATED IN SPACE, AND BLOCKED.                                    !
!       MAGNITUDE AND DIRECTION ARE INTERPOLATED LINEARLY IN TIME.             !
!       WINDFIELDS AND SAVED IN WAM_WIND_MODULE.                               !
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

CHARACTER (LEN=14), INTENT(IN)  :: CDTWIS  !! DATE OF FIRST WIND FIELD.
CHARACTER (LEN=14), INTENT(IN)  :: CDTWIE  !! DATE OF LAST WIND FIELD.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER :: MP, MP0, NTS, N, IWPERIOD
REAL    :: DEL
CHARACTER (LEN=14) :: CDT1, CDT2, CDTH
CHARACTER (LEN=14), PARAMETER :: ZERO = ' '

REAL :: US (1:NSEA)  !! OUTPUT WIND FIELD ARRAY (FRICTION VELOCITY).
REAL :: DS (1:NSEA)  !! OUTPUT WIND FIELD ARRAY (DIRECTION).
REAL :: US2(1:NSEA)  !! OUTPUT WIND FIELD ARRAY (FRICTION VELOCITY).
REAL :: DS2(1:NSEA)  !! OUTPUT WIND FIELD ARRAY (DIRECTION).

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INITIALIZE TIMECOUNTER.                                               !
!        -----------------------                                               !

IF (CDA.EQ.ZERO) THEN
   CDT1 = CDTWIS
   CALL GETWND (US, DS, CDT1)
   CDA = CDT1
   U10 = US
   UDIR = DS
   IF (ITEST.GE.3) WRITE(IU06,*)                                               &
&              '       SUB. TIMIN: FIRST WIND FIELD SAVED IN WAM_MODEL_MODULE'

ELSE
   CDT1 = CDA
   US = U10
   DS = UDIR
   CDTH = CDT1
   CALL INCDATE (CDTH,IDELWO)
   IF (CDTWIS.NE.CDTH) THEN
      WRITE(IU06,*) ' *******************************************'
      WRITE(IU06,*) ' *                                         *'
      WRITE(IU06,*) ' *        FATAL ERROR IN --TIMIN--         *'
      WRITE(IU06,*) ' *        =========================        *'
      WRITE(IU06,*) ' * DATES DO NOT MATCH.                     *'
      WRITE(IU06,*) ' * START DATE FOR WIND IS       CDTWIS = ', CDTWIS
      WRITE(IU06,*) ' * LAST DATE SAVED IN COM WIND IS CDT1 = ', CDT1
      WRITE(IU06,*) ' * PROCESSING WILL BE ABORTED              *'
      WRITE(IU06,*) ' *                                         *'
      WRITE(IU06,*) ' *******************************************'
      CALL ABORT1
   END IF
ENDIF

CDT2 = CDT1
CALL INCDATE(CDT2,IDELWI)
NTS = IDELWI/IDELWO
DEL = REAL(IDELWO)/REAL(IDELWI)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. NUMBER OF ADDITIONAL WIND FIELDS TO BE GENERATED.                     !
!        -------------------------------------------------                     !

CALL DIFDATE(CDT1, CDTWIE, IWPERIOD)
MPWIND = IWPERIOD/IDELWO

IF (ITEST.GE.3) THEN
    WRITE(IU06,*) '       SUB. TIMIN:  NUMBER OF ADDITIONAL WIND FIELDS IS ',  &
&                 ' MPWIND = ', MPWIND
    WRITE(IU06,*) '                    FIELDS ARE SAVED IN MODULE WAM_WIND'
END IF
IF (.NOT. ALLOCATED(USM) ) ALLOCATE (USM(1:NSEA,MPWIND))
IF (.NOT. ALLOCATED(DSM) ) ALLOCATE (DSM(1:NSEA,MPWIND))
IF (.NOT. ALLOCATED(CDTM)) ALLOCATE (CDTM(MPWIND))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. LOOP OVER INPUT WINDFIELDS.                                           !
!        ---------------------------                                           !
!                                                                              !

MP0 = 0
DO
   MP0 = MP0 +1
!                                                                              !
!     2.1 READ ONE WINDFIELD AND TRANSFORM TO BLOCKS.                          !
!         -------------------------------------------                          !

   CDT2 = CDT1
   CALL INCDATE(CDT2,IDELWI)
   CALL GETWND (US2, DS2, CDT2)

!     2.2 INTERPOLATE AND SAVE BLOCKED WIND FIELDS.                            !
!         -----------------------------------------                            !

   CDTH = CDT1
   MP = MP0-1
   DO N = 1,NTS
      MP = MP + 1
      CALL INCDATE(CDTH,IDELWO)
      CDTM(MP) = CDTH
      USM(:,MP) = US + REAL(N)*DEL*(US2-US)
      DSM(:,MP) = DS2 - DS

      WHERE (ABS(DSM(:,MP)).GT.PI)                                             &
&                              DSM(:,MP) = DSM(:,MP)-ZPI*SIGN(1.,DSM(:,MP))
      DSM(:,MP) = DS + REAL(N)*DEL*DSM(:,MP)
      DSM(:,MP) = MOD(DSM(:,MP)+ZPI,ZPI)
   END DO
   IF (ITEST.GE.3) THEN
      WRITE(IU06,*) '       SUB. TIMIN: WIND FIELDS FOR CDTM = ', CDTM(MP0:MP)
   END IF

!     2.3 UPDATE WIND FIELD REQUEST TIME AND READ NEXT IF REQUESTED.           !
!         ----------------------------------------------------------           !

   US = US2
   DS = DS2
   CDT1 = CDT2

   CALL INCDATE (CDTH,IDELWI)
   IF (CDTH.GT.CDTWIE) EXIT
END DO

END SUBROUTINE TIMIN

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE WAMWND (US, DS)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   WAMWND - TRANSFORMS INPUT WINDS TO WAM POINTS.                             !
!                                                                              !
!     H. GUNTHER      ECMWF   MAY 1990     MODIFIED FOR SUB VERSION.           !
!     H. GUNTHER      ECMWF   DECEMBER 90  MODIFIED FOR CYCLE_4.               !
!     H. GUNTHER      GKSS    SEPTEMBER 2000   FT90.                           !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       INTERPOLATE AND CONVERT INPUT WINDS TO WAM WINDS FOR ALL GRID POINTS.  !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE INPUT WINDS ARE INTERPOLATED TO THE WAVE MODEL GRID POINTS.        !
!       THE INTERPOLATED VALUES ARE TRANSFORMED TO MAGNITUDE AND DIRECTION.    !
!       INPUT MAY BE WIND IN 10M HEIGHT, SURFACE WINDS OR FRICTION VELOCETIES. !
!       THE INPUT GRID MUST BE ON A LATITUDE/LONGITUDE GRID EITHER PERIODIC    !
!       OR NON PERIODIC.                                                       !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

REAL, INTENT(OUT)    :: US(:)  !! INTERPOLATED WINDS.
REAL, INTENT(OUT)    :: DS(:)  !! INTERPOLATED WIND DIRECTION.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL, PARAMETER :: ALPHACH = 0.0185

INTEGER :: IJ
REAL    :: UU, VV, USTAR, Z0, CD

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INITIALISE WIND ARRAYS WITH ZERO                                      !
!        --------------------------------                                      !

US = 0.
DS = 0.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. INTERPOLATE INPUT WINDS TO WAVEMODEL GRID.                            !
!        ------------------------------------------                            !

CALL INTERPOLATION_TO_GRID (IU06, IWPER, DLAM, DPHI, RLONL, RLATS,             &
&                           UWND, US, VWND, DS)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. PROCESS WINDS ACCORDING TO TYPE                                       !
!        NOTHING TO DO FOR WIND SPEED U10 (ICODE = 3).                         !
!        ---------------------------------------------                         !

!     3.1 TRANSFORM TO MAGNITUDE AND DIRECTION.                                !
!         -------------------------------------                                !

   DO IJ = 1,SIZE(US)
      UU = US(IJ)
      VV = DS(IJ)
      US(IJ) = SQRT(UU**2 + VV**2)
      IF (US(IJ).NE.0.) DS(IJ) = ATAN2(UU,VV)
      IF (DS(IJ).LT.0.) DS(IJ) = DS(IJ) + ZPI
   END DO

IF (ICODE.EQ.1) THEN

!     3.2  INPUT IS FRICTION VELOCITY.                                         !
!          ---------------------------                                         !

   DO IJ = 1,SIZE(US)
         USTAR = MAX(0.01,US(IJ))
         Z0  = ALPHACH/G*USTAR**2
         CD  = XKAPPA/ALOG(10./Z0)
         US(IJ) = USTAR/CD
   END DO

ELSE IF (ICODE.EQ.2) THEN

!     3.3 INPUT WINDS ARE SURFACE STRESSES.                                    !
!         ---------------------------------                                    !
!                                                                              !
   DO IJ = 1,SIZE(US)
         USTAR = MAX (0.01, SQRT(US(IJ)/ROAIR))
         Z0  = ALPHACH/G*USTAR**2
         CD  = XKAPPA/ALOG(10./Z0)
         US(IJ) = USTAR/CD
   END DO
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. TEST OUTPUT OF WAVE MODEL BLOCKS                                      !
!        ---------------------------------                                     !

IF (ITEST.GE.3) THEN
   IJ = MIN (10,SIZE(US))
   WRITE (IU06,*) ' '
   WRITE (IU06,*) '      SUB. WAMWND: WINDFIELDS CONVERTED TO BLOCKS'
   WRITE (IU06,*) ' '
   WRITE (IU06,*) ' US(1:10) = ', US(1:IJ)
   WRITE (IU06,*) ' DS(1:10) = ', DS(1:IJ)
END IF

END SUBROUTINE WAMWND

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_WIND_MODULE
