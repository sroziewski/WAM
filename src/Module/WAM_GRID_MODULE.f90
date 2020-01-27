MODULE WAM_GRID_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS ALL VARIABLES AND CONSTANTS TO DEFINE THE MODEL GRID. !
!   ALL PROCEDURES ARE INCLUDED TO COMPUTE THE INFORMATION.                    !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  &  !! TERMINATES PROCESSING.
&       ADJUST,                  &  !! ADJUST LONGITUDES.
&       PRINT_ARRAY                 !! PRINTS AN ARRAY.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_FILE_MODULE,    ONLY: IU06, ITEST
USE WAM_GENERAL_MODULE, ONLY: RAD, CIRC, EPS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE
PRIVATE

LOGICAL :: SET_STATUS    = .FALSE. !! .TRUE. IF USER INPUT IS DEFINED.
 
! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. TOPOGRAPHY INPUT DATA AS PROVIDED BY USER.                            !
!        ------------------------------------------                            !

INTEGER              :: NX_T = -1     !! NUMBER OF LONGITUDES.
INTEGER              :: NY_T = -1     !! NUMBER OF LATITUDES.
LOGICAL              :: PER_T         !! .TRUE. IF GRID IS PERIODICAL.
REAL                 :: DY_T          !! LATITUDE INCREMENT [DEG].
REAL                 :: DX_T          !! LONGITUDE INCREMENT [DEG].
REAL                 :: SOUTH_T       !! SOUTH LATITUDE OF GRID [DEG].
REAL                 :: NORTH_T       !! NORTH LATITUDE OF GRID [DEG].
REAL                 :: WEST_T        !! WEST LONGITUDE OF GRID [DEG].
REAL                 :: EAST_T        !! EAST LONGITUDE OF GRID [DEG].
INTEGER, ALLOCATABLE :: GRID_IN(:,:)  !! WATER DEPTH [M].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. GRID CORRECTION AREAS.                                                !
!        ----------------------                                                !

INTEGER              :: N_CA = 0      !! NO. OF CORRECTION AREAS
REAL,    ALLOCATABLE :: SOUTH_CA(:)   !! S - LATITUDE OF AREA
REAL,    ALLOCATABLE :: NORTH_CA(:)   !! N - LATITUDE OF AREA
REAL,    ALLOCATABLE :: WEST_CA (:)   !! W - LONGITUIDE OF AREA
REAL,    ALLOCATABLE :: EAST_CA (:)   !! E - LONGITUIDE OF AREA
INTEGER, ALLOCATABLE :: DEPTH_CA(:)   !! DEPTH OF AREA

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. GRIDDED MODEL DEPTH DATA.                                             !
!        -------------------------                                             !

INTEGER,ALLOCATABLE, DIMENSION(:,:) :: GRD   !! GRIDDED TOPOGRAPHY

! ---------------------------------------------------------------------------- !
!
!     5. GENERAL MODEL GRID INFORMATION.
!        -------------------------------                                       !

CHARACTER (LEN=80) :: HEADER=' ' !! HEADER OF MODEL RUN.
INTEGER            :: NX = -1    !! NUMBER OF LONGITUDES IN GRID.
INTEGER            :: NY = -1    !! NUMBER OF LATITUDES  IN GRID.
INTEGER            :: NSEA = -1  !! NUMBER OF SEA POINTS. 
LOGICAL            :: IPER       !! .TRUE. IF GRID IS PERIODIC.
LOGICAL            :: ONE_POINT  !! .TRUE. IF GRID HAS ONE POINT ONLY.
REAL               :: AMOWEP     !! MOST WESTERN LONGITUDE IN GRID [DEG].
REAL               :: AMOSOP     !! MOST SOUTHERN LATITUDE IN GRID [DEG].
REAL               :: AMOEAP     !! MOST EASTERN LONGITUDE IN GRID [DEG].
REAL               :: AMONOP     !! MOST NORTHERN LATITUDE IN GRID [DEG].
REAL               :: XDELLA     !! GRID INCREMENT FOR LATITUDE [DEG].
REAL               :: DELPHI     !! GRID INCREMENT FOR LATITUDE [M].
REAL               :: XDELLO     !! GRID INCREMENT FOR LONG. AT EQUATOR [DEG].
REAL               :: DELLAM     !! GRID INCREMENT FOR LONG. AT EQUATOR [M].
REAL,  ALLOCATABLE :: SINPH(:)   !! SIN OF LATITUDE.
REAL,  ALLOCATABLE :: COSPH(:)   !! COS OF LATITUDE. 

LOGICAL(KIND=1), ALLOCATABLE :: L_S_MASK(:,:) !! .TRUE. AT SEA POINTS.

REAL,    ALLOCATABLE :: DEPTH(:)  !! WATER DEPTH [M].
INTEGER, ALLOCATABLE :: IXLG(:)   !! LONGITUDE GRID INDEX.
INTEGER, ALLOCATABLE :: KXLT(:)   !! LATITUDE GRID INDEX.

INTEGER, ALLOCATABLE :: KLAT(:,:) !! INDEX OF GRIDPOINT SOUTH AND NORTH
                                  !! LANDPOINTS ARE MARKED BY ZERO.
INTEGER, ALLOCATABLE :: KLON(:,:) !! INDEX OF GRIDPOINT WEST AND EAST
                                  !! LANDPOINTS ARE MARKED BY ZERO.
INTEGER, ALLOCATABLE :: INDEP(:)  !! DEPTH INDEX.

PUBLIC :: HEADER, NX, NY, NSEA, IPER, AMOWEP, AMOSOP, AMOEAP, AMONOP, XDELLA,  &
&         DELPHI, XDELLO, DELLAM, SINPH, COSPH, DEPTH, IXLG, KXLT, KLAT, KLON, &
&         INDEP, L_S_MASK, ONE_POINT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE SET_GRID_CORRECTIONS       !! TRANSFERS GRID CORRECTIONS TO MODULE.
   MODULE PROCEDURE SET_GRID_CORRECTIONS
END INTERFACE
PUBLIC SET_GRID_CORRECTIONS

INTERFACE SET_GRID_DEF               !! TRANSFERS MODEL GRID DEFINITIONS 
   MODULE PROCEDURE SET_GRID_DEF
END INTERFACE
PUBLIC SET_GRID_DEF

INTERFACE SET_HEADER                 !! TRANSFERS MODEL HEADER 
   MODULE PROCEDURE SET_HEADER
END INTERFACE
PUBLIC SET_HEADER

INTERFACE SET_TOPOGRAPHY             !! TRANSFERS DEPTH DATA TO MODULE.
   MODULE PROCEDURE SET_TOPOGRAPHY
END INTERFACE
PUBLIC SET_TOPOGRAPHY

INTERFACE PREPARE_GRID               !! ROUTINE TO ARRANGE WAMODEL GRID.
   MODULE PROCEDURE PREPARE_GRID
END INTERFACE
PUBLIC PREPARE_GRID

INTERFACE PRINT_GRID_STATUS          !! PRINTS GRID MODULE STATUS.
   MODULE PROCEDURE PRINT_GRID_STATUS
END INTERFACE
PUBLIC PRINT_GRID_STATUS

INTERFACE FIND_SEA_POINT              !! FIND SEA POINT NUMBER. 
   MODULE  PROCEDURE FIND_SEA_POINT
END INTERFACE
PUBLIC FIND_SEA_POINT

INTERFACE INTERPOLATION_TO_GRID       !! INTERPOLATES TO MODEL GRID POINTS. 
   MODULE  PROCEDURE INTERPOLATION_TO_GRID
END INTERFACE
PUBLIC INTERPOLATION_TO_GRID

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE SUBGRID_TOPOGRAPHY         !! ARRANGE SUBGRID TOPOGRAPHY.
   MODULE PROCEDURE SUBGRID_TOPOGRAPHY
END INTERFACE
PRIVATE SUBGRID_TOPOGRAPHY

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_GRID_CORRECTIONS (N_COR, SOUTH, NORTH, WEST, EAST, D_COR)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER, INTENT(IN)  :: N_COR      !! NUMBER OF CORRECTION AREAS.
REAL   , INTENT(IN)  :: SOUTH(*)   !! SOUTH LATITUDES OF CORR. AREAS [DEG].
REAL   , INTENT(IN)  :: NORTH(*)   !! NORTH LATITUDES OF CORR. AREAS [DEG].
REAL   , INTENT(IN)  :: WEST(*)    !! WEST LONGITUDES OF CORR. AREAS [DEG].
REAL   , INTENT(IN)  :: EAST(*)    !! EAST LONGITUDES OF CORR. AREAS [DEG].
INTEGER, INTENT(IN)  :: D_COR(*)   !! DEPTH IN CORR. AREAS [M].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CLEAR CORRECTION AREAS.                                               !
!        -----------------------                                               !

N_CA = 0
IF (ALLOCATED(SOUTH_CA)) DEALLOCATE(SOUTH_CA)
IF (ALLOCATED(NORTH_CA)) DEALLOCATE(NORTH_CA)
IF (ALLOCATED(WEST_CA )) DEALLOCATE(WEST_CA)
IF (ALLOCATED(EAST_CA )) DEALLOCATE(EAST_CA)
IF (ALLOCATED(DEPTH_CA)) DEALLOCATE(DEPTH_CA)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COPY CORRECTION AREAS DEFINITIONS.                                    !
!        ----------------------------------                                    !

N_CA =  N_COR
IF (N_CA.GT.0) THEN
   ALLOCATE(SOUTH_CA(1:N_CA))
   ALLOCATE(NORTH_CA(1:N_CA))
   ALLOCATE(WEST_CA(1:N_CA))
   ALLOCATE(EAST_CA(1:N_CA))
   ALLOCATE(DEPTH_CA(1:N_CA))

   SOUTH_CA = SOUTH(1:N_CA)
   NORTH_CA = NORTH(1:N_CA)
   WEST_CA = WEST (1:N_CA)
   EAST_CA = EAST (1:N_CA)
   DEPTH_CA = D_COR(1:N_CA)
   CALL ADJUST (WEST_CA, EAST_CA)
END IF

END SUBROUTINE SET_GRID_CORRECTIONS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_GRID_DEF (N_LON, N_LAT, D_LON, D_LAT, SOUTH, NORTH, WEST, EAST)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER, INTENT(IN)  :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
INTEGER, INTENT(IN)  :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
REAL   , INTENT(IN)  :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].
REAL   , INTENT(IN)  :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].
REAL   , INTENT(IN)  :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].
REAL   , INTENT(IN)  :: NORTH      !! NORTH LATITUDE OF GRID [DEG].
REAL   , INTENT(IN)  :: WEST       !! WEST LONGITUDE OF GRID [DEG].
REAL   , INTENT(IN)  :: EAST       !! EAST LONGITUDE OF GRID [DEG].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CLEAR GRID DEFINITIONS.                                               !
!        -----------------------                                               !

NX = -1
NY = -1

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COPY GRID DEFINITIONS.                                                !
!        ----------------------                                                !

XDELLO = D_LON
XDELLA = D_LAT
AMOSOP = SOUTH
AMONOP = NORTH
AMOWEP = WEST
AMOEAP = EAST
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
IF (NX.EQ.1 .AND. NY.EQ.1) THEN
   ONE_POINT = .TRUE.
   XDELLO = 1.
   XDELLA = 1.
ELSE
   ONE_POINT = .FALSE.
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. CHECK CONSISTENCY.                                                    !
!        ------------------                                                    !

IF (N_LON.NE.NX .OR. N_LAT.NE.NY) THEN
   WRITE (IU06,*) ' **********************************************************'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *          FATAL  ERROR IN SUB. SET_GRID_DEF             *'
   WRITE (IU06,*) ' *          =================================             *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' * GRID SPECIFICATINOS ARE NOT CONSISTENT                 *'
   WRITE (IU06,*) ' * USER PROVIDED GRID DEFINITIONS ARE:                    *'
   WRITE (IU06,*) ' * LONGITUDE    WEST = ', WEST
   WRITE (IU06,*) ' * LONGITUDE    EAST = ', EAST
   WRITE (IU06,*) ' * LONGITUDE INCREMENT D_LON = ', D_LON
   WRITE (IU06,*) ' * NO. OF LONGITUDES   N_LON = ', N_LON
   WRITE (IU06,*) ' * LATITUDE    NORTH = ', NORTH
   WRITE (IU06,*) ' * LATITUDE    SOUTH = ', SOUTH
   WRITE (IU06,*) ' * LATITUDE  INCREMENT D_LAT = ', D_LAT
   WRITE (IU06,*) ' * NO. OF LATITUDE     N_LAT = ', N_LAT
   WRITE (IU06,*) ' * COMPUTED GRID SIZES ARE:                               *'
   WRITE (IU06,*) ' * NO. OF LONGITUDES      NX = ', NX
   WRITE (IU06,*) ' * NO. OF LATITUDE        NY = ', NY
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' **********************************************************'
   NX = -1
   NY = -1
   CALL ABORT1
END IF
IF (ABS(AMOWEP+(NX-1)*XDELLO-AMOEAP).GT.EPS  .OR.                             &
    ABS(AMOSOP+(NY-1)*XDELLA-AMONOP).GT.EPS) THEN
   WRITE (IU06,*) ' **********************************************************'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *          FATAL  ERROR IN SUB. SET_GRID_DEF             *'
   WRITE (IU06,*) ' *          =================================             *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' * ERROR IN GRID SPECIFICATIONS.                          *'
   WRITE (IU06,*) ' * EAST AND WEST LONGITUDE ARE NOT CONSISTENT WITH        *'
   WRITE (IU06,*) ' * LONGITUDE INCREMENT OR                                 *'
   WRITE (IU06,*) ' * SOUTH AND NORTH LATITUDE ARE NOT CONSISTENT WITH       *'
   WRITE (IU06,*) ' * LATITUDE INCREMENT.                                    *'
   WRITE (IU06,*) ' * LONGITUDE    WEST = ', WEST
   WRITE (IU06,*) ' * LONGITUDE    EAST = ', EAST
   WRITE (IU06,*) ' * LONGITUDE INCREMENT D_LON = ', D_LON
   WRITE (IU06,*) ' * NO. OF LONGITUDES   N_LON = ', N_LON
   WRITE (IU06,*) ' * LATITUDE    NORTH = ', NORTH
   WRITE (IU06,*) ' * LATITUDE    SOUTH = ', SOUTH
   WRITE (IU06,*) ' * LATITUDE  INCREMENT D_LAT = ', D_LAT
   WRITE (IU06,*) ' * NO. OF LATITUDE     N_LAT = ', N_LAT
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' **********************************************************'
   NX = -1
   NY = -1
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. INPUT MODULE DATA ARE DEFINED.                                        !
!        ------------------------------                                        !

SET_STATUS = (NX.GT.0 .AND. NY.GT.0 .AND. NX_T.GT.0 .AND. NY_T.GT.0)

END SUBROUTINE SET_GRID_DEF

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_HEADER (TEXT)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

CHARACTER (LEN=*), INTENT(IN)  :: TEXT      !! MODEL HEADER.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COPY HEADER DEFINITION.                                               !
!        -----------------------                                               !

HEADER = TEXT(1:MIN(LEN_TRIM(TEXT), LEN(HEADER)))

END SUBROUTINE SET_HEADER

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_TOPOGRAPHY (N_LON, N_LAT, D_LON, D_LAT,                         &
&                          SOUTH, NORTH, WEST, EAST, D_MAP)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER, INTENT(IN)  :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
INTEGER, INTENT(IN)  :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
REAL   , INTENT(IN)  :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].
REAL   , INTENT(IN)  :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].
REAL   , INTENT(IN)  :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].
REAL   , INTENT(IN)  :: NORTH      !! NORTH LATITUDE OF GRID [DEG].
REAL   , INTENT(IN)  :: WEST       !! WEST LONGITUDE OF GRID [DEG].
REAL   , INTENT(IN)  :: EAST       !! EAST LONGITUDE OF GRID [DEG].
INTEGER, INTENT(IN)  :: D_MAP(1:N_LON,1:N_LAT) !! WATER DEPTH [M].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CLEAR INPUT TOPOGRAPHY.                                               !
!        -----------------------                                               !

NX_T = -1
NY_T = -1
IF (ALLOCATED(GRID_IN)) DEALLOCATE(GRID_IN)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COPY GRID DEFINITIONS.                                                !
!        ----------------------                                                !

DY_T = D_LAT        !! LATITUDE INCREMENT [DEG].
DX_T = D_LON        !! LONGITUDE INCREMENT [DEG].
SOUTH_T = SOUTH     !! SOUTH LATITUDE OF GRID [DEG].
NORTH_T = NORTH     !! NORTH LATITUDE OF GRID [DEG].
WEST_T = WEST       !! WEST LONGITUDE OF GRID [DEG].
EAST_T = EAST       !! EAST LONGITUDE OF GRID [DEG].
CALL ADJUST (WEST_T, EAST_T)
NX_T = NINT((EAST_T-WEST_T)/DX_T+1.0)          !! NO. OF LONGITUDES
NY_T = NINT((NORTH_T-SOUTH_T)/DY_T+1.0)        !! NO. OF LATITUDES
PER_T = ABS(EAST+D_LON-360.-WEST).LT.EPS       !! PERIODIC?

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. CHECK CONSISTENCY.                                                    !
!        ------------------                                                    !

IF (N_LON.NE.NX_T .OR. N_LAT.NE.NY_T .OR.                                      &
&   N_LON.NE.SIZE(D_MAP,1) .OR. N_LAT.NE.SIZE(D_MAP,2)) THEN
   WRITE (IU06,*) ' **********************************************************'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *         FATAL  ERROR IN SUB. SET_TOPOGRAPHY            *'
   WRITE (IU06,*) ' *         ===================================            *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' * GRID SPECIFICATINOS ARE NOT CONSISTENT                 *'
   WRITE (IU06,*) ' * USER PROVIDED GRID DEFINITIONS ARE:                    *'
   WRITE (IU06,*) ' * LONGITUDE    WEST = ', WEST
   WRITE (IU06,*) ' * LONGITUDE    EAST = ', EAST
   WRITE (IU06,*) ' * LONGITUDE INCREMENT D_LON = ', D_LON
   WRITE (IU06,*) ' * NO. OF LONGITUDES   N_LON = ', N_LON
   WRITE (IU06,*) ' * LATITUDE    NORTH = ', NORTH
   WRITE (IU06,*) ' * LATITUDE    SOUTH = ', SOUTH
   WRITE (IU06,*) ' * LATITUDE  INCREMENT D_LAT = ', D_LAT
   WRITE (IU06,*) ' * NO. OF LATITUDE     N_LAT = ', N_LAT
   WRITE (IU06,*) ' * COMPUTED GRID SIZES ARE:                               *'
   WRITE (IU06,*) ' * NO. OF LONGITUDES    NX_T = ', NX_T
   WRITE (IU06,*) ' * NO. OF LATITUDE      NY_T = ', NY_T
   WRITE (IU06,*) ' * DIMENSIONS OF DEPTH MAP ARRAY ARE :                    *'
   WRITE (IU06,*) ' * NO. OF LONGITUDES 1. DIMENSION = ', SIZE(D_MAP,1)
   WRITE (IU06,*) ' * NO. OF LATITUDE   2. DIMENSION = ', SIZE(D_MAP,2)
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                        *'
   WRITE (IU06,*) ' **********************************************************'
   NX_T = -1
   NY_T = -1
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. COPY DEPTH FIELD.                                                     !
!        -----------------                                                     !

ALLOCATE (GRID_IN(NX_T,NY_T))
GRID_IN = D_MAP

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. INPUT MODULE DATA ARE DEFINED.                                        !
!        ------------------------------                                        !

SET_STATUS = (NX.GT.0 .AND. NY.GT.0 .AND. NX_T.GT.0 .AND. NY_T.GT.0)

END SUBROUTINE SET_TOPOGRAPHY

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PREPARE_GRID

! ---------------------------------------------------------------------------- !
!                                                                              !
!   PREPARE_GRID - ROUTINE TO ARRANGE WAMODEL GRID.                            !
!                                                                              !
!     H.GUNTHER            ECMWF       04/04/1990                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       TO ARRANGE WAMODEL GRID FOR A GIVEN AREA AND COMPUTE VARIOUS           !
!       MODEL CONSTANTS.                                                       !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE MODEL GRID AREA IS EXTRACTED FROM THE INPUT TOPOGRAGHY.            !
!       THE NEAREST GRID POINT IS TAKEN FOR DEPTH INTERPOLATION.               !
!       LAND POINTS ARE REMOVED FROM THE GRID.                                 !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER              :: I, K, IP
REAL                 :: XLAT

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CLEAR MODEL GRID AND CHECK MODULE STATUS.                             !
!        -----------------------------------------                             !

NSEA = -1
IF (ALLOCATED (SINPH))  DEALLOCATE (SINPH)
IF (ALLOCATED (COSPH))  DEALLOCATE (COSPH)
IF (ALLOCATED (DEPTH))  DEALLOCATE (DEPTH)
IF (ALLOCATED (L_S_MASK))  DEALLOCATE (L_S_MASK)

IF (ALLOCATED (IXLG))   DEALLOCATE (IXLG)
IF (ALLOCATED (KXLT))   DEALLOCATE (KXLT)
IF (ALLOCATED (KLAT))   DEALLOCATE (KLAT)
IF (ALLOCATED (KLON))   DEALLOCATE (KLON)
IF (ALLOCATED (GRD))    DEALLOCATE (GRD)

IF (.NOT.SET_STATUS) THEN
   WRITE (IU06,*) ' ********************************************************'
   WRITE (IU06,*) ' *                                                      *'
   WRITE (IU06,*) ' *          FATAL  ERROR IN SUB. PREPARE_GRID           *'
   WRITE (IU06,*) ' *          =================================           *'
   IF (NX_T .EQ. -1 .OR. NY_T .EQ. -1) THEN
      WRITE (IU06,*) ' *                                                      *'
      WRITE (IU06,*) ' * TOPOGRAPHY INPUT DATA ARE NOT DEFINED IN GRID MODULE.*'
      WRITE (IU06,*) ' * DATA MUST BE DEFINED BY SUB. SET_TOPOGRAPHY.         *'
   END IF
   IF (NX .EQ. -1 .OR. NY .EQ. -1) THEN
      WRITE (IU06,*) ' *                                                      *'
      WRITE (IU06,*) ' * MODEL GRID DIMENISIONS ARE NOT DEFINED GRID MODULE.  *'
      WRITE (IU06,*) ' * DATA MUST BE DEFINED BY SUB. SET_GRID_DEF.           *'
   END IF
   WRITE (IU06,*) ' *                                                      *'
   WRITE (IU06,*) ' *           PROGRAM ABORTS  PROGRAM ABORTS             *'
   WRITE (IU06,*) ' *                                                      *'
   WRITE (IU06,*) ' ********************************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!    2. INITIALISE.                                                            !
!       -----------                                                            !

DELPHI = XDELLA*CIRC/360.  !! LATITUDE INCREMENT IN  METRES.
DELLAM = XDELLO*CIRC/360.  !! LONGITUDE INCREMENT IN  METRES.

IPER = ABS(MOD(AMOEAP-AMOWEP+XDELLO+720., 360.)).LT.EPS !! PERIODIC MODEL GRID?

WHERE (GRID_IN.LE.0) GRID_IN = -999  !! MARK LAND BY -999

! ---------------------------------------------------------------------------- !
!                                                                              !
!    3. INITIALISE SIN AND COS OF LATITUDES.                                   !
!       ------------------------------------                                   !

ALLOCATE (SINPH(1:NY))
ALLOCATE (COSPH(1:NY))

DO K = 1,NY
   XLAT = (AMOSOP + REAL(K-1)*XDELLA)*RAD
   SINPH(K)   = SIN(XLAT)
   COSPH(K)   = COS(XLAT)
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!    4. ARRANGE THE TOPOGRAPHYON REQUESTED MODEL AREA AND RESOLUTION.          !
!       -------------------------------------------------------------          !

CALL SUBGRID_TOPOGRAPHY

IF (ITEST.GT.1) WRITE (IU06,*) ' SUB SUBGRID_TOPOGRAPHY DONE'

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. GENERATE LAND-SEA MASK AND COUNT NUMBER OF SEA POINTS.                !
!        ------------------------------------------------------                !

ALLOCATE (L_S_MASK(1:NX,1:NY))
L_S_MASK = GRD.GT.-990
NSEA = COUNT (L_S_MASK)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. COMPUTE ARRAYS TO MAP POINT INDEX TO GRID POINT INDEX.                !
!        ------------------------------------------------------                !

ALLOCATE (IXLG (1:NSEA))
ALLOCATE (KXLT (1:NSEA))

IXLG = 0
KXLT = 0

IP = 0
DO K = 1,NY
   DO I = 1,NX
      IF (L_S_MASK(I,K)) THEN
         IP = IP+1
         IXLG(IP) = I
         KXLT(IP) = K
      END IF
   END DO
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. REMOVE LAND POINT FROM DEPTH ARRAY.                                   !
!        -----------------------------------                                   !

ALLOCATE (DEPTH(1:NSEA))
DEPTH = PACK (GRD, L_S_MASK)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. COMPUTE INDICES OF NEIGHBOUR SEA POINTS.                              !
!        ----------------------------------------                              !

ALLOCATE (KLAT(1:NSEA,1:2))   !! NEXT POINTS ON LATITUDE.
ALLOCATE (KLON(1:NSEA,1:2))   !! NEXT POINTS ON LONGITUDE.
KLAT = 0
KLON = 0

IP = 0
DO K = 1,NY
   DO I = 1,NX
      IF ( .NOT.L_S_MASK(I,K) ) CYCLE
      IP = IP+1

!     8.1 WEST LONGITUDE NEIGHBOURS.                                           !

      IF (I.GT.1) THEN
         IF (L_S_MASK(I-1,K)) KLON(IP,1) = IP-1
      ELSE
         IF (IPER.AND.L_S_MASK(NX,K)) KLON(IP,1) = IP+COUNT(L_S_MASK(2:NX,K))
      END IF

!     8.2 EAST LONGITUDE NEIGHBOURS.                                           !

      IF (I.LT.NX) THEN
         IF (L_S_MASK(I+1,K)) KLON(IP,2) = IP+1
      ELSE
         IF (IPER.AND.L_S_MASK(1,K)) KLON(IP,2) = IP-COUNT(L_S_MASK(1:NX-1,K))
      END IF

!     8.3 SOUTH LATITUDE NEIGHBOURS.                                           !

      IF (K.GT.1) THEN
         IF (L_S_MASK(I,K-1)) THEN
            KLAT(IP,1) = IP+1-COUNT(L_S_MASK(1:I,K))-COUNT(L_S_MASK(I:NX,K-1))
         END IF
      END IF

!     8.4 NORTH LATITUDE NEIGHBOURS.                                           !

      IF (K.LT.NY) THEN
         IF (L_S_MASK(I,K+1)) THEN
            KLAT(IP,2) = IP-1+COUNT(L_S_MASK(I:NX,K))+COUNT(L_S_MASK(1:I,K+1))
         END IF
      END IF
   END DO
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     9. CLEAR MODULE AND SET STATUS TO INITILIZED.                            !
!        ------------------------------------------                            !

NX_T = -1
NY_T = -1
IF (ALLOCATED(GRID_IN)) DEALLOCATE (GRID_IN)
N_CA = 0
IF (ALLOCATED(SOUTH_CA)) DEALLOCATE(SOUTH_CA)
IF (ALLOCATED(NORTH_CA)) DEALLOCATE(NORTH_CA)
IF (ALLOCATED(WEST_CA )) DEALLOCATE(WEST_CA)
IF (ALLOCATED(EAST_CA )) DEALLOCATE(EAST_CA)
IF (ALLOCATED(DEPTH_CA)) DEALLOCATE(DEPTH_CA)

IF (ALLOCATED (GRD))     DEALLOCATE (GRD)

END SUBROUTINE PREPARE_GRID

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_GRID_STATUS

! ---------------------------------------------------------------------------- !
!                                                                              !
!   PRINT_GRID_STATUS - PRINT STATUS OF GRID MODULE.                           !
!                                                                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       MAKE A PRINTER OUTPUT OF THE DATA, WHICH ARE SAVED IN WAM_GRID_MODULE. !
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
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER             :: I
REAL                :: GRID(NX,NY)   !! ARRAY FOR GRIDDED PRINT OUTPUT.
CHARACTER (LEN=100) :: TITL
CHARACTER (LEN=14)  :: ZERO = ' '


! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. PRINT STATUS.                                                         !
!        -------------                                                         !

WRITE (IU06,'(/,'' ----------------------------------------'')')
WRITE (IU06,'(  ''            GRID MODULE STATUS'')')
WRITE (IU06,'(  '' ----------------------------------------'')')
WRITE(IU06,*) ' '
WRITE(IU06,*) ' GRID HEADER: ', HEADER
WRITE(IU06,*) ' '

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. INPUT GRID DEFINITIONS.                                               !
!        -----------------------                                               !

IF (NX_T.GT.0 .AND. NY_T.GT.0) THEN
   WRITE (IU06,'('' TOPOGRAPHY INPUT GRID: '')')
   WRITE (IU06,'('' NUMBER OF LONGITUDES IS      NX_T = '',I5)') NX_T
   WRITE (IU06,'('' MOST WESTERN LONGITUDE IS  WEST_T = '',F9.4)') WEST_T
   WRITE (IU06,'('' MOST EASTERN LONGITUDE IS  EAST_T = '',F9.4)') EAST_T
   WRITE (IU06,'('' LATITUDE INCREMENT IS        DX_T = '',F9.4)') DX_T
   WRITE (IU06,'('' NUMBER OF LATITUDES IS       NY_T = '',I5)') NY_T
   WRITE (IU06,'('' MOST SOUTHERN LATITUDE IS SOUTH_T = '',F9.4)') SOUTH_T
   WRITE (IU06,'('' MOST NORTHERN LATITUDE IS NORTH_T = '',F9.4)') NORTH_T
   WRITE (IU06,'('' LATITUDE INCREMENT IS        DY_T = '',F9.4)') DY_T
   IF (PER_T) THEN
      WRITE (IU06,*) 'THE GRID IS EAST-WEST PERIODIC'
   ELSE
      WRITE (IU06,*) 'THE GRID IS NOT EAST-WEST PERIODIC'
   END IF
ELSE
   WRITE (IU06,*) ' TOPOGRAPHY INPUT GRID IS NOT DEFINED'
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. GRID CORRECTIONS.                                                     !
!        -----------------                                                     !

IF (N_CA.GT.0) THEN
   WRITE (IU06,'(/4X,'' AREAS TO BE CORRECTED IN OUTPUT GRID'',                &
&               /,4X,''  NO.   SOUTHERN LAT  NORTHERN LAT  WESTERN LONG '',    &
&                    '' EASTERN LONG  DEPTH'')')
   DO I = 1,N_CA
     WRITE (IU06,'(4X,I5,1X,4F14.3,I7 )') I, SOUTH_CA(I), NORTH_CA(I),         &
&                                         WEST_CA(I), EAST_CA(I), DEPTH_CA(I)
   END DO
ELSE
   WRITE (IU06,*) ' CORRECTION AREAS ARE NOT DEFINED'
END IF


! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. MODEL GRID DEFINITIONS.                                               !
!        -----------------------                                               !

IF (NX.GT.0 .AND. NY.GT.0) THEN
   WRITE (IU06,'(/,'' MODEL GRID: '')')
   WRITE (IU06,'('' NUMBER OF LONGITUDES IS       NX = '',I5)') NX
   WRITE (IU06,'('' MOST WESTERN LONGITUDE IS  AMOWEP = '',F9.4)') AMOWEP
   WRITE (IU06,'('' MOST EASTERN LONGITUDE IS  AMOEAP = '',F9.4)') AMOEAP
   WRITE (IU06,'('' LATITUDE INCREMENT IS      XDELLO = '',F9.4)') XDELLO
   WRITE (IU06,'('' NUMBER OF LATITUDES IS         NY = '',I5)') NY
   WRITE (IU06,'('' MOST SOUTHERN LATITUDE IS  AMOSOP = '',F9.4)') AMOSOP
   WRITE (IU06,'('' MOST NORTHERN LATITUDE IS  AMONOP = '',F9.4)') AMONOP
   WRITE (IU06,'('' LATITUDE INCREMENT IS      XDELLA = '',F9.4)') XDELLA
   IF (ONE_POINT) THEN
      WRITE (IU06,*) 'THIS A ONE POINT GRID: PROPAGATION IS NOT DONE'
   ELSE
      IF (IPER) THEN
         WRITE (IU06,*) 'THE GRID IS EAST-WEST PERIODIC'
      ELSE
         WRITE (IU06,*) 'THE GRID IS NOT EAST-WEST PERIODIC'
      END IF
   END IF
ELSE
   WRITE (IU06,*) ' MODEL GRID DEFINITIONS ARE NOT DEFINED'
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. OUTPUT OF DEPTH FIELD.                                                !
!        ----------------------                                                !
  
IF (NSEA.GT.0) THEN
   WRITE (IU06,'('' NUMBER OF SEAPOINTS IS       NSEA = '',I5)') NSEA
   GRID=UNPACK(MIN(DEPTH,999.),L_S_MASK,99999.)
   WRITE (IU06,*) '              '
   TITL = 'WATER DEPTH IN METERS. (DEPTH DEEPER THAN 999M ARE PRINTED AS 999)'
   CALL PRINT_ARRAY (IU06, ZERO, TITL, GRID, AMOWEP, AMOSOP, AMOEAP, AMONOP)
ELSE
   WRITE (IU06,*) '  MODULE DATA ARE NOT PREPARED'
END IF

END SUBROUTINE PRINT_GRID_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE FIND_SEA_POINT (NUMBER, LATITUDE, LONGITUDE, POINT_NO)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   FIND_SEA_POINT - FIND SEA POINT NUMBER.                                    !
!                                                                              !
!     R. PORTZ     MPI         15/01/1991                                      !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       FIND SEA POINT NUMBERS FOR A GIVEN ARRAY OF LONGITUDES AND LATITUDES.  !
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
! 
!     INTERFACE VARIABLES.
!     --------------------

INTEGER, INTENT(IN)    :: NUMBER       !! NUMBER OF POINTS IN ARRAYS.
REAL,    INTENT(IN)    :: LATITUDE(*)  !! INPUT LATITUDES.
REAL,    INTENT(IN)    :: LONGITUDE(*) !! INPUT LONGITUDES.
INTEGER, INTENT(OUT)   :: POINT_NO(*)  !! OUTPUT SEA POINT NUMBERS.

! ---------------------------------------------------------------------------- !
!
!     LOCAL VARIABLES.
!     ----------------

INTEGER  :: IO, IOLT, IOLG

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. LOOP OVER INPUT LATITUDES, LONGITUDES.                                !
!        --------------------------------------                                !

POINT_NO(1:NUMBER) = 0
POINT: DO IO = 1,NUMBER

!     1.1 COMPUTE GRID MATRIX INDICES.                                         !
!         ----------------------------                                         !

   IOLT = NINT((LATITUDE(IO)-AMOSOP)/XDELLA+1.0)
   IF (IOLT.LT.1.OR.IOLT.GT.NY) CYCLE POINT
   IOLG = NINT(MOD(LONGITUDE(IO)-AMOWEP+720.,360.)/XDELLO+1.0)
   IF (IOLG.EQ.NX+1 .AND. IPER) IOLG = 1

!     1.2 IF SEA POINT FIND SEA POINT NUMBER.                                  !
!         -----------------------------------                                  !

   IF (IOLG.LT.1.OR.IOLG.GT.NX) CYCLE POINT
   IF (.NOT. L_S_MASK(IOLG,IOLT)) CYCLE POINT
   POINT_NO(IO) = COUNT(L_S_MASK(1:IOLG,IOLT))
   IF (IOLT.GT.1) POINT_NO(IO) = POINT_NO(IO) + COUNT(L_S_MASK(1:NX,1:IOLT-1))

END DO POINT

END SUBROUTINE FIND_SEA_POINT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE INTERPOLATION_TO_GRID (IU06, IGPER, DLAM, DPHI, RLONL, RLATS,       &
&                                 U, US, V, VS)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   INTERPOLATION_TO_GRID - INTERPOLATES TO MODEL GRID POINTS.                 !
!                                                                              !
!     H. GUNTHER    GKSS  DECEMBER 2001.                                       !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!        LOCATE AND INTERPOLATE IN INPUT GRID.                                 !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       DOUBLE LINEAR INTERPOLATION IN INPUT GRID. OPTIONAL A SECOND INPUT     !
!       CAN BE INTERPOLATED AT THE SAME CALL.                                  !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.
!     --------------------

INTEGER, INTENT(IN)  :: IU06   !! PRINTER OUTPUT UNIT.
LOGICAL, INTENT(IN)  :: IGPER  !! .TRUE. = PERIODIC INPUT GRID 
                               !! OTHERWISE NON-PERIODICAL
REAL, INTENT(IN)  :: DLAM      !! STEPS BETWEEN LONGITUDES INPUT (DEG).
REAL, INTENT(IN)  :: DPHI      !! STEPS BETWEEN LATITUDES INPUT (DEG).
REAL, INTENT(IN)  :: RLATS     !! SOUTHERN LATITUDE OF INPUT (DEG).
REAL, INTENT(IN)  :: RLONL     !! WESTERN LONGITUDE OF INPUT (DEG).
REAL, INTENT(IN)  :: U(:,:)          !! INPUT FIELD.
REAL, INTENT(OUT) :: US(:)           !! SPACE INTERPOLATED OUTPUT FIELD.

REAL, INTENT(IN), OPTIONAL :: V(:,:) !! OPTIONAL SECOND INPUT FIELD.
REAL, INTENT(OUT),OPTIONAL :: VS(:)  !! OPTIONAL SECOND OUTPUT FIELD.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.
!     ----------------

INTEGER :: NC, NR, NSEA
INTEGER :: IJ, I1(1:SIZE(US)), I2(1:SIZE(US)), K1(1:SIZE(US)), K2(1:SIZE(US))
REAL    :: DI(1:SIZE(US)), DK(1:SIZE(US))

NC = SIZE(U,1)
NR = SIZE(U,2)
NSEA = SIZE(US)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. TRANSFORM MODEL COORDINATES TO INPUT GRID.                            !
!        ------------------------------------------                            !

DI = AMOWEP + REAL(IXLG-1)*XDELLO-RLONL
DI = MOD(DI+720.,360.)
DI = DI/DLAM+1.00001

DK = AMOSOP + REAL(KXLT-1)*XDELLA-RLATS
DK = DK/DPHI+1.00001

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTE CORNER POINT INDICES IN INPUT GRID.                           !
!        -------------------------------------------                           !

I1  = DI
K1  = DK
K2  = MIN(NR,K1+1)
I2  = I1+1

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. DISTANCES OF INTERPOLATION POINT FROM LOW LEFT CORNER POINT.          !
!        ------------------------------------------------------------          !

DI = DI-I1
DK = DK-K1

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. CORRECTIONOF FIRST AND LAST GRID LINES (PERIODIC OR UNPERIODIC GRID). !
!        --------------------------------------------------------------------- !

IF (IGPER) THEN
   WHERE (I1.EQ.NC) I2 = 1
   WHERE (I1.EQ.0 ) I1 = NC
ELSE
   WHERE (I1.EQ.NC) I2 = NC
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. CHECK WHETHER POINTS ARE IN GRID.                                     !
!        ---------------------------------                                     !

IF (MINVAL(I1).LT.1 .OR. MAXVAL(I1).GT.NC .OR.                                 &
&   MINVAL(K1).LT.1 .OR. MAXVAL(K1).GT.NR) THEN
    WRITE(IU06,*) ' *******************************************'
    WRITE(IU06,*) ' *                                         *'
    WRITE(IU06,*) ' *  FATAL ERROR IN INTERPOLATION_TO_GRID   *'
    WRITE(IU06,*) ' *  ====================================   *'
    WRITE(IU06,*) ' * POINT IS OUTSIDE OF INPUT GRID          *'
    WRITE(IU06,*) ' * DIMENSION OF INPUT GRID IS   NC = ', NC
    WRITE(IU06,*) ' *                              NR = ', NR
    WRITE(IU06,*) ' * MIN AND MAX OF INDEX ARE                *'
    WRITE(IU06,*) ' * I1:  MIN, MAX = ', MINVAL(I1), MAXVAL(I1)
    WRITE(IU06,*) ' * I2:  MIN, MAX = ', MINVAL(I2), MAXVAL(I2)
    WRITE(IU06,*) ' * K1:  MIN, MAX = ', MINVAL(K1), MAXVAL(K1)
    WRITE(IU06,*) ' * K2:  MIN, MAX = ', MINVAL(K2), MAXVAL(K2)
    WRITE(IU06,*) ' *                                         *'
    WRITE(IU06,*) ' *  PROGRAM ABORTS     PROGRAM ABORTS      *'
    WRITE(IU06,*) ' *                                         *'
    WRITE(IU06,*) ' *******************************************'
    CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. LINEAR INTERPOLATION.                                                 !
!        ----------------------                                                !

!     6.1 FIRST FIELD.

US = 0.
DO IJ = 1,NSEA
   US(IJ) = (U(I1(IJ),K1(IJ))*(1.-DI(IJ))+U(I2(IJ),K1(IJ))*DI(IJ))*(1.-DK(IJ)) &
&         + (U(I1(IJ),K2(IJ))*(1.-DI(IJ))+U(I2(IJ),K2(IJ))*DI(IJ))*DK(IJ)
END DO

!     6.2 OPTIONAL SECOND FIELD.

IF (PRESENT(V) .AND. PRESENT(VS)) THEN
   VS = 0.
   DO IJ = 1,NSEA
      VS(IJ) = (V(I1(IJ),K1(IJ))*(1.-DI(IJ))+V(I2(IJ),K1(IJ))*DI(IJ))         &
&                                                                *(1.-DK(IJ)) &
&            + (V(I1(IJ),K2(IJ))*(1.-DI(IJ))+V(I2(IJ),K2(IJ))*DI(IJ))*DK(IJ)
   END DO
END IF      

END SUBROUTINE INTERPOLATION_TO_GRID

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PRIVATE MODULE PROCEDURES.                                            !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SUBGRID_TOPOGRAPHY

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SUBGRID_TOPOGRAPHY - ARRANGE SUBGRID TOPOGRAPHY.                           !
!                                                                              !
!     S. HASSELMANN     MPIFM           1/6/86.                                !
!                                                                              !
!     MODIFIED BY       H. GUNTHER      1/4/90  -  REARANGEMENT OF CODE.       !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO CONVERT THE INPUT GRID TO THE MODEL GRID.                           !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE TOPOGRAPHIC DATA ARE PUT ON THE REQUESTED SUBGRID LAT-LONG         !
!       RESOLUTION, ALWAYS USING THE NEAREST POINT.                            !
!       FINALLY THE SUBGRID TOPOGRAPHY MAY BE ADJUSTED.                        !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER :: I, K, L
INTEGER :: IH(NX), KH(NY)
REAL    :: XLAT, XLON

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. STORING TOPOGRAPHIC DATA AT LONGITUDES AND LATITUDES OF GRID AREA.    !
!        ------------------------------------------------------------------    !

IF (.NOT.ALLOCATED(GRD)) ALLOCATE(GRD(NX,NY))

GRD(1:NX,1:NY) = 999

DO I = 1,NX
   IF (PER_T) THEN
      IH(I) = INT( (AMOWEP + REAL(I-1)*XDELLO+360. - WEST_T)/DX_T )
      IH(I) = MOD(IH(I)+NX_T,NX_T) + 1
   ELSE
      IH(I) = NINT( (AMOWEP + REAL(I-1)*XDELLO - WEST_T)/DX_T ) + 1
   END IF
END DO
DO K = 1,NY
   KH(K) = NINT( (AMOSOP + REAL(K-1)*XDELLA - SOUTH_T)/DY_T ) + 1
END DO

IF (MINVAL(IH).LT.1 .OR. MAXVAL(IH).GT.NX_T .OR.                               &
&   MINVAL(KH).LT.1 .OR. MAXVAL(KH).GT.NY_T           ) THEN
   WRITE (IU06,*) ' *****************************************************'
   WRITE (IU06,*) ' *                                                   *'
   WRITE (IU06,*) ' *     FATAL  ERROR IN SUB. SUBGRID_TOPOGRAPHY       *'
   WRITE (IU06,*) ' *     =======================================       *'
   WRITE (IU06,*) ' *                                                   *'
   WRITE (IU06,*) ' * INPUT TOPOGRAPHY DOES NOT FIT TO REQUESTED GRID.  *'
   WRITE (IU06,*) ' *                                                   *'
   WRITE (IU06,*) ' *      PROGRAM ABORTS  PROGRAM ABORTS               *'
   WRITE (IU06,*) ' *                                                   *'
   WRITE (IU06,*) ' *****************************************************'
   CALL ABORT1
END IF

GRD(1:NX,1:NY) = GRID_IN(IH(1:NX),KH(1:NY))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. MANUAL ADJUSTMENT OF TOPOGRAPHY.                                      !
!        --------------------------------                                      !

IF (N_CA.GT.0) THEN
   DO K = 1,NY
      XLAT = AMOSOP+REAL(K-1)*XDELLA
      DO I = 1,NX
         XLON = AMOWEP+REAL(I-1)*XDELLO
         IF (XLON.GE.360.) XLON = XLON-360.
         DO L = 1,N_CA
            IF (XLON.LT.WEST_CA(L)) XLON = XLON+360.
            IF (XLON.GT.EAST_CA(L)) XLON = XLON-360.
            IF (XLON.GE.WEST_CA(L) .AND. XLAT.GE.SOUTH_CA(L) .AND.                 &
&               XLON.LE.EAST_CA(L) .AND. XLAT.LE.NORTH_CA(L))                  &
&                       GRD(I,K) = DEPTH_CA(L)
         END DO
      END DO
   END DO
END IF

END SUBROUTINE SUBGRID_TOPOGRAPHY

END MODULE WAM_GRID_MODULE
