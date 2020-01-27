MODULE WAM_NEST_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE STORES THE BOUNDARY INPUT VALUES FOR A FINE GRID RUN.          !
!   THE VALUES WERE PRODUCED BY A PREVIOUS COARSE GRID RUN.                    !
!                                                                              !
! ---------------------------------------------------------------------------- !

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  METHODS FROM BASIC MODULES.                                          !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  &  !! TERMINATES PROCESSING.
&       ADJUST,                  &  !! ADJUST LONGITUDES.
&       CHECK_MULTIPLE,          &  !! CHECK INTEGER MULTIPLE
&       PRINT_ARRAY                 !! PRINTS AN ARRAY.

USE WAM_GRID_MODULE,      ONLY:  &
&       FIND_SEA_POINT              !! FIND SEA POINT NUMBERS.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B.  DATA FROM BASIC MODULES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

USE WAM_FILE_MODULE,      ONLY: IU06, ITEST, IU10, FILE10

USE WAM_GENERAL_MODULE,   ONLY: ZPI

USE WAM_GRID_MODULE,      ONLY: NX, NY, NSEA, XDELLA, XDELLO, L_S_MASK,        &
&                               AMOWEP, AMOSOP, AMOEAP, AMONOP, IPER, IXLG,    &
&                               KXLT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C.  MODULE DATA.                                                         !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

IMPLICIT NONE

CHARACTER (LEN=11) , PARAMETER :: MODULE_NAME = 'NEST_MODULE'

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. BOUNDARY OPTIONS.                                                     !
!        -----------------                                                     !

LOGICAL      :: COARSE = .FALSE.  !! = .TRUE. IF COARSE GRID RUN.
LOGICAL      :: FINE   = .FALSE.  !! = .TRUE. IF FINE GRID RUN.

! ---------------------------------------------------------------------------- !
!
!    2. COURSE GRID NEST ORGANIZATION.

INTEGER, SAVE                   :: N_NEST = 0 !! NUMBER OF NESTS
INTEGER, SAVE                   :: MAX_NEST=0 !! MAXIMUM NUMBER OF NEST POINTS.
INTEGER,            ALLOCATABLE :: NBOUNC(:)  !! NUMBERS OF NEST POINTS.
REAL,               ALLOCATABLE :: N_SOUTH(:) !! MOST SOUTHERN LAT. OF NESTS.
REAL,               ALLOCATABLE :: N_NORTH(:) !! MOST NORTHERN LAT. OF NESTS.
REAL,               ALLOCATABLE :: N_WEST(:)  !! MOST WESTERN LONG. OF NESTS.
REAL,               ALLOCATABLE :: N_EAST(:)  !! MOST EASTERN LONG. OF NESTS.
CHARACTER (LEN=20), ALLOCATABLE :: N_NAME(:)  !! NAMES OF NESTS.


INTEGER,      ALLOCATABLE :: IJARC(:,:) !! SEA POINT INDEX OF NEST POINTS.
REAL,         ALLOCATABLE :: BLATC(:,:) !! LAT.  OF NEST POINTS.
REAL,         ALLOCATABLE :: BLNGC(:,:) !! LONG.  OF NEST POINTS.
                                        !! FIRST INDEX IS POINT NUMBER 
                                        !! SECOND INDEX IS NEST NUMBER 

! ---------------------------------------------------------------------------- !
!
!    3. FINE GRID ORGANIZATION THE BOUNDARY POINTS.

INTEGER            :: NBOUNF = 0   !! NUMBER OF FINE GRID BOUNDARY POINTS.
INTEGER            :: NBINP  = 0   !! NUMBER OF INPUT BOUNDARY POINTS.
CHARACTER (LEN=20) :: C_NAME       !! NAME OF NEST GIVEN IN COARSE GRID.
REAL               :: DLAMAC = -1. !! LONGITUDE INCREMENT OF COARSE GRID (DEG).
REAL               :: DPHIAC = -1. !! LATITUDE INCREMENT OF COARSE GRID (DEG).

INTEGER,ALLOCATABLE, DIMENSION(:) :: IJARF !! POINT INDEX OF BOUNDARY POINT. 
INTEGER,ALLOCATABLE, DIMENSION(:) :: IBFL  !! INDEX OF LEFT 
                                           !! COARSE GRID OUTPUT POINT.
INTEGER,ALLOCATABLE, DIMENSION(:) :: IBFR  !! INDEX OF RIGHT
                                           !! COARSE GRID OUTPUT POINT.
REAL,   ALLOCATABLE, DIMENSION(:) :: BFW   !! SPACE INTERPOLATION WEIGHT.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

INTERFACE SET_NEST                   !! TRANSFERS NEST (COARSE) DEFINITION 
   MODULE PROCEDURE SET_NEST
END INTERFACE
PUBLIC SET_NEST

INTERFACE PREPARE_BOUNDARY           !! CHECKS THE BOUNDARY OPTION.
   MODULE PROCEDURE PREPARE_BOUNDARY 
END INTERFACE
PUBLIC PREPARE_BOUNDARY

INTERFACE SET_BOUNDARY_OPTION         !! SETS THE BOUNDARY OPTION.
   MODULE PROCEDURE SET_BOUNDARY_OPTION
END INTERFACE
PUBLIC SET_BOUNDARY_OPTION

INTERFACE PRINT_NEST_STATUS       !! PRINTS THIS MODULE STATUS.
   MODULE PROCEDURE PRINT_NEST_STATUS 
END INTERFACE
PUBLIC PRINT_NEST_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

INTERFACE MAKE_NEST                 !! MAKES INFORMATION FOR ONE NEST.
   MODULE PROCEDURE MAKE_NEST
END INTERFACE
PUBLIC MAKE_NEST

INTERFACE MAKE_FINE_BOUNDARY         !! MAKE FINE GRID BOUNDARY.
   MODULE PROCEDURE MAKE_FINE_BOUNDARY
END INTERFACE
PUBLIC MAKE_FINE_BOUNDARY

INTERFACE MAKE_BOX                   !! MAKES A BOX IN THE GRID
   MODULE  PROCEDURE MAKE_BOX
END INTERFACE
PRIVATE :: MAKE_BOX

INTERFACE MINTF                      !! INTERPOLATION TABLES FOR BOUNDARY INPUT.
   MODULE PROCEDURE MINTF
END INTERFACE
PRIVATE MINTF

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_NEST (NUMBER, SOUTH, NORTH, WEST, EAST, NAME)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

INTEGER          , INTENT(IN)  :: NUMBER     !! NUMBER OF NESTS.
REAL             , INTENT(IN)  :: SOUTH(:)   !! SOUTH LATITUDE OF NEST [DEG].
REAL             , INTENT(IN)  :: NORTH(:)   !! NORTH LATITUDE OF NEST [DEG].
REAL             , INTENT(IN)  :: WEST(:)    !! WEST LONGITUDE OF NEST [DEG].
REAL             , INTENT(IN)  :: EAST(:)    !! EAST LONGITUDE OF NEST [DEG].
CHARACTER (LEN=*), INTENT(IN)  :: NAME(:)    !! NAME OF NEST

INTEGER :: I, LEN

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CLEAR NEST INFORMATION IN MODULE.                                     !
!        ---------------------------------                                     !

N_NEST=0
IF (ALLOCATED(N_SOUTH)) DEALLOCATE(N_SOUTH)
IF (ALLOCATED(N_NORTH)) DEALLOCATE(N_NORTH)
IF (ALLOCATED(N_WEST)) DEALLOCATE(N_WEST)
IF (ALLOCATED(N_EAST)) DEALLOCATE(N_EAST)
IF (ALLOCATED(N_NAME)) DEALLOCATE(N_NAME)

IF (ALLOCATED(NBOUNC)) DEALLOCATE(NBOUNC)
IF (ALLOCATED(IJARC)) DEALLOCATE(IJARC)
IF (ALLOCATED(BLATC)) DEALLOCATE(BLATC)
IF (ALLOCATED(BLNGC)) DEALLOCATE(BLNGC)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COPY BOUNDARY DEFINITIONS.                                            !
!        --------------------------                                            !

N_NEST = NUMBER
ALLOCATE (N_SOUTH(1:N_NEST))
ALLOCATE (N_NORTH(1:N_NEST))
ALLOCATE (N_WEST(1:N_NEST))
ALLOCATE (N_EAST(1:N_NEST))
ALLOCATE (N_NAME(1:N_NEST))

N_SOUTH(1:N_NEST) = SOUTH(1:N_NEST)
N_NORTH(1:N_NEST) = NORTH(1:N_NEST)
N_WEST(1:N_NEST)  = WEST(1:N_NEST)
N_EAST(1:N_NEST)  = EAST(1:N_NEST)
DO I = 1,N_NEST
   LEN = MIN(LEN_TRIM(NAME(I)),20)
   N_NAME(I)  = NAME(I)(1:LEN)
END DO
CALL ADJUST (N_WEST, N_EAST)

END SUBROUTINE SET_NEST

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SET_BOUNDARY_OPTION (C , F)
LOGICAL, INTENT(IN), OPTIONAL :: C   !! COARSE GRID OPTION
LOGICAL, INTENT(IN), OPTIONAL :: F   !! FINE GRID OPTION

IF (PRESENT(C)) COARSE = C
IF (PRESENT(F)) FINE = F
 
END SUBROUTINE SET_BOUNDARY_OPTION

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_NEST_STATUS

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER             :: I, L
CHARACTER (LEN=1)   :: CGRID(NX,NY)  !! ARRAY FOR GRIDDED PRINT OUTPUT.
CHARACTER (LEN=100) :: TITL

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. PRINT STATUS.                                                         !
!        -------------                                                         !

WRITE (IU06,'(/,'' -------------------------------------------------'')')
WRITE (IU06,*) ' DATA STORED IN MODULE #',MODULE_NAME,'#'
WRITE (IU06,'('' -------------------------------------------------'')')
WRITE (IU06,*)' '
WRITE(IU06,*) ' ------------------------------------------------- '
WRITE(IU06,*) '              NEST MODULE STATUS:'
WRITE(IU06,*) ' ------------------------------------------------- '

WRITE(IU06,*) '  '
WRITE(IU06,*) ' COARSE GRID (BOUNDARY VALUE OUTPUT): '
WRITE(IU06,*) ' ------------------------------------ '
WRITE(IU06,*) '  '

IF (COARSE) THEN
   WRITE(IU06,*) ' COARSE GRID RUN (OUTPUT OF BOUNDARY VALUES)'
   WRITE(IU06,*) ' NO. OF NESTS DEFINED IN GRID IS N_NEST = ', N_NEST
   WRITE(IU06,*) ' THE MAXIMUM NUMBER OF POINTS IN ALL NESTS IS: ',MAX_NEST

   DO L=1,N_NEST
      WRITE(IU06,*) '  '
      WRITE(IU06,*) ' INFORMATION OF NEST ',L,' NEST NAME IS : ',N_NAME(L)
      WRITE(IU06,*) ' NORTH LATIDUTE = ', N_NORTH(L)
      WRITE(IU06,*) ' SOUTH LATIDUTE = ', N_SOUTH(L)
      WRITE(IU06,*) ' WEST LONGITUDE = ', N_WEST(L)
      WRITE(IU06,*) ' EAST LONGIDUTE = ', N_EAST(L)
      IF (ALLOCATED(NBOUNC)) THEN
         WRITE(IU06,*) ' NUMBER OF POINTS IS: ',NBOUNC(L)
         IF (NBOUNC(L) .NE. 0 .AND. ITEST.GE.2) THEN
            WRITE(IU06,*) '  '
            WRITE(IU06,'('' | LONGITUDE |  LATITUDE |    POINT   |'')')
            WRITE(IU06,'('' |-----------|-----------|------------|'')')
            DO I = 1,NBOUNC(L)
               WRITE(IU06,'('' | '',F9.4,'' | '',F9.4,'' | '',I10,'' |'')')   &
&                              BLNGC(I,L), BLATC(I,L), IJARC(I,L)
            END DO
         END IF
      ELSE
         WRITE(IU06,*) ' BOUNDARY POINTS ARE NOT INITIALIZED '
      END IF
   END DO
ELSE
   WRITE(IU06,*) ' MODEL RUNS WITHOUT BOUNDARY OUTPUT'
END IF

WRITE(IU06,*) '  '
WRITE(IU06,*) ' FINE GRID (BOUNDARY VALUE INPUT): '
WRITE(IU06,*) ' --------------------------------- '
WRITE(IU06,*) '  '
IF (FINE) THEN
   WRITE(IU06,*) ' FINE GRID RUN (INPUT OF BOUNDARY VALUES)'
   IF (NBOUNF .NE. 0) THEN
      WRITE (IU06,*) ' NUMBER OF SPECTRA FROM PREVIOUS COARSE GRID IS: ', NBINP
      WRITE (IU06,*) ' NUMBER OF BOUNDARY POINTS IS: ',NBOUNF
      WRITE (IU06,*) ' THE COARSE GRID NEST NAME IS: ',C_NAME
      WRITE (IU06,*) '  '
      IF (ITEST.GE.2) THEN
         WRITE (IU06,*) '        |--INPUT-|-RELATED COURSE GRID INDICES--|'
         WRITE (IU06,*) '     NO.   POINT.    LEFT   RIGHT   WEIGHT '
         DO I = 1, NBOUNF
            WRITE (IU06,'(4X,I5,3I8,F10.4)') I, IJARF(I), IBFL(I), IBFR(I), BFW(I)
         END DO
      END IF
   ELSE
      WRITE(IU06,*) ' BOUNDARY POINTS ARE NOT INITIALIZED '
   END IF
ELSE
   WRITE(IU06,*) ' MODEL RUNS WITHOUT BOUNDARY POINTS (FINE GRID)'
END IF
WRITE(IU06,*) '  '

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. OUTPUT OF LAND-SEA MASK.                                              !
!        ------------------------                                              !
  
IF (NSEA.GT.0 .AND. (COARSE .OR. FINE)) THEN
   WRITE (IU06,'('' NUMBER OF SEAPOINTS IS       NSEA = '',I5)') NSEA
   WRITE(IU06,*) ' '
   TITL = 'LAND-SEA MASK  (S = SEA, L = LAND)'
   CGRID ='L'
   WHERE (L_S_MASK) CGRID ='S'
   IF (COARSE) THEN
      DO L=1,N_NEST
         DO I=1,NBOUNC(L)
           CGRID(IXLG(IJARC(I,L)), KXLT(IJARC(I,L))) = CHAR(64+L)
         END DO
      END DO
      L = LEN_TRIM(TITL)
      TITL = TITL(1:L-1)//', '//CHAR(65)//'-'// CHAR(64+N_NEST)//' = NESTS)'
   END IF
   IF (FINE) THEN
      DO I=1,NBOUNF
           CGRID(IXLG(IJARF(I)), KXLT(IJARF(I))) = '/'
      END DO
      L = LEN_TRIM(TITL)
      TITL = TITL(1:L-1)//', / = BOUNDARY INPUT)'
   END IF
   CALL PRINT_ARRAY (IU06, '              ', TITL, CGRID,                      &
&                    AMOWEP, AMOSOP, AMOEAP, AMONOP)
   WRITE(IU06,*) ' '
END IF

END SUBROUTINE PRINT_NEST_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PREPARE_BOUNDARY

INTEGER  :: NUMBER, I, N_NEST_NEW, I_WEG

IF (NSEA.LE.0) THEN
   WRITE (IU06,*) '********************************************************'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*        FATAL ERROR IN SUB. PREPARE_BOUNDARY          *'
   WRITE (IU06,*) '*        ====================================          *'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*  THE MODEL GRID IS NOT DEFIENED.                     *'
   WRITE (IU06,*) '*  SUB. PREPARE_GRID HAS TO BE EXECUTED BEFORE         *'
   WRITE (IU06,*) '*  SUB. PREPARE_BOUNDARY                               *'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*              THE PROGRAM ABORTS                      *'
   WRITE (IU06,*) '********************************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COARSE GRIDS.                                                         !
!        -------------                                                         !

IF (COARSE) THEN
   DPHIAC = XDELLA
   DLAMAC = XDELLO

   NUMBER = MAXVAL(   (NINT((N_EAST - N_WEST)/XDELLO)                              &
&                    + NINT((N_NORTH - N_SOUTH)/XDELLA)) * 2)

   IF (ALLOCATED(NBOUNC)) DEALLOCATE(NBOUNC)
   IF (ALLOCATED(IJARC)) DEALLOCATE(IJARC)
   IF (ALLOCATED(BLATC)) DEALLOCATE(BLATC)
   IF (ALLOCATED(BLNGC)) DEALLOCATE(BLNGC)

   ALLOCATE (NBOUNC(1:N_NEST))
   ALLOCATE (IJARC(1:NUMBER,1:N_NEST))
   ALLOCATE (BLATC(1:NUMBER,1:N_NEST))
   ALLOCATE (BLNGC(1:NUMBER,1:N_NEST))

   DO I = 1, N_NEST
      CALL MAKE_NEST (N_SOUTH(I), N_NORTH(I), N_WEST(I), N_EAST(I),            &
&                     N_NAME(I), NBOUNC(I), IJARC(:,I), BLATC(:,I), BLNGC(:,I))
   END DO

   N_NEST_NEW = COUNT(NBOUNC.GT.0)
   MAX_NEST = MAXVAL(NBOUNC)
   IF (N_NEST_NEW .NE. N_NEST) THEN
      N_SOUTH(1: N_NEST_NEW) = PACK(N_SOUTH,NBOUNC.GT.0)
      N_NORTH(1: N_NEST_NEW) = PACK(N_NORTH,NBOUNC.GT.0)
      N_WEST (1: N_NEST_NEW) = PACK(N_WEST, NBOUNC.GT.0)
      N_EAST (1: N_NEST_NEW) = PACK(N_EAST, NBOUNC.GT.0)
      N_NAME (1: N_NEST_NEW) = PACK(N_NAME, NBOUNC.GT.0)
      I_WEG=0
      DO I = 1, N_NEST
         IF (NBOUNC(I).LE.0) THEN
            I_WEG = I_WEG + 1
            CYCLE
         END IF
         IF (I_WEG.EQ.0) CYCLE
         IF (I-I_WEG.LE.0) CYCLE
         IJARC(:,I-I_WEG) = IJARC(:,I)
         BLATC(:,I-I_WEG) = BLATC(:,I)
         BLNGC(:,I-I_WEG) = BLNGC(:,I)
      END DO
      NBOUNC (1: N_NEST_NEW) = PACK(NBOUNC, NBOUNC.GT.0)
      N_NEST = N_NEST_NEW  
   END IF
   IF (N_NEST.EQ.0) COARSE =.FALSE.
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. FINE GRID.                                                            !
!        ----------                                                            !

IF (FINE) CALL MAKE_FINE_BOUNDARY

END SUBROUTINE PREPARE_BOUNDARY

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     G. PRIVATE MODULE PROCEDURES.                                            !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE MAKE_NEST (SOUTH, NORTH, WEST, EAST, NAME,                          &
&                     NUMBER, IJ_N, LAT_N, LON_N)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   MAKE_NEST - MAKES COARSE GRID BOUNDARY.                                    !
!                                                                              !
!     R. PORTZ     MPI         15/01/1991                                      !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       COMPUTE ALL INFORMATION FOR COARSE GRID BOUNDARY VALUE OUTPUT.         !
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
!      INTERFACE VARIABLES.                                                    !

REAL              , INTENT(IN)    :: SOUTH    !! SOUTH LATITUDE OF NEST [DEG].
REAL              , INTENT(IN)    :: NORTH    !! NORTH LATITUDE OF NEST [DEG].
REAL              , INTENT(IN)    :: WEST     !! WEST LONGITUDE OF NEST [DEG].
REAL              , INTENT(IN)    :: EAST     !! EAST LONGITUDE OF NEST [DEG].
CHARACTER (LEN=20), INTENT(IN)    :: NAME     !! NAME OF NEST

INTEGER           , INTENT(INOUT) :: NUMBER   !! NUMBER OF NESTS POINTS.
INTEGER           , INTENT(INOUT) :: IJ_N(*)  !! SEA POINT NUMBERS.
REAL              , INTENT(INOUT) :: LAT_N(*) !! LATITUDES OF NEST [DEG].
REAL              , INTENT(INOUT) :: LON_N(*) !! LONGITUDE OF NEST [DEG].
 
! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER      :: NBOUNEW
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MASK

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CHECK COARSE GRID.                                                    !
!        ------------------                                                    !

NUMBER = 1
IF (AMOSOP .GT. SOUTH .OR. NORTH .GT. AMONOP .OR. SOUTH .GE. NORTH) NUMBER = 0

IF ((IPER) .AND.                                                               &
&   (AMOWEP.GT.WEST      .OR. WEST.GT.AMOEAP     )  .AND.                      &
&   (AMOWEP.GT.WEST+360. .OR. EAST+360..GT.AMOEAP)  .AND.                      &
&   (AMOWEP.GT.WEST-360. .OR. EAST-360..GT.AMOEAP) ) NUMBER = 0

IF (.NOT.CHECK_MULTIPLE (XDELLO, WEST  - AMOWEP) .OR.                          &
&   .NOT.CHECK_MULTIPLE (XDELLO, EAST  - AMOWEP) .OR.                          &  
&   .NOT.CHECK_MULTIPLE (XDELLA, SOUTH - AMOSOP) .OR.                          &
&   .NOT.CHECK_MULTIPLE (XDELLA, NORTH - AMOSOP) )  NUMBER = 0

IF (NUMBER .EQ. 0) THEN
   WRITE (IU06,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
   WRITE (IU06,*) '+                                                    +'
   WRITE (IU06,*) '+        WARNING ERROR IN SUB. MAKE__NEST            +'
   WRITE (IU06,*) '+        ================================            +'
   WRITE (IU06,*) '+                                                    +'
   WRITE (IU06,*) '+ ERROR IN NEST SPECIFICATIONS.                      +'
   WRITE (IU06,*) '+ WEST, EAST, NORTH OR SOUTH BOUNDARY IS NOT         +'
   WRITE (IU06,*) '+ IN GRID AREA  OR                                   +'
   WRITE (IU06,*) '+ SOUTH IS GREATER OR EQUAL NORTH  OR                +'
   WRITE (IU06,*) '+ CORNER POINTS ARE NOT COARSE GRID POINTS.          +'
   WRITE (IU06,*) '+                                                    +'
   WRITE (IU06,*) '+ GRID WEST  IS AMOWEP = ', AMOWEP
   WRITE (IU06,*) '+ GRID EAST  IS AMOEAP = ', AMOEAP
   WRITE (IU06,*) '+ GRID NORTH IS AMONOP = ', AMONOP
   WRITE (IU06,*) '+ GRID SOUTH IS AMOSOP = ', AMOSOP
   WRITE (IU06,*) '+ NEST WEST  IS   WEST = ', WEST
   WRITE (IU06,*) '+ NEST EAST  IS   EAST = ', EAST
   WRITE (IU06,*) '+ NEST NORTH IS  NORTH = ', NORTH
   WRITE (IU06,*) '+ NEST SOUTH IS  SOUTH = ', SOUTH
   WRITE (IU06,*) '+ NEST NAME  IS: ', NAME
   WRITE (IU06,*) '+                                                    +'
   WRITE (IU06,*) '+  NEST INFORMATION WILL NOT BE GENERATED            +'
   WRITE (IU06,*) '+                                                    +'
   WRITE (IU06,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++'

   RETURN
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INITIAL.                                                              !
!        --------                                                              !

NUMBER = (NINT((EAST -WEST)/XDELLO)  + NINT((NORTH - SOUTH)/XDELLA)) * 2

IJ_N(1:NUMBER)  = 0
LAT_N(1:NUMBER) = 0.
LON_N(1:NUMBER) = 0.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTED THE SQUARE BOX.                                              !
!        ------------------------                                              !

CALL MAKE_BOX (NUMBER, WEST, SOUTH, EAST, NORTH, XDELLA, XDELLO, LAT_N, LON_N)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. SEARCH BLOCK NUMBER AND SEA POINT NUMBER.                             !
!        -----------------------------------------                             !

CALL FIND_SEA_POINT (NUMBER, LAT_N, LON_N, IJ_N)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. PACKED ALL ARRAYS.                                                    !
!        -------------------                                                   !

IF (.NOT.ALLOCATED(MASK)) ALLOCATE(MASK(1:NUMBER))

MASK = (IJ_N(1:NUMBER).GT.0)
NBOUNEW = COUNT(MASK)

LAT_N(1:NBOUNEW) = PACK (LAT_N(1:NUMBER), MASK(1:NUMBER))
LON_N(1:NBOUNEW) = PACK (LON_N(1:NUMBER), MASK(1:NUMBER))
IJ_N(1:NBOUNEW) = PACK (IJ_N(1:NUMBER), MASK(1:NUMBER))

NUMBER = NBOUNEW
DEALLOCATE (MASK)

END SUBROUTINE MAKE_NEST

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE MAKE_FINE_BOUNDARY

! ---------------------------------------------------------------------------- !
!                                                                              !
!   MAKE_FINE_BOUNDARY - MAKE FINE GRID BOUNDARY.                              !
!                                                                              !
!     R. PORTZ     MPI         15/01/1991                                      !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       COMPUTE ALL INFORMATION FOR FINE GRID BOUNDARY VALUE INPUT.            !
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

INTEGER              :: I, IO, NBOUNEW, LEN, N_NEST_C, MAX_NEST_C 
REAL                 :: SOUTH_C, NORTH_C, EAST_C, WEST_C
INTEGER, ALLOCATABLE :: IJ_C(:)
REAL,    ALLOCATABLE :: LAT_C(:)
REAL,    ALLOCATABLE :: LON_C(:)
REAL,    ALLOCATABLE :: BLATF(:)
REAL,    ALLOCATABLE :: BLNGF(:)
LOGICAL, ALLOCATABLE :: MASK(:)
CHARACTER (LEN=80)   :: HEADER_COARSE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. READ INFO ABOUT COARSE GRID.                                          !
!     -------------------------------                                          !

LEN = LEN_TRIM(FILE10)
IO = 0
OPEN (UNIT=IU10, FILE=FILE10(1:LEN), FORM='UNFORMATTED', STATUS='OLD',IOSTAT=IO)
IF (IO.NE.0) THEN
   WRITE (IU06,*) '********************************************************'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*       FATAL ERROR IN SUB. MAKE_FINE_BOUNDARY         *'
   WRITE (IU06,*) '*       ======================================         *'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*  A FINE GRID RUN IS REQUESTED BUT COARSE GRID        *'
   WRITE (IU06,*) '*  PREPROC OUTPUT COULD NOT BE OPENED.                 *'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*  UNIT IS         IU10 = ', IU10
   WRITE (IU06,*) '*  FILE NAME IS  FILE10 = ', FILE10(1:LEN)
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*              THE PROGRAM ABORTS                      *'
   WRITE (IU06,*) '********************************************************'
   CALL ABORT1
END IF

READ (IU10) HEADER_COARSE
READ (IU10) N_NEST_C, MAX_NEST_C 
IF (N_NEST_C.EQ.0) THEN
   WRITE (IU06,*) '********************************************************'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*       FATAL ERROR IN SUB. MAKE_FINE_BOUNDARY         *'
   WRITE (IU06,*) '*       ======================================         *'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*  A FINE GRID RUN IS REQUESTED BUT COARSE GRID OUTPUT *'
   WRITE (IU06,*) '*  INFORMATION IS NOT IN THE COARSE PREPROC OUTPUT     *'
   WRITE (IU06,*) '*  COARSE GRID HEADER IS: ', HEADER_COARSE
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*  UNIT IS         IU10 = ', IU10
   WRITE (IU06,*) '*  FILE NAME IS  FILE10 = ', FILE10(1:LEN)
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*              THE PROGRAM ABORTS                      *'
   WRITE (IU06,*) '********************************************************'
   CALL ABORT1
END IF

DO I=1,N_NEST_C
   READ (IU10) NBINP, C_NAME

   IF (ALLOCATED(IJ_C)) DEALLOCATE(IJ_C)
   ALLOCATE(IJ_C(1:NBINP))
   IF (ALLOCATED(LAT_C))  DEALLOCATE(LAT_C)
   ALLOCATE(LAT_C(1:NBINP))
   IF (ALLOCATED(LON_C))  DEALLOCATE(LON_C)
   ALLOCATE(LON_C(1:NBINP))

   READ (IU10) IJ_C
   READ (IU10) DLAMAC, DPHIAC, SOUTH_C, NORTH_C, EAST_C, WEST_C, LON_C, LAT_C
   IF (AMOWEP.EQ.WEST_C .AND. AMOEAP.EQ.EAST_C .AND.                                &
&      AMONOP.EQ.NORTH_C .AND. AMOSOP.EQ.SOUTH_C) EXIT
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. CHECK THE INPUT.                                                      !
!     -------------------                                                      !

!     IS THE FINE GRID THE SAME AS IN THE COURSE GRID ?                        !

IF (AMOWEP.NE.WEST_C .OR. AMOEAP.NE.EAST_C .OR.                                &
&    AMONOP.NE.NORTH_C .OR. AMOSOP.NE.SOUTH_C) THEN
   WRITE (IU06,*) '********************************************************'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*       FATAL ERROR IN SUB. MAKE_FINE_BOUNDARY         *'
   WRITE (IU06,*) '*       ======================================         *'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*  THE GRID IS NOT IN THE COURSE GRID SET-UP           *'
   WRITE (IU06,*) '*  COARSE GRID HEADER IS: ', HEADER_COARSE
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*              THE PROGRAM ABORTS                      *'
   WRITE (IU06,*) '********************************************************'
   CALL ABORT1
END IF

!    IS THE STEP OF LAT. AND LONG. OF FINE GRID A MULTIPLE OF COURSE GRID ?    !

IF (.NOT.CHECK_MULTIPLE(XDELLA,DPHIAC)) THEN
   WRITE (IU06,*) '********************************************************'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*       FATAL ERROR IN SUB. MAKE_FINE_BOUNDARY         *'
   WRITE (IU06,*) '*       ======================================         *'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*  THE FINE GRID LATITUDE INCREMENT IS NOT A MULTIPLE  *'
   WRITE (IU06,*) '*  OF THE COURSE GRID INCREMENT                        *'
   WRITE (IU06,*) '*  COARSE GRID HEADER IS: ', HEADER_COARSE
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*  FINE GRID   XDELLA: ', XDELLA
   WRITE (IU06,*) '*  COARSE GRID DPHIAC: ', DPHIAC
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*              THE PROGRAM ABORTS                      *'
   WRITE (IU06,*) '********************************************************'
   CALL ABORT1
END IF
IF (.NOT.CHECK_MULTIPLE(XDELLO,DLAMAC)) THEN
   WRITE (IU06,*) '********************************************************'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*       FATAL ERROR IN SUB. MAKE_FINE_BOUNDARY         *'
   WRITE (IU06,*) '*       ======================================         *'
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*  THE FINE GRID LONGITUDE INCREMENT IS NOT A MULTIPLE *'
   WRITE (IU06,*) '*  OF THE COURSE GRID INCREMENT                        *'
   WRITE (IU06,*) '*  COARSE GRID HEADER IS: ', HEADER_COARSE
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*  FINE GRID   XDELLO: ', XDELLO
   WRITE (IU06,*) '*  COARSE GRID DLAMAC: ', DLAMAC
   WRITE (IU06,*) '*                                                      *'
   WRITE (IU06,*) '*              THE PROGRAM ABORTS                      *'
   WRITE (IU06,*) '********************************************************'
   CALL ABORT1
ENDIF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. ALLOCATE ARRAYS AND INITIAL.                                          !
!        ----------------------------                                          !

NBOUNF = (NX+NY)*2-4
IF (.NOT.ALLOCATED(IJARF)) ALLOCATE(IJARF(1:NBOUNF))
IF (.NOT.ALLOCATED(IBFL))  ALLOCATE(IBFL(1:NBOUNF))
IF (.NOT.ALLOCATED(IBFR))  ALLOCATE(IBFR(1:NBOUNF))
IF (.NOT.ALLOCATED(BFW))   ALLOCATE(BFW(1:NBOUNF))
IF (.NOT.ALLOCATED(BLATF)) ALLOCATE(BLATF(1:NBOUNF))
IF (.NOT.ALLOCATED(BLNGF)) ALLOCATE(BLNGF(1:NBOUNF))

IJARF = 0
IBFL  = 0
IBFR  = 0
BFW   = 0.
BLATF = 0.
BLNGF = 0.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. COMPUTED THE SQUARE BOX.                                              !
!        ------------------------                                              !

CALL MAKE_BOX (NBOUNF, AMOWEP, AMOSOP, AMOEAP, AMONOP, XDELLA, XDELLO,         &
&              BLATF, BLNGF)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. SEARCH BLOCK NUMBER AND SEA POINT NUMBER.                             !
!        -----------------------------------------                             !

CALL FIND_SEA_POINT (NBOUNF, BLATF, BLNGF, IJARF)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. MAKE INTERPOLATED ARRAYS.                                             !
!        -------------------------                                             !

CALL MINTF (DPHIAC, DLAMAC, LAT_C, LON_C)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. PACK ALL ARRAYS.                                                      !
!        ----------------                                                      !

IF (.NOT.ALLOCATED(MASK)) ALLOCATE(MASK(1:NBOUNF))

MASK = IJARF.GT.0
NBOUNEW = COUNT(MASK)

BLATF(1:NBOUNEW) = PACK (BLATF(1:NBOUNF), MASK(1:NBOUNF))
BLNGF(1:NBOUNEW) = PACK (BLNGF(1:NBOUNF), MASK(1:NBOUNF))
BFW  (1:NBOUNEW) = PACK (BFW  (1:NBOUNF), MASK(1:NBOUNF))
IJARF(1:NBOUNEW) = PACK (IJARF(1:NBOUNF), MASK(1:NBOUNF))
IBFR (1:NBOUNEW) = PACK (IBFR (1:NBOUNF), MASK(1:NBOUNF))
IBFL (1:NBOUNEW) = PACK (IBFL (1:NBOUNF), MASK(1:NBOUNF))

NBOUNF = NBOUNEW
DEALLOCATE (MASK)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. DEALLOCATE ARRAYS.                                                    !
!        ------------------                                                    !

DEALLOCATE (IJ_C, LAT_C, LON_C)
DEALLOCATE (BLATF, BLNGF)

END SUBROUTINE MAKE_FINE_BOUNDARY

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

SUBROUTINE MINTF (DPHIAC, DLAMAC, LAT_C, LON_C)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   MINTF - MAKE INTERPOLATION TABLES FOR BOUNDARY INPUT.                      !
!                                                                              !
!     R. PORTZ     MPI         15/01/1991                                      !
!     H. GUNTHER   GKSS/ECMWF  15/01/1991                                      !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       GENERATE SPACE INTERPOLATION TABLES USED FOR BOUNDARY VALUE INPUT      !
!       INTO A FINE GRID MODEL.                                                !
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

IMPLICIT NONE

REAL, INTENT(IN) :: DPHIAC   !! COARSE GRID LATITUDE INCREMENT.
REAL, INTENT(IN) :: DLAMAC   !! COARSE GRID LONGITUDE INCREMENT.
REAL, INTENT(IN) :: LAT_C(:) !! COARSE GRID LATITUDES.
REAL, INTENT(IN) :: LON_C(:) !! COARSE GRID LONGITUDES.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER  :: IDELLA, IDELLO, NI, I, N, M, IS, IE, K, NSTEP
REAL     :: PHI, XLAMDA

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. RATIOS OF GRID INCREMENTS.                                            !
!        --------------------------                                            !

IDELLA = NINT(DPHIAC/XDELLA)
IDELLO = NINT(DLAMAC/XDELLO)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. SOUTHERN MOST LATITUDE OF FINE GRID.                                  !
!        ------------------------------------                                  !

NI    = IDELLO - 1
PHI   = AMOSOP

!     2.1 LOOP OVER COARSE GRID POINTS.                                        !
!         -----------------------------                                        !

DO I = 1, NX, IDELLO

!     2.2 INTERPOLATION WEIGHT FOR INTERMEDIATE POINTS.                        !
!         ---------------------------------------------                        !
!                                                                              !
   IF (I.NE.NX) THEN
      DO N = 1, NI
         BFW (I+N) = REAL(N) / REAL(IDELLO)
      END DO
   END IF
!                                                                              !
!     2.3 INDICES OF COARSE GRID OUTPUT POINTS.                                !
!         -------------------------------------                                !
!                                                                              !
   XLAMDA = AMOWEP + REAL(I-1) * XDELLO
   DO M = 1,NBINP
      IF (LAT_C(M).EQ.PHI .AND. LON_C(M).EQ.XLAMDA) THEN
         IBFL(I) = M
         IBFR(I) = M
         DO N = 1, NI
            IF (I.NE.1) THEN
               IBFR(I-N) = M
            END IF
            IF (I.NE.NX) THEN
               IBFL(I+N) = M
            END IF
         END DO
         EXIT
      END IF
   END DO
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. NORTHERN MOST LATITUDE OF FINE GRID.                                  !
!        ------------------------------------                                  !

PHI = AMONOP
IS  = NX + 2*(NY-2) + 1
IE  = 2*(NX+NY-2)

!     3.1 LOOP OVER COARSE GRID POINTS.                                        !
!         -----------------------------                                        !

DO I = IS, IE, IDELLO
!                                                                              !
!     3.2 INTERPOLATION WEIGHT FOR INTERMEDIATE POINTS.                        !
!         ---------------------------------------------                        !
!                                                                              !
   IF (I.NE.IE) THEN
      DO N = 1, NI
         BFW (I+N) = REAL(N) / REAL(IDELLO)
      END DO
   END IF
!                                                                              !
!     3.3 INDICES OF COARSE GRID OUTPUT POINTS.                                !
!         -------------------------------------                                !
!                                                                              !
   XLAMDA = AMOWEP + (I-IS) * XDELLO
   DO M = 1,NBINP
      IF (LAT_C(M).EQ.PHI .AND. LON_C(M).EQ.XLAMDA) THEN
         IBFL(I) = M
         IBFR(I) = M
         DO N = 1, NI
            IF (I.NE.IS) THEN
               IBFR(I-N) = M
            END IF
            IF (I.NE.IE) THEN
               IBFL(I+N) = M
            END IF
         END DO
         EXIT
      END IF
   END DO
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. WESTERN MOST LONGITUDE OF FINE GRID.                                  !
!        ------------------------------------                                  !

XLAMDA = AMOWEP
NI = IDELLA - 1
NSTEP = 2 * IDELLA
IE = NX + 2*(NY-2) - 1
IS = NX + 2*NI + 1
K = 1

!     4.1 WEIGHTS AND LEFT INDICES FOR FIRST COARSE GRID SECTION.              !
!        --------------------------------------------------------              !

DO N = 1, NI
   IBFL(NX-1+2*N) = IBFL(1)
   BFW (NX-1+2*N) = REAL(N) / REAL(IDELLA)
END DO

!     4.2 LOOP OVER COARSE GRID POINTS.                                        !
!         -----------------------------                                        !

DO I = IS, IE, NSTEP
!                                                                              !
!     4.3 INTERPOLATION WEIGHT FOR INTERMEDIATE POINTS.                        !
!         ---------------------------------------------                        !
!                                                                              !
   DO N = 1, NI
      BFW (I+2*N) = REAL(N) / REAL(IDELLA)
   END DO

!     4.4 INDICES OF COARSE GRID OUTPUT POINTS.                                !
!         -------------------------------------                                !

   K = K + 1
   PHI = AMOSOP + (K-1) * DPHIAC
   DO M = 1,NBINP
      IF (LON_C(M).EQ.XLAMDA .AND. LAT_C(M).EQ.PHI) THEN
         IBFL(I) = M
         IBFR(I) = M
         DO N = 1, NI
            IBFL(I+2*N) = M
            IBFR(I-2*N) = M
         END DO
         EXIT
      END IF
   END DO
END DO

!     4.5 RIGHT INDICES FOR LAST COARSE GRID SECTION.                          !
!         -------------------------------------------                          !

K = NX + (2* (NY-2)) + 1
IF (IBFR(K) .NE. 0) THEN
    DO N = 1, NI
      IBFR(K-2*N) = IBFR(K)
   END DO
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. EASTERN MOST LONGITUDE OF FINE GRID.                                  !
!        ------------------------------------                                  !

XLAMDA = AMOEAP
IS = NX + 2*NI + 2
IE = NX + 2*(NY-2)
K = 1

!     5.1 WEIGHTS AND LEFT INDICES FOR FIRST COARSE GRID SECTION.              !
!        --------------------------------------------------------              !
!                                                                              !
DO N = 1, NI
   IBFL(NX+2*N) = IBFL(NX)
   BFW (NX+2*N) = REAL(N) / REAL(IDELLA)
END DO
!                                                                              !
!     5.2 LOOP OVER COARSE GRID POINTS.                                        !
!         -----------------------------                                        !

DO I = IS, IE, NSTEP

!     5.3 INTERPOLATION WEIGHT FOR INTERMEDIATE POINTS.                        !
!         ---------------------------------------------                        !
!                                                                              !
   DO N = 1, NI
      BFW (I+2*N) = REAL(N) / REAL(IDELLA)
   END DO
!                                                                              !
!     5.4 INDICES OF COARSE GRID OUTPUT POINTS.                                !
!         -------------------------------------                                !
!                                                                              !
   K = K + 1
   PHI = AMOSOP + (K-1) * DPHIAC
   DO M = 1,NBINP
      IF (LON_C(M).EQ.XLAMDA .AND. LAT_C(M).EQ.PHI) THEN
         IBFL(I) = M
         IBFR(I) = M
         DO N = 1, NI
            IBFL(I+2*N) = M
            IBFR(I-2*N) = M
         END DO
         EXIT
      END IF
   END DO
END DO

!     5.5 RIGHT INDICES FOR LAST COARSE GRID SECTION.                          !
!         -------------------------------------------                          !

K = 2*(NX + NY - 2)
IF (IBFR(K) .NE. 0) THEN
   M = NX + 2*NY - 2
   DO N = 1, NI
      IBFR(M-2*N) = IBFR(K)
   END DO
END IF

END SUBROUTINE MINTF

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_NEST_MODULE
