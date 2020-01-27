MODULE PREPROC_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS ALL VARIABLES AND CONSTANTS NECESSARY FOR THE         !
!   PREPROC PROGRAM. ALL PROCEDURES ARE INCLUDED TO COMPUTE THE INFORMATION    !
!   STORED IN WAM_CONNST_MODULE.                                               !
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

USE WAM_GRID_MODULE,      ONLY:  &
&       PREPARE_GRID,            &
&       PRINT_GRID_STATUS,       &
&       FIND_SEA_POINT              !! FIND BLOCK AND SEA POINT NUMBERS.

USE WAM_FRE_DIR_MODULE,   ONLY:  &
&       PRINT_FRE_DIR_STATUS

USE WAM_NEST_MODULE,  ONLY:  &
&       PREPARE_BOUNDARY,    & !! MAKE COARSE GRID BOUNDARY.
&       PRINT_NEST_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_FILE_MODULE,    ONLY: IU06, ITEST, IU07, FILE07, IU10, FILE10

USE WAM_GENERAL_MODULE, ONLY: G, PI, ZPI, R, DEG, EPS

USE WAM_FRE_DIR_MODULE, ONLY: KL, ML, FR, CO, TH, DELTH, DELTR, COSTH, SINTH,  &
&                             DFIM, GOM, C, NDEPTH, TCGOND, TSIHKD, TFAK,      &
&                             DFIMOFR, DFFR2, DFFR

USE WAM_GRID_MODULE,    ONLY: HEADER, NX, NY, NSEA, XDELLA, DELLAM, XDELLO,    &
&                             DELPHI, AMOWEP, AMOSOP, AMOEAP, AMONOP, IPER,    &
&                             SINPH, COSPH, DEPTH, KLAT, KLON, IXLG, KXLT,     &
&                             L_S_MASK, ONE_POINT

USE WAM_NEST_MODULE,    ONLY: N_NEST, MAX_NEST, N_NAME,                        &
                              NBOUNC, IJARC, BLATC, BLNGC, DLAMAC, DPHIAC,     &
&                             N_SOUTH, N_NORTH, N_EAST, N_WEST,                &
&                             NBINP, NBOUNF, C_NAME, IJARF, IBFL, IBFR, BFW

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE PREPARE_CONST              !! COMPUTES WAM_CONST_MODULE DATA.
   MODULE PROCEDURE PREPARE_CONST
END INTERFACE
PUBLIC PREPARE_CONST

INTERFACE PRINT_PREPROC_STATUS        !! PRINTS PREPROC STATUS.
   MODULE PROCEDURE PRINT_PREPROC_STATUS
END INTERFACE
PUBLIC PRINT_PREPROC_STATUS

INTERFACE READ_PREPROC_FILE           !! READS PREPROC OUTPUT FILE.
   MODULE PROCEDURE READ_PREPROC_FILE
END INTERFACE
PUBLIC READ_PREPROC_FILE

INTERFACE WRITE_PREPROC_FILE          !! WRITES PREPROC OUTPUT FILE.
   MODULE PROCEDURE WRITE_PREPROC_FILE
END INTERFACE
PUBLIC WRITE_PREPROC_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PREPARE_CONST

! ---------------------------------------------------------------------------- !
!                                                                              !
!   PREPARE_CONST - ROUTINE TO PREPARE WAM CONST MODULE.                       !
!                                                                              !
!     H.GUNTHER            ECMWF       04/04/1990                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       TO COMPUTE ALL VARAIABLES IN WAM CONST MODULE FROM THE USER INPUT.     !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       DONE BY CALLS TO MANY SUBS.                                            !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. GENERATE MODEL GRID.                                                  !
!        --------------------                                                  !

CALL PREPARE_GRID
IF (ITEST.GT.1) WRITE (IU06,*) ' SUB. PREPARE_GRID DONE'

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. COMPUTE NEST INFORMATION.                                             !
!        -------------------------                                             !

CALL PREPARE_BOUNDARY
IF (ITEST.GT.0) WRITE (IU06,*) ' SUB. PREPARE_BOUNDARY DONE'

END SUBROUTINE PREPARE_CONST

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE READ_PREPROC_FILE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   READ_PREPROC_FILE -  READ OUTPUT FILE FROM PREPROC.                        !
!                                                                              !
!     H. GUNTHER      GKSS/ECMWF      MAY 1990                                 !
!     H. GUNTHER      GKSS        OCTOBER 2000  FT90                           !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       INPUT OF PREPROC OUTPUT.                                               !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       UNFORMATED READ FROM FILE07.                                           !
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

INTEGER  :: IOS, LEN, IREFRAH, I

! ---------------------------------------------------------------------------- !
!                                                                              !
!     0. OPEN GRID_INFO FILE FROM PREPROC OUTPUT.                              !
!        ----------------------------------------                              !

IOS = 0
LEN = LEN_TRIM(FILE07)
OPEN (UNIT=IU07, FILE=FILE07(1:LEN), FORM='UNFORMATTED', STATUS='OLD',         &
&                                                                 IOSTAT=IOS)
IF (IOS.NE.0) THEN
   WRITE (IU06,*) ' ****************************************************'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *     FATAL ERROR IN SUB. READ_PREPROC_FILE        *'
   WRITE (IU06,*) ' *     =====================================        *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' * PREPROC OUTPUT FILE COULD NOT BE OPENED          *'
   WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
   WRITE (IU06,*) ' *    FILE NAME IS  FILE07 = ', FILE07(1:LEN)
   WRITE (IU06,*) ' *    UNIT IS         IU07 = ', IU07
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' ****************************************************'
   CALL ABORT1
END IF
READ (IU07) HEADER

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. READ COARSE GRID BOUNDARY OUTPUT INFORMATION.                         !
!        ---------------------------------------------                         !

READ(IU07) N_NEST, MAX_NEST
IF (.NOT.ALLOCATED(NBOUNC )) ALLOCATE (NBOUNC(N_NEST))
IF (.NOT.ALLOCATED(N_NAME )) ALLOCATE (N_NAME(N_NEST))
IF (.NOT.ALLOCATED(IJARC  )) ALLOCATE (IJARC(MAX_NEST,N_NEST))
IF (.NOT.ALLOCATED(N_SOUTH)) ALLOCATE (N_SOUTH(N_NEST))
IF (.NOT.ALLOCATED(N_NORTH)) ALLOCATE (N_NORTH(N_NEST))
IF (.NOT.ALLOCATED(N_EAST )) ALLOCATE (N_EAST(N_NEST))
IF (.NOT.ALLOCATED(N_WEST )) ALLOCATE (N_WEST(N_NEST))
IF (.NOT.ALLOCATED(BLNGC  )) ALLOCATE (BLNGC(MAX_NEST,N_NEST))
IF (.NOT.ALLOCATED(BLATC  )) ALLOCATE (BLATC(MAX_NEST,N_NEST))
DO I=1,N_NEST
   READ(IU07) NBOUNC(I), N_NAME(I)
   IF (NBOUNC(I).GT.0) THEN
      READ(IU07) IJARC(1:NBOUNC(I),I)
      READ(IU07) XDELLO, XDELLA, N_SOUTH(I), N_NORTH(I), N_EAST(I), N_WEST(I), &
&              BLNGC(1:NBOUNC(I),I), BLATC(1:NBOUNC(I),I)
   END IF
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. READ FINE GRID BOUNDARY INPUT INFORMATION.                            !
!        ------------------------------------------                            !

READ (UNIT=IU07) NBOUNF, NBINP, C_NAME

IF (NBOUNF.GT.0) THEN
   IF (.NOT.ALLOCATED(IJARF)) ALLOCATE (IJARF(NBOUNF))
   IF (.NOT.ALLOCATED(IBFL )) ALLOCATE (IBFL(NBOUNF))
   IF (.NOT.ALLOCATED(IBFR )) ALLOCATE (IBFR(NBOUNF))
   IF (.NOT.ALLOCATED(BFW  )) ALLOCATE (BFW(NBOUNF))
   READ (IU07) IJARF(1:NBOUNF), IBFL(1:NBOUNF), IBFR(1:NBOUNF), BFW(1:NBOUNF)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. READ FREQUENCY DIRECTION GRID.                                        !
!        ------------------------------                                        !

READ (IU07) ML, KL
IF ( .NOT.ALLOCATED(FR)     ) ALLOCATE( FR(ML)     )
IF ( .NOT.ALLOCATED(DFIM)   ) ALLOCATE( DFIM(ML)   )
IF ( .NOT.ALLOCATED(GOM)    ) ALLOCATE( GOM(ML)    )
IF ( .NOT.ALLOCATED(C)      ) ALLOCATE( C(ML)      )
IF ( .NOT.ALLOCATED(TH)     ) ALLOCATE( TH(KL)     )
IF ( .NOT.ALLOCATED(COSTH)  ) ALLOCATE( COSTH(KL)  )
IF ( .NOT.ALLOCATED(SINTH)  ) ALLOCATE( SINTH(KL)  )
IF ( .NOT.ALLOCATED(DFIMOFR)) ALLOCATE( DFIMOFR(ML))
IF ( .NOT.ALLOCATED(DFFR2)  ) ALLOCATE( DFFR2(ML)  )
IF ( .NOT.ALLOCATED(DFFR)   ) ALLOCATE( DFFR(ML)   )

READ (IU07) FR, DFIM, GOM, C, DELTH, DELTR, TH, COSTH, SINTH,                  &
&           DFIMOFR, DFFR2, DFFR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. READ GRID INFORMATION.                                                !
!        ----------------------                                                !

READ (IU07) NX, NY, NSEA, IPER, ONE_POINT

IF ( .NOT.ALLOCATED(L_S_MASK)) ALLOCATE( L_S_MASK(1:NX,1:NY) )
IF ( .NOT.ALLOCATED(IXLG)    ) ALLOCATE( IXLG(1:NSEA) )
IF ( .NOT.ALLOCATED(KXLT)    ) ALLOCATE( KXLT(1:NSEA) )
IF ( .NOT.ALLOCATED(SINPH)   ) ALLOCATE( SINPH(NY)    )
IF ( .NOT.ALLOCATED(COSPH)   ) ALLOCATE( COSPH(NY)    )
IF ( .NOT.ALLOCATED(KLAT)    ) ALLOCATE( KLAT(1:NSEA,1:2) )
IF ( .NOT.ALLOCATED(KLON)    ) ALLOCATE( KLON(1:NSEA,1:2) )
IF ( .NOT.ALLOCATED(DEPTH)   ) ALLOCATE( DEPTH(1:NSEA) )

READ (IU07) DELPHI, DELLAM, SINPH, COSPH, AMOWEP, AMOSOP, AMOEAP, AMONOP,      &
&           XDELLA, XDELLO
READ (IU07) IXLG, KXLT, L_S_MASK
READ (IU07) KLAT, KLON, DEPTH

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. READ SHALLOW WATER TABLES.                                            !
!        --------------------------                                            !

IF ( .NOT.ALLOCATED(TCGOND) ) ALLOCATE( TCGOND(NDEPTH,ML) )
IF ( .NOT.ALLOCATED(TFAK)   ) ALLOCATE( TFAK(NDEPTH,ML)   )
IF ( .NOT.ALLOCATED(TSIHKD) ) ALLOCATE( TSIHKD(NDEPTH,ML) )

READ (IU07) TCGOND, TFAK, TSIHKD

! ---------------------------------------------------------------------------- !
!                                                                              !
!     9. CLOSE FILE AND RETURN.                                                !
!        ----------------------                                                !

CLOSE (UNIT=IU07, STATUS='KEEP')

END SUBROUTINE READ_PREPROC_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_PREPROC_STATUS

! ---------------------------------------------------------------------------- !
!                                                                              !
!   PRINT_PREPROC_STATUS - PRINT STATUS OF PREPROC.                            !
!                                                                              !
!     H.GUNTHER            ECMWF       04/04/1990                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       MAKE A PRINTER OUTPUT OF THE PREPROC RESULTS, WHICH ARE SAVED IN       !
!       WAM_CONST_MODULE.                                                      !
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

CHARACTER    :: TITL*100
CHARACTER*1  :: LST(NX,NY)    !! LAND SEA TABLE  L = LAND,  S = SEA,
                              !!   / = COARSE GRID BOUNDARY OUTPUT POINTS,
                              !!   B = FINE GRID BOUNDARY INPUT POINTS.
CHARACTER*14 :: ZERO = ' '
CHARACTER*1  :: PMSK(1:NSEA)
INTEGER      :: I

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. FREQUENCY DIRECTION.                                                  !
!        --------------------                                                  !

CALL PRINT_FRE_DIR_STATUS

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. NEST INFORMATION.                                                     !
!        -----------------                                                     !

CALL PRINT_NEST_STATUS

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. GENERATE LAND SEA TABLE FROM INDEX ARRAYS AND PRINT IT.               !
!        -------------------------------------------------------               !


! IF (NSEA.GT.0) THEN
!   PMSK =  'S'                             !! SEA POINTS
!   IF (NBOUNC.GT.0) PMSK(IJARC) = '/'      !! NEST OUTPUT POINTS
!   IF (NBOUNF.GT.0) PMSK(IJARF) = 'B'      !! NEST INPUT POINTS
!   LST = UNPACK(PMSK, L_S_MASK, 'L')       !! LAND POINTS
!   WRITE (IU06,*) ' '
!   TITL = ' LAND SEA MAP:  L = LAND, S = SEA,  / = BOUNDARY VALUE OUTPUT,'//   &
!&      '  B = BOUDARY VALUE INPUT'
!!   CALL PRINT_ARRAY (IU06, ZERO, TITL, LST, AMOWEP, AMOSOP, AMOEAP, AMONOP)
!END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. DEPTH FIELD.                                                          !
!        ------------                                                          !

CALL PRINT_GRID_STATUS

END SUBROUTINE PRINT_PREPROC_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE WRITE_PREPROC_FILE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   WRITE_PREPROC_FILE - ROUTINE TO WRITE PREPROC OUTPUT TO FILE               !
!                                                                              !
!     H.GUNTHER            ECMWF       04/04/1990                              !
!     H.GUNTHER            GKSS       SEPTEMBER 2000   FT90                    !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO WRITE OUT THE COMPUTED CONSTANTS WHICH ARE STORED IN MODULE         !
!       WAM_CONST_MODULE.                                                      !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       UNFORMATTED WRITE AS SPECIFIED TO UNIT = IU07.                         !
!       FILENAME IS 'FILE07' AS DEFINED IN THE USER INPUT                      !
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

INTEGER      :: LEN, I

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. OPEN FILES.                                                           !
!        -----------                                                           !

LEN = LEN_TRIM(FILE07)
OPEN (UNIT=IU07, FILE=FILE07(1:LEN), FORM='UNFORMATTED', STATUS='UNKNOWN')

WRITE(IU07) HEADER

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. WRITE COARSE GRID BOUNDARY OUTPUT INFORMATION.                        !
!        ----------------------------------------------                        !

WRITE(IU07) N_NEST, MAX_NEST
DO I=1,N_NEST
   WRITE(IU07) NBOUNC(I), N_NAME(I)
   IF (NBOUNC(I).GT.0) THEN
      WRITE(IU07) IJARC(1:NBOUNC(I),I)
      WRITE(IU07) XDELLO, XDELLA, N_SOUTH(I), N_NORTH(I), N_EAST(I), N_WEST(I),&
&              BLNGC(1:NBOUNC(I),I), BLATC(1:NBOUNC(I),I)
   END IF
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. WRITE FINE GRID BOUNDARY INPUT INFORMATION.                           !
!        -------------------------------------------                           !

WRITE(IU07) NBOUNF, NBINP, C_NAME
IF (NBOUNF.GT.0) THEN
   WRITE(IU07) IJARF(1:NBOUNF), IBFL(1:NBOUNF), IBFR(1:NBOUNF), BFW(1:NBOUNF)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. WRITE FREQUENCY DIRECTION GRID.                                       !
!        ------------------------------                                        !

WRITE (IU07) ML, KL
WRITE (IU07) FR, DFIM, GOM, C, DELTH, DELTR, TH, COSTH, SINTH,                 &
&            DFIMOFR, DFFR2, DFFR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. WRITE GRID INFORMATION.                                               !
!        -----------------------                                               !

WRITE (IU07) NX, NY, NSEA, IPER, ONE_POINT
WRITE (IU07) DELPHI, DELLAM, SINPH, COSPH, AMOWEP, AMOSOP, AMOEAP, AMONOP,     &
&            XDELLA, XDELLO
WRITE (IU07) IXLG, KXLT, L_S_MASK
WRITE (IU07) KLAT, KLON, DEPTH

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. WRITE SHALLOW WATER TABLES.                                           !
!        ---------------------------                                           !

WRITE (IU07) TCGOND, TFAK, TSIHKD

! ---------------------------------------------------------------------------- !
!                                                                              !
!    10. CLOSE FILES.                                                          !
!        ------------                                                          !

CLOSE (UNIT=IU07, STATUS="KEEP")

END SUBROUTINE WRITE_PREPROC_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     G. PRIVAT MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE PREPROC_MODULE
