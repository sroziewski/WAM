MODULE WAM_OUTPUT_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS: ARRAYS NECESSARY FOR GRIDDED FIELDS OF PARAMTERS.    !
!                         ALL PROCEEDURES TO COMPUTE AND WRITE OUTPUT.          !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  &  !! TERMINATE PROCESSING.
&       OPEN_FILE,               &  !! OPENS A FILE.
&       INCDATE,                 &  !! UPDATE DATE TIME GROUP.
&       PRINT_ARRAY,             &  !! PRINTER OUTPUT OF AN ARRAY.
&       PRINT_SPECTRUM              !! PRINT A SPECTRUM.

USE WAM_INTERFACE_MODULE, ONLY:  &
&       FEMEAN,                  &  !! COMPUTATION OF MEAN FREQUENCY.
&       MEAN_DIRECTION,          &  !! COMPUTATION OF MEAN DIRECTION AND SPREAD.
&       PEAK_PERIOD,             &  !! COMPUTATION OF PEAK PERIOD.
&       TM1_TM2_PERIODS,         &  !! COMPUTATION OF TM1 AND/OR TM2 PERIOD.
&       TOTAL_ENERGY                !! COMPUTATION OF TOTAL ENERGY.

USE WAM_OUTPUT_SET_UP_MODULE, ONLY:  &
&       SAVE_OUTPUT_FILES           !! CLOSES AND OPENS OUTPUT FILES.

USE WAM_ICE_MODULE,    ONLY:     & 
&       PUT_ICE                     !! PUTS ICE INDICATOR INTO DATA FIELD.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE,ONLY: G, ZPI, DEG

USE WAM_FRE_DIR_MODULE,ONLY: KL, ML, CO, FR, C, DFIM, TH, COSTH, SINTH, TFAK

USE WAM_GRID_MODULE,   ONLY: NX, NY, AMOWEP, AMOSOP, AMOEAP, AMONOP,           &
&                            INDEP, L_S_MASK

USE WAM_CURRENT_MODULE,ONLY: U, V

USE WAM_FILE_MODULE,   ONLY: IU06, ITEST, CDTRES, IDELRES

USE WAM_MODEL_MODULE,  ONLY: U10, UDIR, USTAR, TAUW

USE WAM_TIMOPT_MODULE, ONLY: CDATEA, CDATEE, IDELPRO, CDTPRO, ISHALLO, IREFRA

USE WAM_OUTPUT_SET_UP_MODULE, ONLY:  &
&                            CDTINTT, CDTSPT, IDELINT, IDELSPT, NOUTT, COUTT,  &
&                            NPOUT, FFLAG, FFLAG20, FFLAG25,                   &
&                            PFLAG, PFLAG20, PFLAG25, CFLAG, CFLAG20, CFLAG25, &
&                            TITL, SCAL,                                       &
&                            NOUTP, OUTLAT, OUTLONG, NAME, IJAR      

USE WAM_ICE_MODULE,    ONLY: ICE_RUN

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE

INTEGER :: I

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. BLOCKED INTEGRATED PARAMETER TOTAL SPECTRUM.                          !
!        --------------------------------------------                          !

REAL,    ALLOCATABLE :: UDIR_BL(:) !! WIND DIRECTION [DEG].
REAL,    ALLOCATABLE :: CD_BL(:)   !! DRAG COEFFICENT.

REAL,    ALLOCATABLE :: HS_BL(:)   !! SIG. WAVE HEIGHT [M].
REAL,    ALLOCATABLE :: PPER_BL(:) !! PEAK PERIOD [S].
REAL,    ALLOCATABLE :: MPER_BL(:) !! MEAN PERIOD [S].
REAL,    ALLOCATABLE :: TM1_BL(:)  !! TM1 PERIOD [S].
REAL,    ALLOCATABLE :: TM2_BL(:)  !! TM2 PERIOD [S].
REAL,    ALLOCATABLE :: MDIR_BL(:) !! MEAN DIRECTION [DEG].
REAL,    ALLOCATABLE :: SPRE_BL(:) !! MEAN SPREAD [DEG].
REAL,    ALLOCATABLE :: TAUW_BL(:) !! NORMALISED WAVE STRESS [%].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. BLOCKED INTEGRATED SEA PARAMETERS.                                    !
!        ----------------------------------                                    !

REAL,    ALLOCATABLE :: HS_SEA_BL(:)     !! SEA SIG. WAVE HEIGHT [M].
REAL,    ALLOCATABLE :: PPER_SEA_BL(:)   !! SEA PEAK PERIOD [S].
REAL,    ALLOCATABLE :: MPER_SEA_BL(:)   !! SEA MEAN PERIOD [S].
REAL,    ALLOCATABLE :: TM1_SEA_BL(:)    !! SEA TM1 PERIOD [S].
REAL,    ALLOCATABLE :: TM2_SEA_BL(:)    !! SEA TM2 PERIOD [S].
REAL,    ALLOCATABLE :: MDIR_SEA_BL(:)   !! SEA MEAN DIRECTION [DEG].
REAL,    ALLOCATABLE :: SPRE_SEA_BL(:)   !! SEA MEAN SPREAD [DEG].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. BLOCKED INTEGRATED SWELL PARAMETERS.                                  !
!        ------------------------------------                                  !

REAL,    ALLOCATABLE :: HS_SWELL_BL(:)    !! SWELL SIG. WAVE HEIGHT [M].
REAL,    ALLOCATABLE :: PPER_SWELL_BL(:)  !! SWELL PEAK PERIOD [S].
REAL,    ALLOCATABLE :: MPER_SWELL_BL(:)  !! SWELL MEAN PERIOD [S].
REAL,    ALLOCATABLE :: TM1_SWELL_BL(:)   !! SWELL TM1 PERIOD [S].
REAL,    ALLOCATABLE :: TM2_SWELL_BL(:)   !! SWELL TM2 PERIOD [S].
REAL,    ALLOCATABLE :: MDIR_SWELL_BL(:)  !! SWELL MEAN DIRECTION [DEG].
REAL,    ALLOCATABLE :: SPRE_SWELL_BL(:)  !! SWELL MEAN SPREAD [DEG].

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE MODEL_OUTPUT_CONTROL        !! CONTROLS MODEL OUTPUT.
   MODULE PROCEDURE MODEL_OUTPUT_CONTROL
END INTERFACE
PUBLIC MODEL_OUTPUT_CONTROL

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     E.  PRIVATE INTERFACES.                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE COMPUTE_OUTPUT_PARAMETER    !! COMPUTES OUTPUT PARAMETER.
   MODULE PROCEDURE COMPUTE_OUTPUT_PARAMETER
END INTERFACE
PRIVATE  COMPUTE_OUTPUT_PARAMETER

INTERFACE INTPOL                      !! MAP SPECTRUM FROM SIGMA TO OMEGA SPACE.
   MODULE PROCEDURE INTPOL
END INTERFACE
PRIVATE INTPOL

INTERFACE SWELL_SEPARATION            !! SWELL SEPARATION AND INTEGRATED
   MODULE PROCEDURE SWELL_SEPARATION  !! PARAMETER OF SEA AND SWELL.
END INTERFACE
PRIVATE SWELL_SEPARATION

INTERFACE WRITE_MODEL_OUTPUT           !! WRITE MODEL OUTPUT.
   MODULE PROCEDURE WRITE_MODEL_OUTPUT
END INTERFACE
PRIVATE WRITE_MODEL_OUTPUT

INTERFACE WRITE_INT_PAR_OUTPUT        !! WRITE OUTPUT OF INTEGRATED PARAMETER.
   MODULE PROCEDURE WRITE_INT_PAR_OUTPUT
END INTERFACE
PRIVATE  WRITE_INT_PAR_OUTPUT

INTERFACE WRITE_SPECTRA_OUTPUT        !! WRITE OUTPUT OF SPECTRA.
   MODULE PROCEDURE WRITE_SPECTRA_OUTPUT
END INTERFACE
PRIVATE WRITE_SPECTRA_OUTPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     F. PUBLIC MODULE PROCEDURES.                                             !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE MODEL_OUTPUT_CONTROL (FL3, IU1, FILE1, IU2, FILE2)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   MODEL_OUTPUT_CONTROL - CONTROLS MODEL OUTPUT.                              !
!                                                                              !
!     H. GUNTHER         GKSS/ECMWF         JUNE 1990                          !
!                                                                              !
!    PURPOSE.                                                                  !
!     --------                                                                 !
!                                                                              !
!       CONTROL OUTPUT OF WAVE AND WIND FIELDS.                                !
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
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

REAL,              INTENT(IN) :: FL3(:,:,:)  !! BLOCK OF SPECTRA.
INTEGER,           INTENT(IN) :: IU1         !! FILE OUTPUT UNIT FOR PRAMETER.
CHARACTER (LEN=*), INTENT(IN) :: FILE1       !! FILE NAME FOR PRAMETER.
INTEGER,           INTENT(IN) :: IU2         !! FILE OUTPUT UNIT FOR SPECTRA.
CHARACTER (LEN=*), INTENT(IN) :: FILE2       !! FILE NAME FOR SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL, ALLOCATABLE  :: FL1(:,:,:)  !! BLOCK OF SPECTRA.
REAL, ALLOCATABLE  :: FL (:,:,:)  !! SWELL SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. ALLOCATE PARAMETER ARRAYS.                                            !
!        --------------------------                                            !

IF (CFLAG( 2)) ALLOCATE(UDIR_BL(SIZE(FL3,1)))
IF (CFLAG( 4)) ALLOCATE(CD_BL  (SIZE(FL3,1)))

IF (CFLAG( 7).OR.ANY(CFLAG( 9:11))) ALLOCATE(HS_BL(SIZE(FL3,1)))
IF (CFLAG( 8)) ALLOCATE(PPER_BL(SIZE(FL3,1)))
IF (CFLAG( 9)) ALLOCATE(MPER_BL(SIZE(FL3,1)))
IF (CFLAG(10)) ALLOCATE(TM1_BL (SIZE(FL3,1)))
IF (CFLAG(11)) ALLOCATE(TM2_BL (SIZE(FL3,1)))
IF (CFLAG(12)) ALLOCATE(MDIR_BL(SIZE(FL3,1)))
IF (CFLAG(13)) ALLOCATE(SPRE_BL(SIZE(FL3,1)))
IF (CFLAG(14)) ALLOCATE(TAUW_BL(SIZE(FL3,1)))

IF (CFLAG(15).OR.ANY(CFLAG(17:19))) ALLOCATE(HS_SEA_BL(SIZE(FL3,1)))
IF (CFLAG(16)) ALLOCATE(PPER_SEA_BL(SIZE(FL3,1)))
IF (CFLAG(17)) ALLOCATE(MPER_SEA_BL(SIZE(FL3,1)))
IF (CFLAG(18)) ALLOCATE(TM1_SEA_BL (SIZE(FL3,1)))
IF (CFLAG(19)) ALLOCATE(TM2_SEA_BL (SIZE(FL3,1)))
IF (CFLAG(20)) ALLOCATE(MDIR_SEA_BL(SIZE(FL3,1)))
IF (CFLAG(21)) ALLOCATE(SPRE_SEA_BL(SIZE(FL3,1)))

IF (CFLAG(23).OR.ANY(CFLAG(25:27))) ALLOCATE(HS_SWELL_BL(SIZE(FL3,1)))
IF (CFLAG(24)) ALLOCATE(PPER_SWELL_BL(SIZE(FL3,1)))
IF (CFLAG(25)) ALLOCATE(MPER_SWELL_BL(SIZE(FL3,1)))
IF (CFLAG(26)) ALLOCATE(TM1_SWELL_BL (SIZE(FL3,1)))
IF (CFLAG(27)) ALLOCATE(TM2_SWELL_BL (SIZE(FL3,1)))
IF (CFLAG(28)) ALLOCATE(MDIR_SWELL_BL(SIZE(FL3,1)))
IF (CFLAG(29)) ALLOCATE(SPRE_SWELL_BL(SIZE(FL3,1)))

IF (ANY(CFLAG(15:30)).OR.ANY(CFLAG(32:34)) ) THEN
   ALLOCATE (FL(SIZE(FL3,1),SIZE(FL3,2),SIZE(FL3,3)))
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. PROCESS OUTPUT.                                                       !
!        ---------------                                                       !

IF (IREFRA.EQ.2) THEN
   IF (.NOT.ALLOCATED(FL1)) ALLOCATE(FL1(SIZE(FL3,1),SIZE(FL3,2),SIZE(FL3,3)))
   CALL INTPOL (FL3, FL1)
   CALL COMPUTE_OUTPUT_PARAMETER (FL1, FL)
   CALL WRITE_MODEL_OUTPUT (FL1, FL, IU1, IU2)
ELSE
   CALL COMPUTE_OUTPUT_PARAMETER (FL3, FL)
   CALL WRITE_MODEL_OUTPUT (FL3, FL, IU1, IU2)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. SAVE OUTPUT FILES.                                                    !
!        ------------------                                                    !

IF (CDTRES.EQ.CDTPRO .OR. CDATEE.EQ.CDTPRO) THEN
   CALL SAVE_OUTPUT_FILES (IU1, FILE1, IU2, FILE2)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. DEALLOCATE ARRAYS.                                                    !
!        ------------------                                                    !

IF (CFLAG( 2)) DEALLOCATE(UDIR_BL)
IF (CFLAG( 4)) DEALLOCATE(CD_BL  )

IF (CFLAG( 7).OR.ANY(CFLAG( 9:11))) DEALLOCATE(HS_BL)
IF (CFLAG( 8)) DEALLOCATE(PPER_BL)
IF (CFLAG( 9)) DEALLOCATE(MPER_BL)
IF (CFLAG(10)) DEALLOCATE(TM1_BL )
IF (CFLAG(11)) DEALLOCATE(TM2_BL )
IF (CFLAG(12)) DEALLOCATE(MDIR_BL)
IF (CFLAG(13)) DEALLOCATE(SPRE_BL)
IF (CFLAG(14)) DEALLOCATE(TAUW_BL)

IF (CFLAG(15).OR.ANY(CFLAG(17:19))) DEALLOCATE(HS_SEA_BL)
IF (CFLAG(16)) DEALLOCATE(PPER_SEA_BL)
IF (CFLAG(17)) DEALLOCATE(MPER_SEA_BL)
IF (CFLAG(18)) DEALLOCATE(TM1_SEA_BL )
IF (CFLAG(19)) DEALLOCATE(TM2_SEA_BL )
IF (CFLAG(20)) DEALLOCATE(MDIR_SEA_BL)
IF (CFLAG(21)) DEALLOCATE(SPRE_SEA_BL)

IF (CFLAG(23).OR.ANY(CFLAG(25:27))) DEALLOCATE(HS_SWELL_BL)
IF (CFLAG(24)) DEALLOCATE(PPER_SWELL_BL)
IF (CFLAG(25)) DEALLOCATE(MPER_SWELL_BL)
IF (CFLAG(26)) DEALLOCATE(TM1_SWELL_BL )
IF (CFLAG(27)) DEALLOCATE(TM2_SWELL_BL )
IF (CFLAG(28)) DEALLOCATE(MDIR_SWELL_BL)
IF (CFLAG(29)) DEALLOCATE(SPRE_SWELL_BL)

IF (ANY(CFLAG(15:30)).OR.ANY(CFLAG(32:34)) ) THEN
   DEALLOCATE (FL)
END IF
IF (IREFRA.EQ.2) DEALLOCATE (FL1)

END SUBROUTINE MODEL_OUTPUT_CONTROL

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     G. PRIVATE MODULE PROCEDURES.                                            !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE INTPOL (F3, F1)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   INTPOL - TRANSFORMATION OF SPECTRA FROM SIGMA TO OMEGA.                    !
!                                                                              !
!     S.D.HASSELMANN      MPI            1.1.91                                !
!     H. GUNTHER          GKSS/ECMWF     1.2.91  MODIFIED FOR CYCLE_4          !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TRANSFORMATION OF A MOVING COORDINATE SYSTEM TO AN ABSOLUTE            !
!       COORDINATE SYSTEM.                                                     !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       SCATTERING TO NEIGHBOURING POINT.                                      !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !
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

REAL,    INTENT(IN)    :: F3(:,:,:)     !! BLOCK OF SPECTRA.
REAL,    INTENT(OUT)   :: F1(:,:,:)     !! BLOCK OF SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL, PARAMETER :: ALN00 = 24.1589                    !! 1. / LOG10(CO)
REAL, PARAMETER :: FRE0 = CO-1., PI2G = ZPI/G

INTEGER  :: K, M, IJ, NEWM, NEWM1, KH
REAL     :: FNEW, GWH
INTEGER  :: NEWF(SIZE(F3,1)), NEWF1(SIZE(F3,1)), KNEW(SIZE(F3,1))
REAL     :: FNEF(SIZE(F3,1)), GWP(SIZE(F3,1)), GWM(SIZE(F3,1)), WAVN(SIZE(F3,1))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     0. INITIAL OUTPUT ARRAY WITH ZERO.                                       !
!        -------------------------------                                       !

F1 = 0.0

!     1. LOOP OVER FREQUENCIES.                                                !
!        ----------------------                                                !

FRE: DO M = 1, ML
   IF (ISHALLO.NE.1) THEN
      WAVN = TFAK(INDEP,M)/ZPI
   ELSE
      WAVN = PI2G*FR(M)*FR(M)
   END IF

!     1.3 LOOP OVER DIRECTONS.                                                 !
!         --------------------                                                 !

   DIR: DO K = 1, KL
!                                                                              !
!     1.3.1 NEW FREQUENCY AND DIRECTION AT ALL GRIDPOINTS.                     !
!           ----------------------------------------------                     !

      FNEF = FR(M) + WAVN*(COSTH(K)*V + SINTH(K)*U)
      WHERE (FNEF.GT.0.)
         KNEW = K
      ELSEWHERE
         KNEW = MOD(K+KL/2-1,KL) + 1
         FNEF = -FNEF
      END WHERE

!     1.3.2 NEW FREQUENCY BIN NUMBER AT ALL GRIDPOINTS.                        !
!           -------------------------------------------                        !

      WHERE (FNEF.LE.FR(1)/CO)
          NEWF = -1
      ELSEWHERE
         NEWF = INT(LOG10(FNEF/FR(1))*ALN00+1.000000001)
      END WHERE

!     1.3.3 INTERPOLATED ENERGY DENSITIES AT ALL GRIDPOINTS.                   !
!           ------------------------------------------------                   !
!                                                                              !
      POINT: DO IJ = 1,SIZE(F3,1)
         FNEW = FNEF(IJ)
         NEWM = NEWF(IJ)
         IF (NEWM.LT.ML.AND.NEWM.GE.1) THEN
            NEWM1 = NEWM + 1
            GWH = DFIM(M)/(FR(NEWM1)-FR(NEWM)) *F3(IJ,K,M)
            GWM(IJ) = GWH*(FR(NEWM1)-FNEW)/DFIM(NEWM)
            GWP(IJ) = GWH*(FNEW-FR(NEWM))/DFIM(NEWM1)
            NEWF1(IJ) = NEWM1
         ELSE IF (NEWM.EQ.0) THEN
            GWH = CO*DFIM(M)/(FRE0*FR(1)) * F3(IJ,K,M)
            GWP(IJ) = GWH*(FNEW-FR(1)/CO)/DFIM(1)
            NEWF (IJ) = -1
            NEWF1(IJ) = 1
         ELSE IF (NEWM.EQ.ML) THEN
            GWH = DFIM(M)/(FRE0*FR(ML)) * F3(IJ,K,M)
            GWM(IJ) = GWH*(CO*FR(ML)-FNEW)/DFIM(ML)
            NEWF1(IJ) = -1
         ELSE
            NEWF (IJ) = -1
            NEWF1(IJ) = -1
         END IF
      END DO POINT
!                                                                              !
!     1.3.4 NEW SPECTRUM AT ALL GRIDPOINTS.                                    !
!           -------------------------------                                    !
!                                                                              !
       DO IJ = 1,SIZE(F3,1)
          NEWM  = NEWF (IJ)
          NEWM1 = NEWF1(IJ)
          KH = KNEW(IJ)
          IF (NEWM .NE.-1) F1(IJ,KH,NEWM ) = F1(IJ,KH,NEWM ) + GWM(IJ)
          IF (NEWM1.NE.-1) F1(IJ,KH,NEWM1) = F1(IJ,KH,NEWM1) + GWP(IJ)
      END DO
   END DO DIR
END DO FRE

END SUBROUTINE INTPOL

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE COMPUTE_OUTPUT_PARAMETER (FL3, FL)

! ---------------------------------------------------------------------------- !
!                                                                              !
!    COMPUTE_OUTPUT_PARAMETER - CALCULATES THE MODEL OUTPUT.                   !
!                                                                              !
!     H. GUNTHER         GKSS/ECMWF         JUNE 1990                          !
!                                                                              !
!    PURPOSE.                                                                  !
!     --------                                                                 !
!                                                                              !
!       COMPUTE  REQUESTED OUTPUT PARAMETERS OF WAVES AND WINDS.               !
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
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

REAL,    INTENT(IN)  :: FL3(:,:,:)  !! BLOCK OF SPECTRA.
REAL,    INTENT(OUT) :: FL (:,:,:)  !! BLOCK OF SWELL SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL  :: TAU(SIZE(FL3,1))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COMPUTE OUTPUT PARAMETERS.                                            !
!        --------------------------                                            !

IF (CFLAG(4).OR.CFLAG(14)) THEN
   TAU = USTAR**2 +0.0001
   IF (CFLAG(4)) CD_BL = TAU/(U10**2+0.01)
   IF (CFLAG(14)) TAUW_BL = TAUW/TAU
END IF

IF (CFLAG(2)) UDIR_BL = DEG * UDIR
IF (CFLAG(7).OR.ANY(CFLAG( 9:11))) THEN
   CALL TOTAL_ENERGY (FL3, HS_BL)
   IF (CFLAG(9)) THEN
      CALL FEMEAN (FL3, HS_BL, FM=MPER_BL)
      MPER_BL = 1./MPER_BL
   END IF
   IF (CFLAG(10).AND.CFLAG(11)) THEN
      CALL TM1_TM2_PERIODS (FL3, HS_BL, TM1_BL, TM2_BL)
   ELSE IF (CFLAG(10)) THEN
      CALL TM1_TM2_PERIODS (FL3, HS_BL, TM1=TM1_BL)
   ELSE IF (CFLAG(11)) THEN
      CALL TM1_TM2_PERIODS (FL3, HS_BL, TM2=TM2_BL)
   END IF
   IF (CFLAG(7)) HS_BL = 4.*SQRT(HS_BL)
END IF

IF (CFLAG(8)) CALL PEAK_PERIOD (FL3, PPER_BL)

IF (CFLAG(12).AND.CFLAG(13)) THEN
   CALL MEAN_DIRECTION (FL3, MDIR_BL, SPRE_BL)
ELSE IF (CFLAG(12)) THEN
   CALL MEAN_DIRECTION (FL3, THQ=MDIR_BL)
ELSE IF (CFLAG(13)) THEN
   CALL MEAN_DIRECTION (FL3, SPREAD=SPRE_BL)
END IF
IF (CFLAG(12)) MDIR_BL = MDIR_BL*DEG
IF (CFLAG(13)) SPRE_BL = SPRE_BL*DEG

IF (ITEST.GE.3)   WRITE(IU06,*)                                                &
& '      SUB. MODEL_OUTPUT: INTEGRATED PARAMETERS COMPUTED'

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. WINDSEA SWELL SEPARATION.                                             !
!        -------------------------                                             !

IF (ANY(CFLAG(15:30)).OR.ANY(CFLAG(32:34)) ) THEN
   CALL SWELL_SEPARATION (FL3, FL)
   IF (ITEST.GE.3) THEN
      WRITE(IU06,*) '      SUB. MODEL_OUTPUT: SWELL /SEA SEPARATION DONE'
   END IF
END IF

END SUBROUTINE COMPUTE_OUTPUT_PARAMETER

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SWELL_SEPARATION (FL3, FL1)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SWELL_SEPARATION - COMPUTES THE SWELL SPECTRUM AND INTEGRATED PARAMETER    !
!                      OF SWELL AND WINDSEA.                                   !
!                                                                              !
!     P.LIONELLO     FEBRUARY 87                                               !
!                                                                              !
!     L.ZAMBRESKY    NOVEMBER 87   GKSS/ECMWF   OPTIMIZED SUB.                 !
!     H. GUENTHER    FEBRUARY 2002   GKSS       FT90 AND INTEGRATED PARAMETERS !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO SEPARATE THE SWELL FROM THE WIND INTERACTING SEA AND COMPUTE        !
!       INTEGRATED PARAMETER FOR BOTH SYSTEMS.                                 !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE WAVES WHICH DO NOT INTERACT WITH THE WIND ARE CONSIDERED SWELL.    !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

REAL,    INTENT(IN)  :: FL3(:,:,:)  !! BLOCK OF SPECTRA.
REAL,    INTENT(OUT) :: FL1(:,:,:)  !! BLOCK OF SWELL SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL, PARAMETER :: FRIC = 28.

INTEGER  :: K, M
REAL     :: CM
REAL     :: SIS(SIZE(FL3,1))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. THE SWELL DISTRIBUTION IS COMPUTED.                                   !
!        -----------------------------------                                   !

FL1 = 0.
DO K=1,KL
   SIS = 1.2*USTAR*COS(TH(K)-UDIR)
   DO M = 1,ML
      CM = FRIC/C(M)
      WHERE (CM*SIS.LT.1.0) FL1(:,K,M) = FL3(:,K,M)
   END DO
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTATION INTEGRATED PARAMETER FOR WINDSEA.                         !
!        ---------------------------------------------                         !

IF (CFLAG(15).OR.ANY(CFLAG(17:19))) THEN
   CALL TOTAL_ENERGY (FL3-FL1, HS_SEA_BL)
   IF (CFLAG(17)) THEN
      CALL FEMEAN (FL3-FL1, HS_SEA_BL, FM=MPER_SEA_BL)
      MPER_SEA_BL = 1./MPER_SEA_BL
   END IF
   IF (CFLAG(18)) THEN
      IF (.NOT. CFLAG(19)) CALL TM1_TM2_PERIODS (FL3-FL1, HS_SEA_BL,           &
&                                                TM1=TM1_SEA_BL)
   END IF
   IF (CFLAG(19)) THEN
      IF (.NOT. CFLAG(18)) CALL TM1_TM2_PERIODS (FL3-FL1, HS_SEA_BL,           &
&                                                 TM2=TM2_SEA_BL)
   END IF
   IF (CFLAG(18).AND.CFLAG(19)) THEN
      CALL TM1_TM2_PERIODS (FL3-FL1, HS_SEA_BL, TM1=TM1_SEA_BL,                &
&                                                 TM2=TM2_SEA_BL)
   END IF
   IF (CFLAG(15)) HS_SEA_BL = 4.*SQRT(HS_SEA_BL)
END IF

IF (CFLAG(16)) CALL PEAK_PERIOD (FL3-FL1, PPER_SEA_BL)

IF (CFLAG(20).AND.CFLAG(21)) THEN
   CALL MEAN_DIRECTION (FL3-FL1, MDIR_SEA_BL, SPRE_SEA_BL)
ELSE IF (CFLAG(20)) THEN
   CALL MEAN_DIRECTION (FL3-FL1, THQ=MDIR_SEA_BL)
ELSE IF (CFLAG(21)) THEN
   CALL MEAN_DIRECTION (FL3-FL1, SPREAD=SPRE_SEA_BL)
END IF
IF (CFLAG(20)) MDIR_SEA_BL = MDIR_SEA_BL*DEG
IF (CFLAG(21)) SPRE_SEA_BL = SPRE_SEA_BL*DEG

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. COMPUTATION INTEGRATED PARAMETER FOR SWELL.                           !
!        -------------------------------------------                           !

IF (CFLAG(23).OR.ANY(CFLAG(25:27))) THEN
   CALL TOTAL_ENERGY (FL1, HS_SWELL_BL)
   IF (CFLAG(25)) THEN
      CALL FEMEAN (FL1, HS_SWELL_BL, FM=MPER_SWELL_BL)
      MPER_SWELL_BL = 1./MPER_SWELL_BL
   END IF
   IF (CFLAG(26)) THEN
      IF (.NOT.CFLAG(27)) CALL TM1_TM2_PERIODS (FL1, HS_SWELL_BL,              &
&                                                            TM1=TM1_SWELL_BL)
   END IF
   IF (CFLAG(27)) THEN
      IF (.NOT.CFLAG(26)) CALL TM1_TM2_PERIODS (FL1, HS_SWELL_BL,              &
&                                                            TM2=TM2_SWELL_BL)
   END IF
   IF (CFLAG(26).AND.CFLAG(27)) THEN
      CALL TM1_TM2_PERIODS (FL1, HS_SWELL_BL, TM1_SWELL_BL, TM2_SWELL_BL)
   END IF
   IF (CFLAG(23)) HS_SWELL_BL = 4.*SQRT(HS_SWELL_BL)
END IF

IF (CFLAG(24)) CALL PEAK_PERIOD (FL1, PPER_SWELL_BL)

IF (CFLAG(28).AND.CFLAG(29)) THEN
   CALL MEAN_DIRECTION (FL1, MDIR_SWELL_BL, SPRE_SWELL_BL)
ELSE IF (CFLAG(28)) THEN
   CALL MEAN_DIRECTION (FL1, THQ=MDIR_SWELL_BL)
ELSE IF (CFLAG(29)) THEN
   CALL MEAN_DIRECTION (FL1, SPREAD=SPRE_SWELL_BL)
END IF
IF (CFLAG(28)) MDIR_SWELL_BL = MDIR_SWELL_BL*DEG
IF (CFLAG(29)) SPRE_SWELL_BL = SPRE_SWELL_BL*DEG

END SUBROUTINE SWELL_SEPARATION

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE WRITE_MODEL_OUTPUT (FL3, FL, IU1, IU2)

! ---------------------------------------------------------------------------- !
!                                                                              !
!    WRITE_MODEL_OUTPUT - WRITES THE MODEL OUTPUT.                             !
!                                                                              !
!     H. GUNTHER         GKSS/ECMWF         JUNE 1990                          !
!                                                                              !
!    PURPOSE.                                                                  !
!     --------                                                                 !
!                                                                              !
!       WRITE REQUESTED OUTPUT TO PRINTER AND FILE.                            !
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
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

REAL,    INTENT(IN) :: FL3(:,:,:)  !! BLOCK OF SPECTRA.
REAL,    INTENT(IN) :: FL (:,:,:)  !! BLOCK OF SWELL SPECTRA.
INTEGER, INTENT(IN) :: IU1         !! FILE OUTPUT UNIT FOR PRAMETER.
INTEGER, INTENT(IN) :: IU2         !! FILE OUTPUT UNIT FOR SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. OUTPUT OF SPECTRA AT SELECTED GRID POINTS.                            !
!        ------------------------------------------                            !

IF (CDTSPT.EQ.CDTPRO) THEN
   CALL WRITE_SPECTRA_OUTPUT (FL3, FL, IU2)
   IF (ITEST.GE.3) WRITE(IU06,*)                                               &
&     '      SUB. MODEL_OUTPUT: SUB WRITE_SPECTRA_OUTPUT DONE'
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. OUTPUT OF INTEGRATED PARAMETERS.                                      !
!        --------------------------------                                      !

IF (CDTINTT.EQ.CDTPRO) THEN
   CALL WRITE_INT_PAR_OUTPUT (IU1)
   IF (ITEST.GE.3) WRITE(IU06,*)                                               &
&    '      SUB. MODEL_OUTPUT: SUB WRITE_INT_PAR_OUTPUT DONE'
END IF

END SUBROUTINE WRITE_MODEL_OUTPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE WRITE_INT_PAR_OUTPUT (IU)

! ---------------------------------------------------------------------------- !
!                                                                              !
!      WRITE_INT_PAR_OUTPUT  - OUTPUT OF INTEGRATED FIELDS.                    !
!                                                                              !
!     P.JANSSEN      KNMI                                                      !
!                                                                              !
!     P.LIONELLO  FEB. 87                                                      !
!                 OUTPUT OF SWELL ENERGY ,MEAN SWELL DIRECTION ,               !
!                 MEAN SWELL FREQUENCY AND MEAN WIND-SEA WAVE                  !
!                 DIRECTION AT ALL ACTIVE GRID POINTS                          !
!                                                                              !
!     H.GUNTHER   GKSS/ECMWF     NOVEMBER 1989                                 !
!     H.GUNTHER   GKSS           FEBRUARY 2002                                 !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       SAVE INTEGRATED PARAMETER IN GRID ARRAYS.                              !
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
!     INTERFACE VARIABLE.                                                      !
!     -------------------                                                      !

INTEGER, INTENT(IN)    :: IU         !! OUTPUT UNIT FOR PRAMETER.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLE.                                                          !
!     ---------------                                                          !

REAL    :: DNGX, DNGY

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. WRITE INTEGRATED PARAMETER TO FILE HEADER.                            !
!        ------------------------------------------                            !

IF (FFLAG20) THEN
   DNGX = FLOAT(NX)
   DNGY = FLOAT(NY)
   WRITE (IU) CDTPRO, DNGX, DNGY, AMOWEP, AMOSOP, AMOEAP, AMONOP
   WRITE (IU) FFLAG(1:30)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2.  STORE INTEGRATED PARAMETERS OF TOTAL SEA IN GRID ARRAYS.             !
!         --------------------------------------------------------             !

IF (CFLAG( 1)) CALL GRID_OUTPUT (U10     ,  1 ,IU )
IF (CFLAG( 2)) CALL GRID_OUTPUT (UDIR_BL ,  2 ,IU )
IF (CFLAG( 3)) CALL GRID_OUTPUT (USTAR   ,  3 ,IU )
IF (CFLAG( 4)) CALL GRID_OUTPUT (CD_BL   ,  4 ,IU )

IF (CFLAG( 7)) CALL GRID_OUTPUT (HS_BL   ,  7 ,IU )
IF (CFLAG( 8)) CALL GRID_OUTPUT (PPER_BL ,  8 ,IU )
IF (CFLAG( 9)) CALL GRID_OUTPUT (MPER_BL ,  9 ,IU )
IF (CFLAG(10)) CALL GRID_OUTPUT (TM1_BL  , 10 ,IU )
IF (CFLAG(11)) CALL GRID_OUTPUT (TM2_BL  , 11 ,IU )
IF (CFLAG(12)) CALL GRID_OUTPUT (MDIR_BL , 12 ,IU )
IF (CFLAG(13)) CALL GRID_OUTPUT (SPRE_BL , 13 ,IU )
IF (CFLAG(14)) CALL GRID_OUTPUT (TAUW_BL , 14 ,IU )

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2.  STORE INTEGRATED PARAMETERS OF SEA IN GRID ARRAYS.                   !
!         --------------------------------------------------                   !

IF (CFLAG(15)) CALL GRID_OUTPUT (HS_SEA_BL   , 15 ,IU )
IF (CFLAG(16)) CALL GRID_OUTPUT (PPER_SEA_BL , 16 ,IU )
IF (CFLAG(17)) CALL GRID_OUTPUT (MPER_SEA_BL , 17 ,IU )
IF (CFLAG(18)) CALL GRID_OUTPUT (TM1_SEA_BL  , 18 ,IU )
IF (CFLAG(19)) CALL GRID_OUTPUT (TM2_SEA_BL  , 19 ,IU )
IF (CFLAG(20)) CALL GRID_OUTPUT (MDIR_SEA_BL , 20 ,IU )
IF (CFLAG(21)) CALL GRID_OUTPUT (SPRE_SEA_BL , 21 ,IU )

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2.  STORE INTEGRATED PARAMETERS OF SWELL IN GRID ARRAYS.                 !
!         ----------------------------------------------------                 !

IF (CFLAG(23)) CALL GRID_OUTPUT (HS_SWELL_BL   , 23 ,IU )
IF (CFLAG(24)) CALL GRID_OUTPUT (PPER_SWELL_BL , 24 ,IU )
IF (CFLAG(25)) CALL GRID_OUTPUT (MPER_SWELL_BL , 25 ,IU )
IF (CFLAG(26)) CALL GRID_OUTPUT (TM1_SWELL_BL  , 26 ,IU )
IF (CFLAG(27)) CALL GRID_OUTPUT (TM2_SWELL_BL  , 27 ,IU )
IF (CFLAG(28)) CALL GRID_OUTPUT (MDIR_SWELL_BL , 28 ,IU )
IF (CFLAG(29)) CALL GRID_OUTPUT (SPRE_SWELL_BL , 29 ,IU )

! ---------------------------------------------------------------------------- !

CONTAINS

SUBROUTINE GRID_OUTPUT (BLOCK, IP, IU)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   GRID_OUTPUT - GRIDDED FIELD OUTPUT.                                        !
!                                                                              !
!     H. GUNTHER       ECMWF    NOVEMBER 1989                                  !
!     H. GUNTHER       GKSS     NOVEMBER 2001                                  !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       GRIDDED WAVE OUTPUT OF INTEGRATED PARAMETER FIELDS.                    !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE GRID ARRAY IS INITIALISED WITH THE MISSING VALUE 'ZMISS'.          !
!       THE PARAMETER, WHICH IS GIVEN IN BLOCKED FORMAT, IS DISTRIBUTED OVER   !
!       THE GRID.                                                              !
!       ICE POINTS ARE MARKED BY THE MISSING VALUE 'ZMISS'.                    !
!       THE GRID IS WRITTEN TO FILE AND/OR PRINTER.                            !
!       THE GRID IS NUMBERED FROM WEST TO EAST AND FROM NORTH TO SOUTH.        !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLE.                                                      !
!     -------------------                                                      !

REAL,    INTENT(IN)    :: BLOCK(:)   !! BLOCK OF PARAMETER
INTEGER, INTENT(IN)    :: IP         !! PARAMETER NUMBER.
INTEGER, INTENT(IN)    :: IU         !! OUTPUT UNIT FOR PRAMETER.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLE.                                                          !
!     ---------------                                                          !

REAL, PARAMETER       :: ZMISS =  -999.    !! MISSING VALUE
INTEGER               :: IJ, IX, IY
REAL                  :: GRID(1:NX,1:NY)  !! GRIDDED PARAMETER FIELD
REAL                  :: ICE_B(1:SIZE(BLOCK)) !! BLOCK OF PARAMETER WITH ICE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INSERT ICE AND MAKE GRID FIELD.                                       !
!        -------------------------------                                       !

IF (ICE_RUN) THEN
   ICE_B = BLOCK
   CALL PUT_ICE (ICE_B, ZMISS)             !! SET ICE POINTS TO MISSING DATA.
   GRID = UNPACK (ICE_B, L_S_MASK, ZMISS)  !! MAKE GRIDDED FIELD.
ELSE
   GRID = UNPACK (BLOCK, L_S_MASK, ZMISS)  !! MAKE GRIDDED FIELD.
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. WRITE OUTPUT.                                                         !
!     ----------------                                                         !

IF (FFLAG(IP)) WRITE (IU) GRID
IF (PFLAG(IP)) CALL PRINT_ARRAY (IU06, CDTPRO, TITL(IP), GRID,                 &
&                                AMOWEP, AMOSOP, AMOEAP, AMONOP, SCAL(IP))

END SUBROUTINE GRID_OUTPUT

END SUBROUTINE WRITE_INT_PAR_OUTPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE WRITE_SPECTRA_OUTPUT (FL3, FL1, IU)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   WRITE_SPECTRA_OUTPUT -  OUTPUT OF SPECTRA.                                 !
!                                                                              !
!      S.D.HASSELMANN.                                                         !
!      P.JANSSEN       KNMI         FEBRUARY 1986                              !
!                                                                              !
!      P. LIONELLO                  FEBRUARY 1987                              !
!                  OUTPUT OF SWELL 2-D DISTRIBUTION , SWELL WAVE HEIGHT        !
!                  MEAN SWELL FREQUENCY , MEAN SWELL DIRECTION ,WIND-SE        !
!                  WAVE HEIGTH AND WIND SEA WAVE DIRECTION.                    !
!                                                                              !
!      L. ZAMBRESKY   GKSS/ECMWF    JULY 89                                    !
!                 VARIANCE ENERGY SUMMED OVER FREQUENCY AND DIRECTION.         !
!                                                                              !
!      H. GUNTHER     ECMWF         OCTOBER 1990                               !
!                 EXTENDED HEADER AND OUTPUT OF 1D SPECTRA IN PRINT.           !
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
!     INTERFACE VARIABLE.                                                      !
!     -------------------                                                      !

REAL,    INTENT(IN) :: FL3(:,:,:)  !! BLOCK OF SPECTRA.
REAL,    INTENT(IN) :: FL1(:,:,:)  !! BLOCK OF SWELL SPECTRA.
INTEGER, INTENT(IN) :: IU          !! OUTPUT UNIT FOR SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

CHARACTER (LEN=40) :: TEXT

INTEGER  :: I, IJ, NGOU
REAL     :: XLON, XLAT, XANG, XFRE, TH0
REAL     :: SPEC(KL,ML)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. LOOP OVER OUTPUT POINTS.                                              !
!        ------------------------                                              !


DO NGOU = 1,NOUTP
   IJ = IJAR(NGOU)
   XLON = OUTLONG(NGOU)
   XLAT = OUTLAT (NGOU)

!     1.1 FILE OUTPUT.                                                         !
!         ------------                                                         !

   IF (FFLAG25) THEN
      XANG = REAL(KL)
      XFRE = REAL(ML)
      TH0 = TH(1)*DEG
      WRITE(IU) XLON, XLAT, CDTPRO, XANG, XFRE, TH0, FR(1), CO
      WRITE(IU) FFLAG(31:34)

!     1.1.1 TOTAL SPECTRUM.                                                    !
!           ---------------                                                    !

      IF (FFLAG(31)) THEN
         WRITE(IU) U10(IJ), UDIR_BL(IJ), USTAR(IJ), HS_BL(IJ),                 &
&                  PPER_BL(IJ), MPER_BL(IJ), TM1_BL(IJ), TM2_BL(IJ),           &
&                  MDIR_BL(IJ), SPRE_BL(IJ)
         WRITE(IU) FL3(IJ,1:KL,1:ML)
      END IF

!     1.1.2 SEA OUTPUT.                                                        !
!           -----------                                                        !

      IF (FFLAG(32)) THEN
         WRITE(IU) U10(IJ), UDIR_BL(IJ), USTAR(IJ), HS_SEA_BL(IJ),             &
&                  PPER_SEA_BL(IJ), MPER_SEA_BL(IJ), TM1_SEA_BL(IJ),           &
&                  TM2_SEA_BL(IJ), MDIR_SEA_BL(IJ), SPRE_SEA_BL(IJ)
         WRITE(IU) FL3(IJ,1:KL,1:ML)-FL1(IJ,1:KL,1:ML)
      END IF

!     1.1.3 SWELL OUTPUT.                                                      !
!           -------------                                                      !

      IF (FFLAG(33)) THEN
         WRITE(IU) U10(IJ), UDIR_BL(IJ), USTAR(IJ), HS_SWELL_BL(IJ),           &
&                  PPER_SWELL_BL(IJ), MPER_SWELL_BL(IJ), TM1_SWELL_BL(IJ),     &
&                  TM2_SWELL_BL(IJ), MDIR_SWELL_BL(IJ), SPRE_SWELL_BL(IJ)
         WRITE(IU) FL1(IJ,1:KL,1:ML)
      END IF
   END IF

!     1.2 PRINT OUTPUT.                                                        !
!         -------------                                                        !

!     1.1.2 PRINT SPECTRUM.                                                    !
!           ---------------                                                    !

   IF (PFLAG25) THEN
      IF (PFLAG(31)) THEN
         SPEC(1:KL,1:ML) = FL3(IJ,1:KL,1:ML)
         TEXT(1:40) = TITL(31)(1:20)//NAME(NGOU)
         CALL PRINT_SPECTRUM (IU06, CDTPRO, XLON, XLAT, TEXT, FR, TH, SPEC,    &
&               U10(IJ), UDIR_BL(IJ), USTAR(IJ), HS_BL(IJ),                    &
&                PPER_BL(IJ), MPER_BL(IJ), TM1_BL(IJ), TM2_BL(IJ),             &
&                MDIR_BL(IJ), SPRE_BL(IJ))
      END IF


!     1.2.2 PRINT OUTPUT SEA SPECTRUM.                                         !
!           --------------------------                                         !

      IF (PFLAG(32)) THEN
         SPEC(1:KL,1:ML) = FL3(IJ,1:KL,1:ML)-FL1(IJ,1:KL,1:ML)
         TEXT(1:40) = TITL(32)(1:20)//NAME(NGOU)
         CALL PRINT_SPECTRUM (IU06, CDTPRO, XLON, XLAT, TEXT, FR, TH, SPEC,    &
&                U10(IJ), UDIR_BL(IJ), USTAR(IJ), HS_SEA_BL(IJ),               &
&                PPER_SEA_BL(IJ), MPER_SEA_BL(IJ), TM1_SEA_BL(IJ),             &
&                TM2_SEA_BL(IJ), MDIR_SEA_BL(IJ), SPRE_SEA_BL(IJ))
      END IF


!     1.2.4 PRINT SWELL SPECTRUM.                                              !
!           ---------------------                                              !

      IF (PFLAG(33)) THEN
         SPEC(1:KL,1:ML) = FL1(IJ,1:KL,1:ML)
         TEXT(1:40) = TITL(33)(1:20)//NAME(NGOU)
         CALL PRINT_SPECTRUM (IU06, CDTPRO, XLON, XLAT, TEXT, FR, TH, SPEC,    &
&                U10(IJ), UDIR_BL(IJ), USTAR(IJ), HS_SWELL_BL(IJ),             &
&                PPER_SWELL_BL(IJ), MPER_SWELL_BL(IJ), TM1_SWELL_BL(IJ),       &
&                TM2_SWELL_BL(IJ), MDIR_SWELL_BL(IJ), SPRE_SWELL_BL(IJ))
      END IF
   END IF
END DO

END SUBROUTINE WRITE_SPECTRA_OUTPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_OUTPUT_MODULE
