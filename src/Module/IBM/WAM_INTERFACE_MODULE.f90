MODULE WAM_INTERFACE_MODULE

! ---------------------------------------------------------------------------- !
!          THIS MODULE COLLECTS ALL INTERFACES OF PROCEDURES USED IN           !
!                       THE WAM MODEL PROGRAMS                                 !
!                                                                              !
! ---------------------------------------------------------------------------- !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     MODULE VARIABLES.                                                        !
!     -----------------                                                        !

USE WAM_FRE_DIR_MODULE, ONLY: CO, TFAK, DELTH, FR, DFIM, SINTH, COSTH,         &
&                             DFIMOFR, DFFR, DFFR2, WETAIL, FRTAIL, WP1TAIL,   &
&                             WP2TAIL
USE WAM_GENERAL_MODULE, ONLY: G, ZPI

IMPLICIT NONE
PRIVATE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     A.  GENERIC INTERFACES (THIS MODULE CONTAINS THE PROCEDURES).            !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE FEMEAN                        !! MEAN FREQUENCY AND WAVE NUMBER.
   MODULE PROCEDURE FEMEAN
END INTERFACE
PUBLIC FEMEAN

INTERFACE INTSPEC                       !! INTERPOLATES SPECTRA.
   MODULE PROCEDURE INTSPEC
END INTERFACE
PUBLIC INTSPEC

INTERFACE MEAN_DIRECTION                !! MEAN DIRECTION AND SPREAD
   MODULE PROCEDURE MEAN_DIRECTION_1    !! SCALAR VERSION
   MODULE PROCEDURE MEAN_DIRECTION_B    !! VECTOR VERSION
END INTERFACE
PUBLIC MEAN_DIRECTION

INTERFACE PEAK_PERIOD                   !! COMPUTATES PEAK PERIOD.
   MODULE  PROCEDURE PEAK_PERIOD_1      !! SCALAR VERSION
   MODULE  PROCEDURE PEAK_PERIOD_B      !! VECTOR VERSION
END INTERFACE
PUBLIC PEAK_PERIOD

INTERFACE ROTSPEC                       !! ROTATE A SPECTRUM.
   MODULE PROCEDURE ROTSPEC
END INTERFACE
PUBLIC ROTSPEC

INTERFACE STRSPEC                       !! STRETCH A SPECTRUM.
   MODULE PROCEDURE STRSPEC
END INTERFACE
PUBLIC STRSPEC

INTERFACE TM1_TM2_PERIODS               !! COMPUTATES TM1 AND/OR TM2 PERIODS.
   MODULE  PROCEDURE TM1_TM2_PERIODS_1  !! SCALAR VERSION
   MODULE  PROCEDURE TM1_TM2_PERIODS_B  !! VECTOR VERSION
END INTERFACE
PUBLIC TM1_TM2_PERIODS

INTERFACE TOTAL_ENERGY                  !! COMPUTES TOTAL ENERGY.
   MODULE  PROCEDURE TOTAL_ENERGY_1     !! SCALAR VERSION
   MODULE  PROCEDURE TOTAL_ENERGY_B     !! VECTOR VERSION
END INTERFACE
PUBLIC TOTAL_ENERGY

INTERFACE COS2_SPR                  !! COSINE SQUARE SPREAD.
   MODULE  PROCEDURE COS2_SPR_1     !! SCALAR VERSION
   MODULE  PROCEDURE COS2_SPR_B     !! VECTOR VERSION
END INTERFACE
PUBLIC COS2_SPR

INTERFACE WM1_WM2_WAVENUMBER               !! WM1 AND/OR WM2 WAVENUMBER.
   MODULE  PROCEDURE WM1_WM2_WAVENUMBER_1  !! SCALAR VERSION
   MODULE  PROCEDURE WM1_WM2_WAVENUMBER_B  !! VECTOR VERSION
END INTERFACE
PUBLIC WM1_WM2_WAVENUMBER


INTERFACE MAT_MUL                        !! MATRIX MULTIPLICATON.
   MODULE  PROCEDURE MAT_MUL
END INTERFACE
PRIVATE MAT_MUL

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE FEMEAN (F, EMEAN, FM, MASK)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   FEMEAN - COMPUTATION OF MEAN FREQUENCY.                                    !
!                                                                              !
!     S.D. HASSELMANN                                                          !
!     MODIFIED : P.JANSSEN (INTEGRATION OF F**-4 TAIL)                         !
!     OPTIMIZED BY : L. ZAMBRESKY AND H. GUENTHER                              !
!     H. GUNTHER     GKSS         DECEMBER 2001    FT90                        !
!                                                                              !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTE MEAN FREQUENCY AND MEAN WAVE NUMBER AT EACH GRID POINT.        !
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

REAL,    INTENT(IN)            :: F(:,:,:)      !! BLOCK OF SPECTRA.
REAL,    INTENT(IN)            :: EMEAN(:)      !! TOTAL ENERGY.
REAL,    INTENT(OUT)           :: FM   (:)      !! MEAN FREQUENCY.
LOGICAL, INTENT(IN),  OPTIONAL :: MASK (:,:,:)  !! INTERATION MASK.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL     :: DELT25, DEL2
REAL     :: TEMP2(SIZE(F,1),SIZE(F,3))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INTEGRATE OVER DIRECTIONS.                                            !
!        ---------------------------                                           !

IF (PRESENT(MASK)) THEN
   TEMP2 = SUM(F,DIM=2, MASK=MASK)
ELSE
   TEMP2 = SUM(F,DIM=2)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTE MEAN FREQUENCY.                                               !
!        -----------------------                                               !

FM = MAT_MUL(TEMP2,DFIMOFR)       !! INTEGRATE OVER FREQUENCIES.

DELT25 = FRTAIL*DELTH            !! ADD TAIL.
FM= FM+DELT25*TEMP2(:,SIZE(F,3))
FM = EMEAN/MAX(FM,TINY(1.))      !! NORMALIZE WITH TOTAL ENERGY.
 
END SUBROUTINE FEMEAN

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE INTSPEC (DEL12, DEL1L,  F1, FMEAN1, EMEAN1, THETM1,                 &
&                   F2, FMEAN2, EMEAN2, THETM2, FL, FMEAN, EMEAN, THETM )

! ---------------------------------------------------------------------------- !
!                                                                              !
!   INTSPEC  -  INTERPOLATION OF SPECTRA.                                      !
!                                                                              !
!     SUSANNE HASSELMANN  MPI        JUNE 1990.                                !
!     H. GUNTHER          GKSS/ECMWF JAN. 1991   MODIFIED FOR CYCLE_4          !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       INTERPOLATION OF SPECTRA.                                              !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       ROTATE SPECTRA ACCORDING TO MEAN OF MEAN ANGLES, TRANSFORM             !
!       FREQUENCIES ACCORDING TO MEAN OF MEAN FREQUENCIES ,ADJUST ENERGY       !
!       ACCORDCING TO MEAN OF TOTAL ENERGY AND INTERPOLATE RESULTING           !
!       SPECTRA.                                                               !
!                                                                              !
!     REFERENCES.                                                              !
!     -----------                                                              !
!                                                                              !
!       K.HASSELMANN, 1990,                                                    !
!          INTERPOLATION OF WAVE SPECTRA. WAM NOTE 6/6/90.                     !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

REAL,    INTENT(IN)  :: DEL12         !! DISTANCE SPECTRUM 2 - SPECTRUM 1.
REAL,    INTENT(IN)  :: DEL1L         !! DISTANCE SPECTRUM L - SPECTRUM 1.
REAL,    INTENT(IN)  :: F1(:,:)       !! SPECTRUM 1.
REAL,    INTENT(IN)  :: FMEAN1        !! MEAN FREQUENCY OF F1.
REAL,    INTENT(IN)  :: EMEAN1        !! MEAN ENERGY OF F1.
REAL,    INTENT(IN)  :: THETM1        !! MEAN DIRECTION OF F1.
REAL,    INTENT(IN)  :: F2(:,:)       !! SPECTRUM 2.
REAL,    INTENT(IN)  :: FMEAN2        !! MEAN FREQUENCY OF F2.
REAL,    INTENT(IN)  :: EMEAN2        !! MEAN ENERGY OF F2.
REAL,    INTENT(IN)  :: THETM2        !! MEAN DIRECTION OF F2.
REAL,    INTENT(OUT) :: FL(: ,:)      !! INTEPOLATED SPECTRUM.
REAL,    INTENT(OUT) :: FMEAN         !! INTEPOLATED MEAN FREQUENCY.
REAL,    INTENT(OUT) :: EMEAN         !! INTEPOLATED MEAN ENERGY.
REAL,    INTENT(OUT) :: THETM         !! INTEPOLATED MEAN DIRECTION.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL :: GW1, GW2
REAL :: F_L(SIZE(F1,1),SIZE(F1,2)), F_R(SIZE(F1,1),SIZE(F1,2))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INTERPOLATION WEIGHTS.                                                !
!        ----------------------                                                !

GW2 = DEL1L/DEL12
GW1 = 1. - GW2

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. LEFT WEIGHT OR ENERGY OF LEFT SPECTRUM IS ZERO.                       !
!        -----------------------------------------------                       !

IF (ABS(GW1).LT.EPSILON(1.) .OR. EMEAN1.LT.EPSILON(1.)) THEN
   FL = GW2*F2
   EMEAN = GW2*EMEAN2
   FMEAN = GW2*FMEAN2
   THETM = GW2*THETM2
   RETURN
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. RIGHT WEIGHT OR ENERGY OF RIGHT SPECTRUM IS ZERO.                     !
!        -------------------------------------------------                     !

IF (ABS(GW2).LT.EPSILON(1.) .OR. EMEAN2.LT.EPSILON(1.)) THEN
   FL = GW1*F1
   EMEAN = GW1*EMEAN1
   FMEAN = GW1*FMEAN1
   THETM = GW1*THETM1
   RETURN
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. ENERGY AND WEIGHTS OF BOTH SPECTRA ARE GT ZERO.                       !
!        -----------------------------------------------                       !

!     3.1 INTERPOLATE MEAN VALUES.                                             !

EMEAN = GW1*EMEAN1+GW2*EMEAN2
FMEAN = GW1*FMEAN1+GW2*FMEAN2
THETM = ATAN2 (GW1*SIN(THETM1)+GW2*SIN(THETM2),GW1*COS(THETM1)+GW2*COS(THETM2))

!     3.2 ADJUST LEFT SPECTRUM TO MEAN VALUES.                                 !

CALL ROTSPEC (F1, FL, THETM-THETM1)        !! ROTATE.
CALL STRSPEC (FL, F_L, FMEAN1/FMEAN)       !! STRETCH.
GW1 = GW1*EMEAN/EMEAN1                     !! ADJUST ENERGY.

!    3.3 ADJUST RIGHT SPECTRUM TO MEAN VALUES.                                 !

CALL ROTSPEC (F2, FL, THETM-THETM2)        !! ROTATE.
CALL STRSPEC (FL, F_R, FMEAN2/FMEAN)       !! STRETCH.
GW2 = GW2*EMEAN/EMEAN2                     !! ADJUST ENERGY.

!      3.4 LINEAR INTERPOLATION TO NEW SPECTRA.                                !

FL = GW1*F_L + GW2*F_R

END SUBROUTINE INTSPEC

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE MEAN_DIRECTION_B (F3, THQ, SPREAD)

! ---------------------------------------------------------------------------- !
!                                                                              !
!  MEAN_DIRECTION_B - COMPUTATION OF MEAN WAVE DIRECTION FOR BLOCK.            !
!                                                                              !
!     S.D. HASSELMANN                                                          !
!     OPTIMIZED BY L. ZAMBRESKY                                                !
!     MODIFIED FOR K-MODEL BY C.SCHNEGGENBURGER                                !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO COMPUTE MEAN WAVE DIRECTION FROM ENERGY DENSITY AT EACH GRID POINT. !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       INTEGRATION OF SPECTRUM TIMES SIN AND COS OVER DIRECTION.              !
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
!     INTERFACE VARIABLE                                                       !

REAL, INTENT(IN)            :: F3(:,:,:)   !! BLOCK OF DENSITY SPECTRA.
REAL, INTENT(OUT), OPTIONAL :: THQ(:)      !! MEAN DIRECTION [RAD].
REAL, INTENT(OUT), OPTIONAL :: SPREAD(:)   !! MEAN SPREAD [RAD].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLE                                                           !

INTEGER         :: K
REAL            :: SI(1:SIZE(F3,1)), CI(1:SIZE(F3,1)), TEMP(1:SIZE(F3,1))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INITIALISE SIN AND COS ARRAYS.                                        !
!        ------------------------------                                        !

SI = 0.
CI = 0.

IF (PRESENT(SPREAD)) SPREAD = 0.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. INTEGRATE OVER FREQUENCIES AND DIRECTIONS.                            !
!        ------------------------------------------                            !

DO K = 1,SIZE(F3,2)
   TEMP = MAT_MUL(F3(:,K,:),DFIM)
   SI = SI + SINTH(K)*TEMP
   CI = CI + COSTH(K)*TEMP
   IF (PRESENT(SPREAD)) SPREAD = SPREAD + TEMP
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. COMPUTE MEAN DIRECTION.                                               !
!        -----------------------                                               !

IF (PRESENT(THQ)) THEN
   WHERE (CI.EQ.0.) CI = 0.1E-30
   THQ = ATAN2(SI,CI)
   WHERE (THQ.LT.0.) THQ = THQ + ZPI
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. COMPUTE MEAN SPREAD.                                                  !
!        --------------------                                                  !

IF (PRESENT(SPREAD)) THEN
   WHERE (SPREAD.GT.0.) SPREAD = 2.*(1.-SQRT(SI**2 + CI**2)/SPREAD)
   WHERE (SPREAD.LE.0.)
      SPREAD = TINY(1.)
   ELSEWHERE
      SPREAD = SQRT(SPREAD)
   END WHERE
END IF

END SUBROUTINE MEAN_DIRECTION_B

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE MEAN_DIRECTION_1 (F3, THQ, SPREAD)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   MEAN_DIRECTION_1 - COMPUTATION OF MEAN WAVE DIRECTION ONE SPECTRUM.        !
!                                                                              !
!     S.D. HASSELMANN                                                          !
!     OPTIMIZED BY L. ZAMBRESKY                                                !
!     MODIFIED FOR K-MODEL BY C.SCHNEGGENBURGER                                !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO COMPUTE MEAN WAVE DIRECTION FROM ONE SPECTRUM.                      !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       INTEGRATION OF SPECTRUM TIMES SIN AND COS OVER DIRECTION.              !
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
!     INTERFACE VARIABLE                                                       !

REAL, INTENT(IN)            :: F3(:,:)  !! DENSITY SPECTRUM.
REAL, INTENT(OUT), OPTIONAL :: THQ      !! MEAN DIRECTION [RAD].
REAL, INTENT(OUT), OPTIONAL :: SPREAD   !! MEAN SPREAD [RAD].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLE                                                           !

REAL     :: SI, CI
REAL     :: TEMP(1:SIZE(F3,1))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INTEGRATE OVER FREQUENCIES AND DIRECTIONS.                            !
!        ------------------------------------------                            !

TEMP = MAT_MUL(F3, DFIM)
SI = DOT_PRODUCT(TEMP,SINTH)
CI = DOT_PRODUCT(TEMP,COSTH)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTE MEAN DIRECTION.                                               !
!        -----------------------                                               !

IF (PRESENT(THQ)) THEN
   IF (CI.EQ.0.) CI = 0.1E-30
   THQ = ATAN2(SI,CI)
   IF (THQ.LT.0.) THQ = THQ + ZPI
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. COMPUTE MEAN SPREAD.                                                  !
!        --------------------                                                  !

IF (PRESENT(SPREAD)) THEN
   SPREAD = SUM(TEMP)
   IF (SPREAD.GT.0.) SPREAD = 2.*(1.-SQRT(SI**2 + CI**2)/SPREAD)
   IF (SPREAD.LE.0.) THEN
      SPREAD = TINY(1.)
   ELSE
      SPREAD = SQRT(SPREAD)
   END IF
END IF

END SUBROUTINE MEAN_DIRECTION_1

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PEAK_PERIOD_B (F, PEAKP, MASK)

!------------------------------------------------------------------------------!
!                                                                              !
!    PEAK_PERIOD_B - COMPUTATES PEAK PERIOD (VECTOR VERSION).                  !
!                                                                              !
!     H. GUNTHER      ECMWF            DECEMBER 1989                           !
!     (CODE REMOVED FROM SUB. FEMEAN)                                          !
!     H. GUNTHER      GKSS            FEBRUARY 2002  CHANGED TO PERIOD.        !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTE PEAK PERIOD AT EACH GRID POINT.                                !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE FREQUENCY INDEX OF THE 1-D SPECTRA ARE COMPUTED AND                !
!       CONVERTED TO PERIODS.                                                  !
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

REAL,    INTENT(IN)            :: F(:,:,:)    !! BLOCK OF SPECTRA.
REAL,    INTENT(OUT)           :: PEAKP(:)    !! PEAK PERIODS.
LOGICAL, INTENT(IN),  OPTIONAL :: MASK(:,:,:) !! INTEGRATION MASK.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER  :: IJ
INTEGER  :: IPEAK(SIZE(F,1))
REAL     :: EED1D(SIZE(F,1),SIZE(F,3))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COMPUTE 1-D SPECTRUM (WITHOUT DELTA THETA).                           !
!        -------------------------------------------                           !

IF (PRESENT(MASK)) THEN
   EED1D = SUM(F, DIM=2, MASK=MASK)
ELSE
   EED1D = SUM(F, DIM=2)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. DEFINE PEAK INDEX.                                                    !
!        ------------------                                                    !

DO IJ = 1,SIZE(F,1)
   IPEAK(IJ:IJ) = MAXLOC(EED1D(IJ,:))
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. CALCULATE PEAK PERIOD FROM PEAK INDEX.                                !
!        --------------------------------------                                !

PEAKP = 1./FR(IPEAK)
WHERE (IPEAK.EQ.1) PEAKP = 1.

END SUBROUTINE PEAK_PERIOD_B

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PEAK_PERIOD_1 (F, PEAKP, MASK)

!------------------------------------------------------------------------------!
!                                                                              !
!    PEAK_PERIOD_1 - COMPUTATES PEAK PERIOD (SCALAR VERSION).                  !
!                                                                              !
!     H. GUNTHER      ECMWF            DECEMBER 1989                           !
!     (CODE REMOVED FROM SUB. FEMEAN)                                          !
!     H. GUNTHER      GKSS            FEBRUARY 2002  CHANGED TO PERIOD.        !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTE PEAK PERIOD.                                                   !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE FREQUENCY INDEX OF THE 1-D SPECTRUM IS COMPUTED AND                !
!       CONVERTED TO PERIOD.                                                   !
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

REAL,    INTENT(IN)            :: F(:,:)     !! SPECTRUM.
REAL,    INTENT(OUT)           :: PEAKP      !! PEAK PERIOD.
LOGICAL, INTENT(IN),  OPTIONAL :: MASK(:,:)  !! INTEGRATION MASK.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER  :: IJ
INTEGER  :: IPEAK(1:1)
REAL     :: EED1D(SIZE(F,2))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COMPUTE 1-D SPECTRUM (WITHOUT DELTA THETA).                           !
!        -------------------------------------------                           !

IF (PRESENT(MASK)) THEN
   EED1D = SUM(F, DIM=1, MASK=MASK)
ELSE
   EED1D = SUM(F, DIM=1)
END IF


! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. DEFINE PEAK INDEX.                                                    !
!        ------------------                                                    !

IPEAK(1:1) = MAXLOC(EED1D)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. CALCULATE PEAK PERIOD FROM PEAK INDEX.                                !
!        --------------------------------------                                !

PEAKP = 1./FR(IPEAK(1))
IF (IPEAK(1).EQ.1) PEAKP = 1.

END SUBROUTINE PEAK_PERIOD_1

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE ROTSPEC (F_IN, F_OUT, RTHET)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   ROTSPEC - ROUTINE TO ROTATE THE SPECTRUM.                                  !
!                                                                              !
!     EVA BAUER      MPI  HAMBURG    MAY 1990.                                 !
!     H. GUNTHER          GKSS/ECMWF JAN. 1991   MODIFIED FOR CYCLE_4          !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       TO ROTATE THE SPECTRUM.                                                !
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
!                                                                              !
!     INTERFACE VARIABLE.                                                      !
!     -------------------                                                      !

REAL,    INTENT(IN)  :: F_IN(:,:)      !! SPECTRUM TO BE ROTATED.
REAL,    INTENT(OUT) :: F_OUT(:,:)     !! ROTATED SPECTRUM.
REAL,    INTENT(IN) ::  RTHET          !! TURNING ANGLE [RAD], CLOCKWISE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER         :: INC
REAL            :: ADIF, BDIF

! ---------------------------------------------------------------------------- !

ADIF = RTHET * REAL(SIZE(F_IN,1)) / ZPI
INC = -FLOOR(ADIF)
ADIF = ADIF + REAL(INC)
BDIF = 1. - ADIF

F_OUT = BDIF * CSHIFT(F_IN, INC) + ADIF * CSHIFT(F_IN, INC-1)

END SUBROUTINE ROTSPEC

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE STRSPEC (F_IN, F_OUT, GAMMA)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   STRSPEC - ROUTINE TO STRETCH A SPECTRUM.                                   !
!                                                                              !
!      EVA BAUER      MPI  HAMBURG    MAY 1990.                                !
!      H. GUNTHER     GKSS/ECMWF      JAN 1991  MODIFIED FOR CYCLE_4.          !
!      H. GUNTHER     GKSS            JAN 2002  FT90.                          !
!                                               ERROR FOR SHIFT TO HIGER       !
!                                               FREQUENCIES CORRECTED.         !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
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
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

REAL,    INTENT(IN)  :: F_IN (:,:)    !! INPUT SPECTRUM.
REAL,    INTENT(OUT) :: F_OUT(:,:)    !! OUTPUT SPECTRUM.
REAL,    INTENT(IN)  :: GAMMA         !! STRETCHING PARAMETER.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER         :: ML, INC
REAL            :: ADIF, BDIF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INITIALIZATION.                                                       !
!        ---------------                                                       !

IF (GAMMA.EQ.1.0) THEN
   F_OUT = F_IN
   RETURN
END IF

F_OUT = 0.0
ML = SIZE(F_IN,2)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. DETERMINE ACROSS HOW MANY FREQUENCY BINS THE  STRETCHING IS ACTING    !
!        AND THE INTERPOLATION WEIGHTS.                                        !
!        --------------------------------------------------------------------  !

INC = FLOOR(LOG10(GAMMA)/LOG10(CO))
IF (ABS(INC).GE.ML-1) RETURN      !! ENERGY IS SHIFTED OUT OF FREQUENCY RANGE

ADIF = (CO -GAMMA*CO**(-INC))/(CO-1.)
BDIF = 1. - ADIF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. STRECH SPECTRUM.                                                      !
!        ----------------                                                      !

IF (INC.GE.0) THEN

!     3.1 SHIFT TO LOWER FREQUENCIES.                                          !

   F_OUT(:,1:ML-INC-1) = ADIF*F_IN(:,1+INC:ML-1) + BDIF*F_IN(:,2+INC:ML)
   F_OUT(:,ML-INC)     = ADIF*F_IN(:,ML)
ELSE

!      3.2 SHIFT TO HIGHER FREQUENCIES.                                        !

   F_OUT(:,1-INC:ML) = ADIF*F_IN(:,1:ML+INC) + BDIF*F_IN(:,2:ML+INC+1)
   F_OUT(:,-INC)     = BDIF*F_IN(:,1)
END IF

END SUBROUTINE STRSPEC

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE TM1_TM2_PERIODS_B (F, EMEAN, TM1, TM2, MASK)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   TM1_TM2_PERIODS_B - COMPUTES TM1 AND/OR TM2 PERIODS (VECTOR VESION).       !
!                                                                              !
!     C.SCHNEGGENBURGER 08/97.                                                 !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTE TM1 AND TM2 PERIODS.                                           !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       INTEGARATION OF SPECTRA AND ADDING OF TAIL FACTORS.                    !
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

REAL,    INTENT(IN )           :: F(:,:,:)    !! BLOCK OF SPECTRA.
REAL,    INTENT(IN )           :: EMEAN(:)    !! TOTAL ENERGY [M*M].
REAL,    INTENT(OUT), OPTIONAL :: TM1(:)      !! TM1 PERIOD [S].
REAL,    INTENT(OUT), OPTIONAL :: TM2(:)      !! TM2 PERIOD [S].
LOGICAL, INTENT(IN),  OPTIONAL :: MASK(:,:,:) !! INTEGRATION MASK.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL    :: TLFR
REAL    :: TEMP(SIZE(F,1),SIZE(F,3))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INTEGRATE OVER DIRECTIONS (WITHOUT DELTH).                            !
!        ------------------------------------------                            !

IF (PRESENT(MASK)) THEN
   TEMP = SUM(F, DIM=2, MASK=MASK)
ELSE
   TEMP = SUM(F, DIM=2)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. TM1 PERIOD.                                                           !
!        -----------                                                           !

IF (PRESENT(TM1)) THEN
   TM1 = MAT_MUL(TEMP, DFFR)           !! FREQUENCY INTEGRATION.

   TLFR = WP1TAIL*(FR(SIZE(F,3)))**2*DELTH     !! ADD TAIL CORRECTION.
   TM1    = TM1 + TLFR * TEMP(:,SIZE(F,3))

   TM1 = MERGE (EMEAN/TM1, 1., TM1.GT.TINY(1.)) !! NORMALIZE WITH ENERGY.
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. TM2 PERIOD.                                                           !
!        -----------                                                           !

IF (PRESENT(TM2)) THEN
   TM2 = MAT_MUL(TEMP, DFFR2)          !! FREQUENCY INTEGRATION.

   TLFR = WP2TAIL*(FR(SIZE(F,3)))**3*DELTH       !! ADD TAIL CORRECTION.
   TM2    = TM2 + TLFR * TEMP(:,SIZE(F,3))

   TM2 = MERGE (SQRT(EMEAN/TM2), 1., TM2.GT.TINY(1.)) !! NORMALIZE WITH ENERGY.
END IF

END SUBROUTINE TM1_TM2_PERIODS_B

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE TM1_TM2_PERIODS_1 (F, EMEAN, TM1, TM2, MASK)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   TM1_TM2_PERIODS_1 - COMPUTES TM1 AND/OR TM2 PERIODS (SCALAR VESION).       !
!                                                                              !
!     C.SCHNEGGENBURGER 08/97.                                                 !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTE TM1 AND TM2 PERIODS.                                           !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       INTEGARATION OF SPECTRA AND ADDING OF TAIL FACTORS.                    !
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

REAL,    INTENT(IN )           :: F(:,:)    !! BLOCK OF SPECTRA.
REAL,    INTENT(IN )           :: EMEAN     !! TOTAL ENERGY [M*M].
REAL,    INTENT(OUT), OPTIONAL :: TM1       !! TM1 PERIOD [S].
REAL,    INTENT(OUT), OPTIONAL :: TM2       !! TM2 PERIOD [S].
LOGICAL, INTENT(IN),  OPTIONAL :: MASK(:,:) !! INTEGRATION MASK.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL    :: TLFR, FDK(SIZE(F,2))
REAL    :: TEMP(SIZE(F,2))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INTEGRATE OVER DIRECTIONS (WITHOUT DELTH).                            !
!        ------------------------------------------                            !

IF (PRESENT(MASK)) THEN
   TEMP = SUM(F, DIM=1, MASK=MASK)
ELSE
   TEMP = SUM(F, DIM=1)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. TM1 PERIOD.                                                           !
!        -----------                                                           !

IF (PRESENT(TM1)) THEN
   TM1 = DOT_PRODUCT(TEMP, DFFR)       !! FREQUENCY INTEGRATION.

   TLFR = WP1TAIL*(FR(SIZE(F,2)))**2*DELTH     !! ADD TAIL CORRECTION.
   TM1    = TM1 + TLFR * TEMP(SIZE(F,2))

   IF (EMEAN.GT.TINY(1.)) THEN        !! NORMALIZE WITH TOTAL ENERGY.
      TM1 = EMEAN / TM1
   ELSE
      TM1    = 1.
   END IF
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. TM2 PERIOD.                                                           !
!        -----------                                                           !

IF (PRESENT(TM2)) THEN
   TM2 = DOT_PRODUCT(TEMP, DFFR2)       !! FREQUENCY INTEGRATION.

   TLFR = WP2TAIL*(FR(SIZE(F,2)))**3*DELTH       !! ADD TAIL CORRECTION.
   TM2    = TM2 + TLFR * TEMP(SIZE(F,2))

   IF (EMEAN.GT.TINY(1.)) THEN        !! NORMALIZE WITH TOTAL ENERGY.
      TM2 = SQRT(EMEAN/TM2)
   ELSE
      TM2    = 1.
   END IF
END IF

END SUBROUTINE TM1_TM2_PERIODS_1

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE TOTAL_ENERGY_B (F3, EMEAN, MASK)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   TOTAL_ENERGY_B - COMPUTES TOTAL ENERGY (VECTOR VERSION).                   !
!                                                                              !
!     S.D. HASSELMANN.                                                         !
!     OPTIMIZED BY: L. ZAMBRESKY AND H. GUENTHER                               !
!     H. GUENTHER     GKSS   DECEMBER 2001  FT90                               !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO COMPUTE TOTAL ENERGY AT EACH GRID POINT.                            !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       INTEGRATION OVER DIRECTION AND FREQUENCY. A TAIL CORRECTION IS ADDED.  !
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

REAL,    INTENT(IN)            :: F3(:,:,:)    !! BLOCK OF SPECTRA.
REAL,    INTENT(OUT)           :: EMEAN(:)     !! TOTAL ENERGY.
LOGICAL, INTENT(IN), OPTIONAL  :: MASK(:,:,:)  !! INTEGRATION MASK.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL     :: DELT25
REAL     :: TEMP(SIZE(F3,1),SIZE(F3,3))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INTEGRATE OVER FREQUENCIES AND DIRECTION.                             !
!        -----------------------------------------                             !

IF (PRESENT(MASK)) THEN
   TEMP = SUM(F3, DIM=2, MASK=MASK)
ELSE
   TEMP = SUM(F3, DIM=2)
END IF
EMEAN = MAT_MUL(TEMP,DFIM)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. ADD TAIL ENERGY.                                                      !
!        ----------------                                                      !

DELT25 = WETAIL*FR(SIZE(F3,3))*DELTH
EMEAN = EMEAN + DELT25*TEMP(:,SIZE(F3,3))
EMEAN = MAX(EMEAN,TINY(1.))

END SUBROUTINE TOTAL_ENERGY_B

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE TOTAL_ENERGY_1 (F3, EMEAN, MASK)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   TOTAL_ENERGY_1 - COMPUTES TOTAL ENERGY (SCALAR VERSION).                   !
!                                                                              !
!     S.D. HASSELMANN.                                                         !
!     OPTIMIZED BY: L. ZAMBRESKY AND H. GUENTHER                               !
!     H. GUENTHER     GKSS   DECEMBER 2001  FT90                               !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO COMPUTE TOTAL ENERGY AT EACH GRID POINT.                            !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       INTEGRATION OVER DIRECTION AND FREQUENCY. A TAIL CORRECTION IS ADDED.  !
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

REAL,    INTENT(IN)           :: F3(:,:)    !! SPECTRUM.
REAL,    INTENT(OUT)          :: EMEAN      !! TOTAL ENERGY.
LOGICAL, INTENT(IN), OPTIONAL :: MASK(:,:)  !! INTEGRATION MASK.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL     :: DELT25
REAL     :: TEMP(SIZE(F3,2))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INTEGRATE OVER FREQUENCIES AND DIRECTION.                             !
!        -----------------------------------------                             !

IF (PRESENT(MASK)) THEN
   TEMP = SUM(F3 ,DIM=1,MASK=MASK)
ELSE
   TEMP = SUM(F3, DIM=1)
END IF
EMEAN = DOT_PRODUCT(TEMP,DFIM)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. ADD TAIL ENERGY.                                                      !
!        ----------------                                                      !

DELT25 = WETAIL*FR(SIZE(F3,2))*DELTH
EMEAN = EMEAN + DELT25*TEMP(SIZE(F3,2))
EMEAN = MAX(EMEAN,TINY(1.))

END SUBROUTINE TOTAL_ENERGY_1

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE COS2_SPR_1 (TH, THES, ST)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     COS2_SPR - ROUTINE TO COMPUTE SPREADING FACTOR (SCALAR VERSION).         !
!                                                                              !
!     SUSANNE HASSELMANN  JULY 1986.                                           !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTATION OF COS**2 SPREADING FUNCTION.                              !
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

REAL,    INTENT(IN) :: TH(:)       !! DIRECTIONS.
REAL,    INTENT(IN) :: THES        !! MEAN WAVE DIRECTION.
REAL,    INTENT(OUT) :: ST(:)      !! SPREADING FUNCTION.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL, PARAMETER :: ZDP=2./PI

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COSINE SQUARE SPREAD.                                                         !
!     ------------------------                                                         !

ST(:) = MAX(0. ,COS(TH(:)-THES))
ST = ZDP*ST**2
WHERE (ST.LT.0.1E-08) ST = 0.

END SUBROUTINE COS2_SPR_1

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE COS2_SPR_B (TH, THES, ST)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     COS2_SPR - ROUTINE TO COMPUTE SPREADING FACTOR (VECTOR VERSION).         !
!                                                                              !
!     SUSANNE HASSELMANN  JULY 1986.                                           !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTATION OF COS**2 SPREADING FUNCTION.                              !
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

REAL,    INTENT(IN) :: TH(:)       !! DIRECTIONS.
REAL,    INTENT(IN) :: THES(:)     !! MEAN WAVE DIRECTIONS.
REAL,    INTENT(OUT) :: ST(:,:)    !! SPREADING FUNCTION.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL, PARAMETER :: ZDP=2./PI
INTEGER         :: K

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COSINE SQUARE SPREAD.                                                         !
!     ------------------------                                                         !

DO K = 1,SIZE(TH)
   ST(:,K) = MAX(0. ,COS(TH(K)-THES(:)))
END DO
ST = ZDP*ST**2
WHERE (ST.LT.0.1E-08) ST = 0.

END SUBROUTINE COS2_SPR_B

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE WM1_WM2_WAVENUMBER_B (F, EMEAN, WM1, WM2, IN, MASK)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   WM1_WM2_WAVENUMBER_B - COMPUTES WM1 AND/OR WM2 WAVENUMBERS (VECTOR VESION).!
!                                                                              !
!     C.SCHNEGGENBURGER 08/97.                                                 !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTE  WM1 AND/OR WM2 WAVENUMBERS                                    !
!          WM1 IS SQRT(1/K)*F INTGRATION                                       !
!          WM2 IS SQRT(K)*F INTGRATION                                         !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       INTEGARATION OF SPECTRA AND ADDING OF TAIL FACTORS.                    !
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

REAL,    INTENT(IN )           :: F(:,:,:)    !! BLOCK OF SPECTRA.
REAL,    INTENT(IN )           :: EMEAN(:)    !! TOTAL ENERGY [M*M].
REAL,    INTENT(OUT), OPTIONAL :: WM1(:)      !! WM1 WAVENUMBER [M].
REAL,    INTENT(OUT), OPTIONAL :: WM2(:)      !! WM2 WAVENUMBER [M].
INTEGER, INTENT(IN),  OPTIONAL :: IN   (:)    !! DEPTH TABLE INDEX.
LOGICAL, INTENT(IN),  OPTIONAL :: MASK(:,:,:) !! INTEGRATION MASK.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL    :: DEL2
REAL    :: TEMP(SIZE(F,1),SIZE(F,3))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INTEGRATE OVER DIRECTIONS (WITHOUT DELTH).                            !
!        ------------------------------------------                            !

IF (PRESENT(MASK)) THEN
   TEMP = SUM(F, DIM=2, MASK=MASK)
ELSE
   TEMP = SUM(F, DIM=2)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTE MEAN WAVE NUMBER WM1.                                         !
!        -----------------------------                                         !

IF (PRESENT(WM1)) THEN
   DEL2 = SQRT(G)/ZPI
   IF (PRESENT(IN)) THEN
      WM1 = MAT_MUL(TEMP/SQRT(TFAK(IN,:)), DFIM)
   ELSE
      WM1 = MAT_MUL(TEMP, DEL2*DFIMOFR)
   END IF

   DEL2 = FRTAIL*DELTH*DEL2            !! ADD TAIL.
   WM1 = WM1 + DEL2*TEMP(:,SIZE(F,3))
   WM1 = MERGE ((EMEAN/WM1)**2, 1., WM1.GT.TINY(1.)) !! NORMALIZE WITH ENERGY.
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTE MEAN WAVE NUMBER WM2.                                         !
!        -----------------------------                                         !

IF (PRESENT(WM2)) THEN
   DEL2 = ZPI/SQRT(G)
   IF (PRESENT(IN)) THEN
      WM2 = MAT_MUL(TEMP*SQRT(TFAK(IN,:)), DFIM)
   ELSE
      WM2 = MAT_MUL(TEMP, DEL2*DFFR)
   END IF

   DEL2 = WP1TAIL*DELTH*DEL2*FR(SIZE(F,3))**2            !! ADD TAIL.
   WM2 = WM2 + DEL2*TEMP(:,SIZE(F,3))
   WM2 = MERGE ((WM2/EMEAN)**2, 1., WM2.GT.TINY(1.))  !! NORMALIZE WITH ENERGY.
END IF

END SUBROUTINE WM1_WM2_WAVENUMBER_B

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE WM1_WM2_WAVENUMBER_1 (F, EMEAN, WM1, WM2, IN, MASK)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   WM1_WM2_WAVENUMBER_1 - COMPUTES WM1 AND/OR WM2 WAVENUMBER (SCALAR VESION). !
!                                                                              !
!     C.SCHNEGGENBURGER 08/97.                                                 !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTE  WM1 AND/OR WM2 WAVENUMBERS                                    !
!          WM1 IS SQRT(1/K)*F INTGRATION                                       !
!          WM2 IS SQRT(K)*F INTGRATION                                         !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       INTEGARATION OF SPECTRA AND ADDING OF TAIL FACTORS.                    !
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

REAL,    INTENT(IN )           :: F(:,:)    !! SPECTRUM.
REAL,    INTENT(IN )           :: EMEAN     !! TOTAL ENERGY [M*M].
REAL,    INTENT(OUT), OPTIONAL :: WM1       !! WM1 WAVENUMBER [M].
REAL,    INTENT(OUT), OPTIONAL :: WM2       !! WM2 WAVENUMBER [M].
INTEGER, INTENT(IN),  OPTIONAL :: IN        !! DEPTH TABLE INDEX.
LOGICAL, INTENT(IN),  OPTIONAL :: MASK(:,:) !! INTEGRATION MASK.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

REAL    :: DEL2
REAL    :: TEMP(SIZE(F,2))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INTEGRATE OVER DIRECTIONS (WITHOUT DELTH).                            !
!        ------------------------------------------                            !

IF (PRESENT(MASK)) THEN
   TEMP = SUM(F, DIM=1, MASK=MASK)
ELSE
   TEMP = SUM(F, DIM=1)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTE MEAN WAVE NUMBER WM1.                                         !
!        -----------------------------                                         !

IF (PRESENT(WM1)) THEN
   DEL2 = SQRT(G)/ZPI
   IF (PRESENT(IN)) THEN
      WM1 = DOT_PRODUCT(TEMP/SQRT(TFAK(IN,:)), DFIM)
   ELSE
      WM1 = DOT_PRODUCT(TEMP, DEL2*DFIMOFR)
   END IF

   DEL2 = FRTAIL*DELTH*DEL2            !! ADD TAIL.
   WM1 = WM1 + DEL2*TEMP(SIZE(F,2))
   WM1 = MERGE ((EMEAN/WM1)**2, 1., WM1.GT.TINY(1.)) !! NORMALIZE WITH ENERGY.
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. COMPUTE MEAN WAVE NUMBER WM2.                                         !
!        -----------------------------                                         !

IF (PRESENT(WM2)) THEN
   DEL2 = ZPI/SQRT(G)
   IF (PRESENT(IN)) THEN
      WM2 = DOT_PRODUCT(TEMP*SQRT(TFAK(IN,:)), DFIM)
   ELSE
      WM2 = DOT_PRODUCT(TEMP, DEL2*DFFR)
   END IF

   DEL2 = WP1TAIL*DELTH*DEL2*FR(SIZE(F,2))**2            !! ADD TAIL.
   WM2 = WM2 + DEL2*TEMP(SIZE(F,2))
   WM2 = MERGE ((WM2/EMEAN)**2, 1., WM2.GT.TINY(1.)) !! NORMALIZE WITH ENERGY.
END IF

END SUBROUTINE WM1_WM2_WAVENUMBER_1

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

FUNCTION MAT_MUL (A, B) RESULT(C)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   MAT_MUL - ROUTINE TO MULTIPLY A MATRIX OF RANK 2 WITH A VECTOR.            !
!                                                                              !
!     H. GUNTHER          GKSS JAN. 2005                                       !
!                                                                              !
!     PURPOSE.                                                                 !
!     -------                                                                  !
!                                                                              !
!       REPLACE THE INTRINSIC FUNCTION MAT_MUL.                                 !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       MULTIPLICATION AND SUMMATION                                           !
!       IT MUST TO BE                                                          !
!          SIZE(B) = SIZE(A,2) AND SIZE(C) = SIZE(A,1)                         !
!       THIS IS NOT CHECKED BY THE ROUTINE                                     !
!                                                                              !
!                                                                              !
!     REFERENCES.                                                              !
!     -----------                                                              !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     INTERFACE VARIABLES.                                                     !
!     --------------------                                                     !

REAL, INTENT(IN) :: A(:,:)   !! MATRIX OF RANK 2
REAL, INTENT(IN) :: B(:)     !! VECTOR 

REAL :: C(SIZE(A,1))  !! RESULT OF A*B

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER :: I, K   !! LOOP CONTROL

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INITIAL RESULT VECTOR.                                                !
!        ----------------------                                                !

C = 0.      !! INITIAL RESULT VECTOR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. MULTIPLY.                                                             !
!        ----------                                                            !

DO K = 1, SIZE(A,2)
   DO I = 1, SIZE(A,1)
      C(I) = C(I) + A(I,K)*B(K)
   END DO
END DO

END FUNCTION MAT_MUL

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_INTERFACE_MODULE
