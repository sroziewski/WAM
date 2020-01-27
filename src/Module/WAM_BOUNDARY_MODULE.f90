MODULE WAM_BOUNDARY_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE STORES THE BOUNDARY INPUT VALUES FOR A FINE GRID RUN.          !
!   THE VALUES WERE PRODUCED BY A PREVIOUS COARSE GRID RUN.                    !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     A.  EXTERNALS.                                                           !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1,                  &  !! TERMINATES PROCESSING.
&       ADJUST,                  &  !! ADJUST LONGITUDES.
&       DIFDATE,                 &  !! COMPUTE TIME DIFFERENCE.
&       MAKE_BOX,                &  !! MAKE BOX IN A GRID.
&       OPEN_FILE,               &  !! OPENS A FILE.
&       INCDATE                     !! INCREMENT A DATE.

USE WAM_INTERFACE_MODULE, ONLY:  &
&       FEMEAN,                  &  !! COMPUTATION OF MEAN FREQUENCY.
&       INTSPEC,                 &  !! INTERPOLATE A SPECTRUM.
&       MEAN_DIRECTION,          &  !! COMPUTATION OF MEAN DIRECTION AND SPREAD.
&       TOTAL_ENERGY                !! COMPUTATION OF TOTAL ENERGY.

USE WAM_GRID_MODULE,      ONLY:  &
&       FIND_SEA_POINT              !! FIND BLOCK AND SEA POINT NUMBERS.

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     B. VARIABLES FROM OTHER MODULES.                                         !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_FILE_MODULE,    ONLY: IU06, ITEST, FILE02, IU10, FILE10, IU19, FILE19, &
&                             CDTRES, IDELRES
USE WAM_TIMOPT_MODULE,  ONLY: CDATEE, IDELPRO, CDTPRO
USE WAM_GENERAL_MODULE, ONLY: ZPI, EPS
USE WAM_FRE_DIR_MODULE, ONLY: KL, ML, CO, FR, TH
USE WAM_GRID_MODULE,    ONLY: NX, NY, XDELLA, XDELLO,                          &
&                             AMOWEP, AMOSOP, AMOEAP, AMONOP, IPER

USE WAM_NEST_MODULE,    ONLY: COARSE, FINE, N_NEST,                            &
&                             NBINP, NBOUNF,IJARF,IBFL,IBFR, BFW,              &
&                             NBOUNC, BLNGC, BLATC, IJARC 

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     C. MODULE VARIABLES.                                                     !
!                                                                              !
! ---------------------------------------------------------------------------- !

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. BOUNDARY VALUES FROM COARSE GRID.                                     !
!        ---------------------------------                                     !

CHARACTER (LEN=14) :: CDTLAST = ' ' !! DATE OF LAST FETCHED FILE.
INTEGER            :: IDELINP       !! TIMESTEP OF BOUNDARY VALUES.
REAL, ALLOCATABLE  :: XLAT(:)       !! LATITUDES OF INPUT SPECTRA.
REAL, ALLOCATABLE  :: XLON(:)       !! LONGITUDES OF INPUT SPECTRA.

!            FIRST TIME SPECTRA FROM COARSE GRID.                              !

CHARACTER (LEN=14) :: CDATE1 = ' '  !! DATE OF SPECTRA.
REAL, ALLOCATABLE  :: F1(:,:,:)     !! SPECTRA FROM COARSE GRID.
REAL, ALLOCATABLE  :: FMEAN1(:)     !! MEAN FREQUENCIES FROM COARSE GRID.
REAL, ALLOCATABLE  :: EMEAN1(:)     !! TOTAL ENERGIES FROM COARSE GRID.
REAL, ALLOCATABLE  :: THQ1(:)       !! MEAN DIRECTIONS FROM COARSE GRID (RAD).

!            SECOND TIME SPECTRA FROM COARSE GRID.                             !

CHARACTER (LEN=14) :: CDATE2 = ' '  !! DATE OF SPECTRA.
REAL, ALLOCATABLE  :: F2(:,:,:)     !! SPECTRA FROM COARSE GRID.
REAL, ALLOCATABLE  :: FMEAN2(:)     !! MEAN FREQUENCIES FROM COARSE GRID.
REAL, ALLOCATABLE  :: EMEAN2(:)     !! TOTAL ENERGIES FROM COARSE GRID.
REAL, ALLOCATABLE  :: THQ2(:)       !! MEAN DIRECTIONS FROM COARSE GRID (RAD).

!            TIME INTERPOLATED COARSE GRID SPECTRA.                            !

CHARACTER (LEN=14) :: CDATEI = ' '  !! DATE OF SPECTRA.
REAL, ALLOCATABLE  :: FI(:,:,:)     !! INTERPOLATED SPECTRUM.
REAL, ALLOCATABLE  :: FMEANI(:)     !! MEAN FREQUENCIES FROM COARSE GRID.
REAL, ALLOCATABLE  :: EMEANI(:)     !! TOTAL ENERGIES FROM COARSE GRID.
REAL, ALLOCATABLE  :: THQI(:)       !! MEAN DIRECTIONS FROM COARSE GRID (RAD).

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     D.  PUBLIC INTERFACES.                                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

INTERFACE BOUNDARY_INPUT             !! BOUNDARY VALUE INPUT.
   MODULE PROCEDURE BOUNDARY_INPUT
END INTERFACE
PUBLIC BOUNDARY_INPUT

INTERFACE BOUNDARY_OUTPUT            !!  OUTPUT OF BOUNDARY VALUES.
   MODULE PROCEDURE BOUNDARY_OUTPUT
END INTERFACE
PUBLIC BOUNDARY_OUTPUT

INTERFACE CHECK_BOUNDARY             !! CHECKS BOUNDARY OPTIONS.
   MODULE PROCEDURE CHECK_BOUNDARY 
END INTERFACE
PUBLIC CHECK_BOUNDARY

INTERFACE SAVE_BOUNDARY_FILE         !! SAVES AND OPENS BOUNDARY OUTPUT FILES.
   MODULE PROCEDURE SAVE_BOUNDARY_FILE 
END INTERFACE
PUBLIC SAVE_BOUNDARY_FILE

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

SUBROUTINE BOUNDARY_INPUT (FL3)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   BOUNDARY_INPUT - BOUNDARY VALUE INPUT INTO THE WAM MODEL.                  !
!                                                                              !
!     H. GUNTHER    GKSS/ECMWF   JANUARY 1991                                  !
!     H. GUNTHER    GKSS         JANUARY 2002    FT90                          !
!                                          - TIME INTERPOLATION OF SPECTRA.    !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO INTERPOLATE BOUNDARY SPECTRA IN TIME AND SPACE AND INSERT THE       !
!       BOUNDARY SPECTRA INTO THE WAM MODEL FIELD.                             !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       WHEN THE SUB IS CALLED FOR THE FIRST THE SUB. READS A COMPLETE         !
!       SET OF BOUNDARY VALUES AND PERFORMS THE TIME INTERPOLATION.            !
!       THE SPECTRA REQUIRED FOR THE ACTUAL BLOCK ARE INTERPOLATED IN SPACE    !
!       AND STORED IN THE BLOCK. INDICES AND WEIGHTS NECESSARY FOR THE SPACE   !
!       INTERPOLATION AND STORAGE ARE PRECOMPUTED IN PROG. PREPROC.            !
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

REAL,    INTENT(INOUT) :: FL3(:,:,:)  !! BLOCK OF SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

CHARACTER (LEN=14), SAVE :: ZERO = ' '

INTEGER      :: IJ, IDEL1L, IJF, IBCL, IBCR
REAL         :: DEL12, DEL1L, FMEAN, EMEAN, THQ

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. INITIALIZATION.                                                       !
!        ---------------                                                       !

!     1.1 FIRST CALL OF SUB THEN INITIALISE FOR LANDPOINTS.                    !
!         -------------------------------------------------                    !

IF (CDTLAST.EQ.ZERO) THEN
   IF (.NOT.ALLOCATED(XLON)  ) ALLOCATE (XLON(1:NBINP))
   IF (.NOT.ALLOCATED(XLAT)  ) ALLOCATE (XLAT(1:NBINP))
   IF (.NOT.ALLOCATED(F1)    ) ALLOCATE (F1(1:KL,1:ML,1:NBINP))
   IF (.NOT.ALLOCATED(FMEAN1)) ALLOCATE (FMEAN1(1:NBINP))
   IF (.NOT.ALLOCATED(EMEAN1)) ALLOCATE (EMEAN1(1:NBINP))
   IF (.NOT.ALLOCATED(THQ1)  ) ALLOCATE (THQ1(1:NBINP))
   IF (.NOT.ALLOCATED(F2)    ) ALLOCATE (F2(1:KL,1:ML,1:NBINP))
   IF (.NOT.ALLOCATED(FMEAN2)) ALLOCATE (FMEAN2(1:NBINP))
   IF (.NOT.ALLOCATED(EMEAN2)) ALLOCATE (EMEAN2(1:NBINP))
   IF (.NOT.ALLOCATED(THQ2)  ) ALLOCATE (THQ2(1:NBINP))
   IF (.NOT.ALLOCATED(FI)    ) ALLOCATE (FI(1:KL,1:ML,0:NBINP))
   IF (.NOT.ALLOCATED(FMEANI)) ALLOCATE (FMEANI(0:NBINP))
   IF (.NOT.ALLOCATED(EMEANI)) ALLOCATE (EMEANI(0:NBINP))
   IF (.NOT.ALLOCATED(THQI)  ) ALLOCATE (THQI(0:NBINP))
   FI(:,:,0) = 0.
   FMEANI(0) = 0.
   EMEANI(0) = 0.
   THQI(0)   = 0.

   CALL READ_BOUNDARY_INPUT
   IF (ITEST.GT.3)  WRITE (IU06,*)                                            &
&  '       SUB. BOUNDARY_INPUT: FIRST BOUNDARY VALUES READ CDATE2 = ', CDATE2
END IF

DO WHILE (CDATE2.LT.CDTPRO)
   CDATE1 = CDATE2
   FMEAN1 = FMEAN2
   EMEAN1 = EMEAN2
   THQ1   = THQ2
   F1 = F2
   CALL READ_BOUNDARY_INPUT
   IF (ITEST.GT.3)  WRITE (IU06,*)                                            &
&  '       SUB. BOUNDARY_INPUT: SECOND BOUNDARY VALUES READ CDATE2 = ', CDATE2
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. TIME INTERPOLATION.                                                   !
!        -------------------                                                   !

IF (CDTPRO.EQ.CDATE1 ) THEN
   CDATEI = CDATE1
   FMEANI(1:) = FMEAN1
   EMEANI(1:) = EMEAN1
   THQI  (1:) = THQ1
   FI(:,:,1:) = F1
ELSE IF (CDTPRO.EQ.CDATE2 .OR. CDATE1.EQ.' ') THEN
   CDATEI = CDATE1
   FMEANI(1:) = FMEAN2
   EMEANI(1:) = EMEAN2
   THQI  (1:) = THQ2
   FI(:,:,1:) = F2
ELSE IF (CDTPRO.GT.CDATE1 .AND. CDTPRO.LT.CDATE2) THEN
   CDATEI = CDTPRO
   CALL DIFDATE (CDATE1, CDATEI, IDEL1L)
   DEL1L = REAL(IDEL1L)
   DEL12 = REAL(IDELINP)
   DO IJ = 1,NBINP
      CALL INTSPEC (DEL12, DEL1L,                                              &
&                   F1(:,:,IJ), FMEAN1(IJ), EMEAN1(IJ), THQ1(IJ),              &
&                   F2(:,:,IJ), FMEAN2(IJ), EMEAN2(IJ), THQ2(IJ),              &
&                   FI(:,:,IJ), FMEANI(IJ), EMEANI(IJ), THQI(IJ))
   END DO
   IF (ITEST.GT.3)  WRITE (IU06,*)                                             &
&  '       SUB. BOUNDARY_INPUT: TIME INTERPOLATION DONE     CDATEI = ', CDATEI
ELSE
   WRITE (IU06,*) '*******************************************'
   WRITE (IU06,*) '*                                         *'
   WRITE (IU06,*) '*   FATAL ERROR SUB. BOUNDARY_INPUT.      *'
   WRITE (IU06,*) '*   ================================      *'
   WRITE (IU06,*) '* DATES DO NOT MATCH.                     *'
   WRITE (IU06,*) '* DATE OF FIRST SPECTRA IS  CDATE1 =  ', CDATE1
   WRITE (IU06,*) '* MODEL DATE IS             CDTPRO =  ', CDTPRO
   WRITE (IU06,*) '* DATE OF SECOND SPECTRA IS CDATE2 =  ', CDATE2
   WRITE (IU06,*) '*                                         *'
   WRITE (IU06,*) '* PROGRAM ABORTS.   PROGRAM ABORTS.       *'
   WRITE (IU06,*) '*                                         *'
   WRITE (IU06,*) '*******************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. SPACE INTERPOLATION.                                                  !
!        --------------------                                                  !

DEL12 = 1.
DO IJ = 1,NBOUNF
      IJF = IJARF(IJ)
      IBCL = IBFL(IJ)
      IBCR = IBFR(IJ)
      DEL1L = BFW(IJ)
      CALL INTSPEC (DEL12, DEL1L,                                             &
&                   FI(:,:,IBCL), FMEANI(IBCL), EMEANI(IBCL), THQI(IBCL),     &
&                   FI(:,:,IBCR), FMEANI(IBCR), EMEANI(IBCR), THQI(IBCR),     &
&                   FL3(IJF,:,:), FMEAN, EMEAN, THQ)

END DO

END SUBROUTINE BOUNDARY_INPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE BOUNDARY_OUTPUT (FL3)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   BOUNDARY_OUTPUT - OUTPUT OF THE COARSE GRID BOUNDARY VALUES.               !
!                                                                              !
!     R. PORTZ     MPI          JANUARY 1991                                   !
!     H. GUENTHER  GKSS         JANUARY 2002    FT90.                          !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!        WRITE THE BOUNDARY VALUE OUTPUT FILE.                                 !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       SEQUENCIAL UNFORMATED WRITE TO UNIT.                                   !
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

REAL,    INTENT(IN) :: FL3(:,:,:) !! BLOCK OF SPECTRA.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER :: I, IU
INTEGER :: IJ
REAL    :: THQC   (1:SIZE(FL3,1))
REAL    :: EMEANC (1:SIZE(FL3,1))
REAL    :: FMEANC (1:SIZE(FL3,1))

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. COMPUTE MEAN PARAMETERS.                                              !
!        ------------------------                                              !

CALL TOTAL_ENERGY (FL3, EMEANC)
CALL FEMEAN (FL3, EMEANC, FM=FMEANC)
CALL MEAN_DIRECTION (FL3, THQ=THQC)


! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. GATHER BOUNDARY SPECTRA.                                              !

IF (ITEST.GE.3) THEN
   WRITE(IU06,*) '      SUB. BOUNDARY_OUTPUT: INTEGRATED PARAMETERS ',         &
&                                   'COMPUTED FOR BOUNDARY POINTS OUTPUT'
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. WRITE BOUNDARY SPECTRA.                                               !
!        -----------------------                                               !

IU = IU19-1
DO I = 1, N_NEST
   IU = IU+1
   DO IJ = 1,NBOUNC(I)
      WRITE(IU) BLNGC(IJ,I), BLATC(IJ,I), CDTPRO, EMEANC(IJARC(IJ,I)),         &
&              THQC(IJARC(IJ,I)), FMEANC(IJARC(IJ,I))
      WRITE(IU) FL3(IJARC(IJ,I),:,:)
   END DO
END DO

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. SAVE BOUNDARY FILE.                                                   !
!        -------------------                                                   !

IF (CDTRES.EQ.CDTPRO .OR. CDATEE.EQ.CDTPRO) THEN
   CALL SAVE_BOUNDARY_FILE 
END IF

END SUBROUTINE BOUNDARY_OUTPUT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE CHECK_BOUNDARY

IF (COARSE .AND. N_NEST.LE.0) THEN
   COARSE = .FALSE.
   WRITE (IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++'
   WRITE (IU06,*) '+                                           +'
   WRITE (IU06,*) '+   WARNING ERROR SUB. CHECK_BOUNDARY.      +'
   WRITE (IU06,*) '+   ==================================      +'
   WRITE (IU06,*) '+ NESTS ARE NOT DEFINED IN THE PREPROC FILE +'
   WRITE (IU06,*) '+ BUT COARSE GRID RUN IS REQUESTED.         +'
   WRITE (IU06,*) '+                                           +'
   WRITE (IU06,*) '+ MODEL OPTION CHANGED TO COARSE = .FALSE.  +'
   WRITE (IU06,*) '+                                           +'
   WRITE (IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++'
END IF
IF (FINE .AND. NBINP.LE.0) THEN
   WRITE (IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++'
   WRITE (IU06,*) '+                                           +'
   WRITE (IU06,*) '+   WARNING ERROR SUB. CHECK_BOUNDARY.      +'
   WRITE (IU06,*) '+   ==================================      +'
   WRITE (IU06,*) '+ NESTS ARE NOT DEFINED IN THE PREPROC FILE +'
   WRITE (IU06,*) '+ BUT FINE GRID RUN IS REQUESTED.           +'
   WRITE (IU06,*) '+                                           +'
   WRITE (IU06,*) '+ MODEL OPTION CHANGED TO FINE = .FALSE.    +'
   WRITE (IU06,*) '+                                           +'
   WRITE (IU06,*) '+++++++++++++++++++++++++++++++++++++++++++++'
   FINE = .FALSE.
END IF

END SUBROUTINE CHECK_BOUNDARY

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE SAVE_BOUNDARY_FILE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   SAVE_BOUNDARY_FILE - SAVES A BOUNDARY VALUE OUPUT FILE.                    !
!                                                                              !
!     H. GUNTHER    GKSS/ECMWF    OCTOBER 1989                                 !
!     P. JANSSEN    KNMI          OCTOBER 1990   YMP-MODIFICATION              !
!     H. GUNTHER    GKSS/ECMWF    OCTOBER 1990   NEW FILE NAMES.               !
!     H. GUNTHER    GKSS          NOVEMBER 1999  NEW DATES AND FT90.           !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO SAVE THE BOUNDARY VALUE OUTPUT FILE OF A COARSE GRID RUN.           !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!        THE ASSIGNED FILE IS CLOSED AND A NEW ONE IS ASSIGNED TO THE UNIT,    !
!        IF THE MODEL DATE IS BEFORE THE END OF RUN DATE.                      !
!        THE NEW FILES ARE OPENED BY SUB. GFILE.                               !
!                                                                              !
!                                                                              !
!     REFERENCES.                                                              !
!      -----------                                                             !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLE.                                                          !
!     ---------------                                                          !

CHARACTER (LEN=14) :: CDATEN
INTEGER            :: IFAIL      !! OPEN ERROR CODE
INTEGER            :: I, IU

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. CLOSE OLD FILE.                                                       !
!     ------------------                                                       !

IU = IU19-1
DO I = 1, N_NEST
   IU = IU+1
   IF (N_NEST.GT.1) WRITE(FILE19(2:3),'(I2.2)' ) I
   CLOSE (UNIT=IU, STATUS ="KEEP") !! BOUNDARY VALUE FILE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. OPEN A NEW FILE IF MODEL DATE IS BEFORE END DATE.                     !
!        -------------------------------------------------                     !

   IF (CDTPRO.LT.CDATEE) THEN 
      CDATEN = CDTPRO                      !! DATE OF NEW FILE.
      CALL INCDATE(CDATEN, IDELRES)
      CALL OPEN_FILE (IU06, IU, FILE19, CDATEN, 'UNKNOWN', IFAIL)
      IF (IFAIL.NE.0) CALL ABORT1

!     WRITE HEADER.

      WRITE(IU) REAL(KL), REAL(ML), TH(1), FR(1), CO, REAL(NBOUNC(I)), REAL(IDELPRO)
   END IF
END DO

END SUBROUTINE SAVE_BOUNDARY_FILE

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
!                                                                              !
!     G. PRIVATE MODULE PROCEDURES.                                            !
!                                                                              !
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

END MODULE WAM_BOUNDARY_MODULE
