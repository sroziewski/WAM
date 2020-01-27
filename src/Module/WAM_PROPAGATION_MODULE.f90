MODULE WAM_PROPAGATION_MODULE

! ---------------------------------------------------------------------------- !
!                                                                              !
!   THIS MODULE CONTAINS ALL VARIABLES, CONSTANTS AND PROCEDURES FOR THE       !
!   PROPAGATION.                                                               !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_GENERAL_MODULE, ONLY: PI, CIRC, ZPI, R, DEG
USE WAM_FRE_DIR_MODULE, ONLY: ML, KL, FR, TH, GOM, DELTH, DELTR, COSTH, SINTH, &
&                             TCGOND, TFAK, TSIHKD, NDEPTH, DEPTHA, DEPTHD
USE WAM_GRID_MODULE,    ONLY: NSEA, DEPTH, IXLG, KXLT, KLAT, KLON,             &
&                             DELPHI, DELLAM, SINPH, COSPH, INDEP, ONE_POINT 
USE WAM_CURRENT_MODULE, ONLY: U, V
USE WAM_TIMOPT_MODULE,  ONLY: IDELPRO, IDELT, ICASE, ISHALLO, IREFRA
USE WAM_FILE_MODULE,    ONLY: IU06, IDELRES
USE WAM_WIND_MODULE,    ONLY: IDELWI

! ---------------------------------------------------------------------------- !
!                                                                              !
!      EXTERNALS.                                                              !
!     -----------                                                              !

USE WAM_GENERAL_MODULE, ONLY:  &
&       ABORT1                      !! TERMINATES PROCESSING.

! ---------------------------------------------------------------------------- !

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. NUMBER OF ADVECTION ITERATIONS PER CALL OF WAMODEL.                   !
!        ---------------------------------------------------                   !

INTEGER :: NADV = 0

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. LATITUDE LONGITUDE PROPAGATION.                                       !
!        -------------------------------                                       !

REAL, ALLOCATABLE :: CGOND(:,:)  !! SHALLOW WATER GROUP VELOCITIES.
REAL, ALLOCATABLE :: DCO(:)      !! 1./ COS(LATITUDE).
REAL, ALLOCATABLE :: DP1(:)      !! COS(LATITUDE SOUTH)/COS(LATITUDE).
REAL, ALLOCATABLE :: DP2(:)      !! COS(LATITUDE NORTH)/COS(LATITUDE).

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. DEPTH AND CURRENT PART OF THETA DOT.                                  !
!        ------------------------------------                                  !

REAL, ALLOCATABLE :: THDD(:,:)    !! DEPTH GRADIENT PART OF THETA DOT.
REAL, ALLOCATABLE :: THDC(:,:)    !! CURRENT GRADIENT PART OF THETA DOT.
REAL, ALLOCATABLE :: SIDC(:,:,:)  !! SIGMA DOT.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. FREQUENCY, DIRECTION NEIGHTBOURS.                                     !
!        ------------------------------------                                  !

INTEGER, ALLOCATABLE :: MP(:)    !! FREQUENCY INDEX +1.
INTEGER, ALLOCATABLE :: MM(:)    !! FREQUENCY INDEX -1.
INTEGER, ALLOCATABLE :: KP(:)    !! DIRECTION INDEX +1.
INTEGER, ALLOCATABLE :: KM(:)    !! DIRECTION INDEX -1.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. CFL INFORMATION.                                                      !
!        ----------------                                                      !

REAL, PARAMETER :: XLIMIT = 0.99       !! MAXIMUM ALLOWED CFL NUMBER
REAL    :: CFLMAX = 0.                 !! MAXIMUM CFL NUMBER
INTEGER :: MAXPOINT(3)                 !! POINT, DIRECTION AND FREQUENCY INDEX
                                       !! OF MAXIMUM CFL NUMBER.
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

INTERFACE CHECK_CFL                   !! CFL CHECK.
   MODULE PROCEDURE CHECK_CFL
END INTERFACE
PRIVATE CHECK_CFL

INTERFACE GRADIENT                    !! CALCULATES GRADIENTS.
   MODULE PROCEDURE GRADIENT
END INTERFACE
PRIVATE GRADIENT

INTERFACE PREPARE_PROPAGATION         !! PREPARES PROPAGATION.
   MODULE PROCEDURE PREPARE_PROPAGATION
END INTERFACE
PUBLIC PREPARE_PROPAGATION

INTERFACE PRINT_PROPAGATION_STATUS    !! PRINTS PROPAGATION STATUS.
   MODULE PROCEDURE PRINT_PROPAGATION_STATUS
END INTERFACE
PUBLIC PRINT_PROPAGATION_STATUS

INTERFACE PROPAGS                     !! PROPAGATION.
   MODULE PROCEDURE PROPAGS
END INTERFACE
PUBLIC PROPAGS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

CONTAINS

SUBROUTINE CHECK_CFL

! ---------------------------------------------------------------------------- !
!                                                                              !
! *** *CHECK_CFL* - CFL CHECK.                                                 !
!                                                                              !
!     H. GUNTHER   GKSS   FEBRUARY 2002                                        !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO CHECK THE PROPAGATION TIME STEP.                                    !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       FOR EACH GRID POINT, FREQUENCY AND DIRECTION THE RELATIVE LOSS OF      !
!       ENERGY PER TIME STEP IS COMPUTED AND CHECKED TO BE LESS THAN 1.        !
!                                                                              !
!     INTERNAL SUBROUTINES.                                                    !
!     ---------------------                                                    !
!                                                                              !
!         SPHERICAL GRID.                                                      !
!                                                                              !
!       *C_SPHER_DEEP*         DEEP WATER WITHOUT CURRENT REFRACTION.          !
!       *C_SPHER_SHALLOW*      SHALLOW WATER WITHOUT CURRENT REFRACTION.       !
!       *C_SPHER_DEEP_CURR*    DEEP WATER WITH CURRENT REFRACTION.             !
!       *C_SPHER_SHALLOW_CURR* SHALLOW WATER WITH DEPTH AND CURRENT REFRACTION.!
!                                                                              !
!         CARTESIAN GRID.                                                      !
!                                                                              !
!       *C_CART_DEEP*          DEEP WATER WITHOUT CURRENT REFRACTION.          !
!       *C_CART_SHALLOW*       SHALLOW WATER WITHOUT CURRENT REFRACTION.       !
!       *C_CART_DEEP_CUR*      DEEP WATER WITH CURRENT REFRACTION.             !
!       *C_CART_SHALLOW_CUR*   SHALLOW WATER WITH DEPTH AND CURRENT REFRACTION.!
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


! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !


INTEGER :: K, M, IS, IC

REAL    :: DELLA0, DELPH0, DELTH0
REAL    :: DELFR0, DFP, DFM, CGS, CGC, SM, SP
REAL    :: SD, CD, SDA, CDA

REAL,DIMENSION(0:NSEA)     :: DPH, DLA
REAL,DIMENSION(1:NSEA)     :: DTP, DTM, DRGM, DTC


REAL    :: CF(1:NSEA,KL,ML)   !! CFL NUMBERS.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     0. INITIAL.                                                              !
!        --------                                                              !

CF = 0.           !! INITIAL CFL NUMBER

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1.0 SELECT CASE.                                                         !
!         -------------                                                        !

IF (ICASE.EQ.1) THEN

!     1.1 CFL CHECK FOR SPHERICAL GRID.                                        !
!         -----------------------------                                        !

   IF (IREFRA.NE.2) THEN
      IF (ISHALLO.EQ.1) THEN
         CALL C_SPHER_DEEP     !! DEEP WATER WITHOUT CURRENT REFRACTION.
      ELSE
         CALL C_SPHER_SHALLOW  !! SHALLOW WATER WITHOUT CURRENT REFRACTION.
      END IF
   ELSE
      IF (ISHALLO.EQ.1) THEN
         CALL C_SPHER_DEEP_CURR    !! DEEP WATER WITH CURRENT REFRACTION.
      ELSE
         CALL C_SPHER_SHALLOW_CURR !! SHALLOW WATER WITH DEPTH AND CURRENT REF.
      END IF
   END IF

ELSE

!     1.2 CFL CHECK FOR CARTESIAN GRID.                                        !
!         -----------------------------                                        !

   IF (IREFRA.NE.2) THEN
      IF (ISHALLO.EQ.1) THEN
         CALL C_CART_DEEP         !! DEEP WATER WITHOUT CURRENT REFRACTION.
      ELSE
         CALL C_CART_SHALLOW      !! SHALLOW WATER WITHOUT CURRENT REFRACTION.
      END IF

   ELSE
      IF (ISHALLO.EQ.1) THEN
         CALL C_CART_DEEP_CUR     !! DEEP WATER WITH CURRENT REFRACTION.
      ELSE
         CALL C_CART_SHALLOW_CUR  !! SHALLOW WATER WITH DEPTH AND CURRENT REF.
      END IF

   END IF
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2.0 EVALUATE CFL.                                                        !
!         -------------                                                        !

CFLMAX = MAXVAL(CF)
MAXPOINT = MAXLOC(CF)

IF (CFLMAX .GT. XLIMIT) THEN
   WRITE(IU06,*)
   WRITE(IU06,*) ' *******************************************'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *    FATAL ERROR IN SUB. CFLCHECK         *'
   WRITE(IU06,*) ' *    ============================         *'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' * VIOLATIONS OF CFL-CRITERIUM             *'
   WRITE(IU06,*) ' * MAXIMUM CFL = ', CFLMAX
   WRITE(IU06,*) ' *    AT POINT = ', MAXPOINT(1)
   WRITE(IU06,*) ' *        KLON = ', IXLG(MAXPOINT(1))
   WRITE(IU06,*) ' *        KLAT = ', KXLT(MAXPOINT(1))
   WRITE(IU06,*) ' *   DIRECTION = ', TH(MAXPOINT(2))*DEG
   WRITE(IU06,*) ' *   FREQUENCY = ', FR(MAXPOINT(3))
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' * PROGRAM ABORTS     PROGRAM ABORTS       *'
   WRITE(IU06,*) ' *                                         *'
   WRITE(IU06,*) ' *******************************************'
   CALL ABORT1
END IF

RETURN

! ---------------------------------------------------------------------------- !

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.0 INTERNAL SUBROUTINES.                                                !
!         ---------------------                                                !

CONTAINS

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.1 CFL CHECK FOR CARTESIAN GRID WITHOUT CURRENT REFRACTION (DEEP).      !
!         ---------------------------------------------------------------      !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE C_CART_DEEP

   DELPH0 = REAL(IDELPRO)/DELPHI
   DELLA0 = REAL(IDELPRO)/DELLAM
   DIR: DO K = 1,KL
      SD = ABS(SINTH(K)*DELLA0)+ABS(COSTH(K)*DELPH0)
      FRE: DO M = 1,ML
         CF(:,K,M) = SD*GOM(M)
      END DO FRE
   END DO DIR

END SUBROUTINE C_CART_DEEP

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.2 CFL CHECK FOR CARTESIAN GRID WITHOUT CURRENT REFRACTION (SHALLOW).   !
!         ------------------------------------------------------------------   !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE C_CART_SHALLOW

   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)

      IF (SD.LT.0) THEN                  !! INDEX FOR ADJOINING LONGITUDE.
         IS = 1
      ELSE
         IS = 2
      END IF
      IF (CD.LT.0) THEN                  !! INDEX FOR ADJOINING LATITUDE.
         IC = 1
      ELSE
         IC = 2
      END IF

      SD = ABS(SD*DELLA0)
      CD = ABS(CD*DELPH0)

      FRE: DO M = 1,ML
         CF(:,K,M) = SD*(CGOND(KLON(:,IS),M) + CGOND(:,M))                     &
&                  + CD*(CGOND(KLAT(:,IC),M) + CGOND(:,M))
         IF (IREFRA.EQ.1) THEN
            CF(:,K,M) = CF(:,K,M)                                              &
&                + TSIHKD(INDEP,M)*(MAX(0.,THDD(:,K  ))-MIN(0.,THDD(:,KM(K))))
         END IF
      END DO FRE
   END DO DIR

END SUBROUTINE C_CART_SHALLOW

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.3 CFL CHECK FOR CARTESIAN GRID WITH CURRENT REFRACTION (DEEP).         !
!         ------------------------------------------------------------         !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE C_CART_DEEP_CUR

   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM
   DELFR0 = 0.5*REAL(IDELPRO)*2.1/0.2

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)

      FRE: DO M = 1,ML

         CGS = GOM(M)*SD               !! GROUP VELOCITIES.
         CGC = GOM(M)*CD

         DLA(0) = CGS*DELLA0
         DPH(0) = CGC*DELPH0
         DLA(1:) = (U + CGS)*DELLA0
         DPH(1:) = (V + CGC)*DELPH0

         CF(:,K,M) = MAX(0. , DLA(1:) + DLA(KLON(:,2)))                        &
&                  - MIN(0. , DLA(1:) + DLA(KLON(:,1)))                        &
&                  + MAX(0. , DPH(1:) + DPH(KLAT(:,2)))                        &
&                  - MIN(0. , DPH(1:) + DPH(KLAT(:,1)))                        &
&                  + MAX(0. , THDC(:,K  )) - MIN(0. , THDC(:,KM(K)))           &
&                  + ABS(SIDC(:,K,ML)*DELFR0)

      END DO FRE
   END DO DIR

END SUBROUTINE C_CART_DEEP_CUR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.4 CFL CHECK FOR CARTESIAN GRID WITH CURRENT REFRACTION (SHALLOW).      !
!         ---------------------------------------------------------------      !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE C_CART_SHALLOW_CUR

   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM
   DELFR0 = 0.5*REAL(IDELPRO)/(0.1*ZPI)

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)

      FRE: DO M = 1,ML
         DFP = DELFR0/FR(M)
         DFM = DELFR0/FR(MM(M))

         DLA(0) = SD*CGOND(0,M)*DELLA0
         DPH(0) = CD*CGOND(0,M)*DELPH0
         DLA(1:) = (U + SD*CGOND(1:,M)) * DELLA0
         DPH(1:) = (V + CD*CGOND(1:,M)) * DELPH0

         CF(:,K,M) = MAX(0. , DLA(1:) + DLA(KLON(:,2)))                        &
&                  - MIN(0. , DLA(1:) + DLA(KLON(:,1)))                        &
&                  + MAX(0. , DPH(1:) + DPH(KLAT(:,2)))                        &
&                  - MIN(0. , DPH(1:) + DPH(KLAT(:,1)))                        &
&                  + MAX(0. , TSIHKD(INDEP,M)*THDD(:,K    ) + THDC(:,K    ))   &
&                  - MIN(0. , TSIHKD(INDEP,M)*THDD(:,KM(K)) + THDC(:,KM(K)))   &
&                  + MAX(0. , (SIDC(:,K,M) + SIDC(:,K,MP(M)))*DFP)             &
&                  - MIN(0. , (SIDC(:,K,M) + SIDC(:,K,MM(M)))*DFM)
      END DO FRE
   END DO DIR

END SUBROUTINE C_CART_SHALLOW_CUR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.5 CFL CHECK FOR SPHERICAL GRID WITHOUT CURRENT REFRACTION (DEEP).      !
!         ---------------------------------------------------------------      !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE C_SPHER_DEEP

   DELTH0 = 0.5*REAL(IDELPRO)/DELTR
   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELLA0 = REAL(IDELPRO)/DELLAM

   DIR: DO K = 1,KL
      SP  = DELTH0*(SINTH(K)+SINTH(KP(K)))
      SM  = DELTH0*(SINTH(K)+SINTH(KM(K)))
      DRGM = SINPH(KXLT)*DCO
      DTC = MAX(0. , DRGM*SP) - MIN(0. , DRGM*SM) + DCO*ABS(SINTH(K))*DELLA0

      IF (COSTH(K).GT.0.) THEN
         DTC = DTC + DELPH0*COSTH(K)*(DP2 + 1.)
      ELSE
         DTC = DTC - DELPH0*COSTH(K)*(DP1 + 1.)
      END IF

      FRE: DO M = 1,ML
         CF(:,K,M) = DTC*GOM(M)
      END DO FRE
   END DO DIR

END SUBROUTINE C_SPHER_DEEP

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.6 CFL CHECK FOR SPHERICAL GRID WITHOUT CURRENT REFRACTION (SHALLOW).   !
!         ------------------------------------------------------------------   !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE C_SPHER_SHALLOW

   DELTH0 = 0.5*REAL(IDELPRO)/DELTR
   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)
      SDA = ABS(SD)
      CDA = ABS(CD)

!     COMPUTE GRID REFRACTION.                                                 !

      SP  = DELTH0*(SINTH(K)+SINTH(KP(K)))
      SM  = DELTH0*(SINTH(K)+SINTH(KM(K)))
      DRGM = SINPH(KXLT)*DCO

      IF (SD.LT.0) THEN          !! INDEX FOR ADJOINING POINTS.
         IS = 1
      ELSE
         IS = 2
      END IF
      IF (CD.LT.0) THEN
         IC = 1
      ELSE
         IC = 2
      END IF

      FRE: DO M = 1,ML
         DTC = DCO*SDA*DELLA0 * (CGOND(KLON(:,IS),M)     + CGOND(1:,M))        &
&            +     CDA*DELPH0 * (CGOND(KLAT(:,IC),M)*DP2 + CGOND(1:,M))

         DTP = DRGM*SP*CGOND(1:,M)      !! REFRACTION WEIGHTS.
         DTM = DRGM*SM*CGOND(1:,M)

         IF (IREFRA.NE.0) THEN      !! ADD DEPTH REFRACTION TO GRID REFRACTION
            DTP = DTP + TSIHKD(INDEP,M)*THDD(:,K  )
            DTM = DTM + TSIHKD(INDEP,M)*THDD(:,KM(K))
         END IF

         CF(:,K,M) = DTC + MAX(0.,DTP) - MIN(0.,DTM)
      END DO FRE
END DO DIR

END SUBROUTINE C_SPHER_SHALLOW

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.7 CFL CHECK FOR SPHERICAL GRID WITH CURRENT REFRACTION (DEEP).         !
!         ------------------------------------------------------------         !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE C_SPHER_DEEP_CURR

   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM
   DELTH0 = 0.5*REAL(IDELPRO)/DELTR
   DELFR0 = 0.5*REAL(IDELPRO)*2.1/0.2

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)

      SP = DELTH0*(SINTH(K)+SINTH(KP(K)))      !! GRID REFRACTION.
      SM = DELTH0*(SINTH(K)+SINTH(KM(K)))
      DRGM = SINPH(KXLT)*DCO

      FRE: DO M = 1,ML

         CGS = GOM(M)*SD                     !! GROUP VELOCITIES.
         CGC = GOM(M)*CD

         DLA(0) = CGS*DELLA0
         DPH(0) = CGC*DELPH0
         DLA(1:) = (U + CGS)*DELLA0 * DCO
         DPH(1:) = (V + CGC)*DELPH0

         CF(:,K,M) = MAX(0. , DLA(1:) + DLA(KLON(:,2)))                        &
&                  - MIN(0. , DLA(1:) + DLA(KLON(:,1)))                        &
&                  + MAX(0. , DPH(1:) + DPH(KLAT(:,2))*DP2)                    &
&                  - MIN(0. , DPH(1:) + DPH(KLAT(:,1))*DP1)                    &
&                  + MAX(0. , DRGM*SP*GOM(M) + THDC(:,K    ))                  &
&                  - MIN(0. , DRGM*SM*GOM(M) + THDC(:,KM(K)))                  &
&                  + ABS(SIDC(:,K,ML) * DELFR0)

      END DO FRE
   END DO DIR

END SUBROUTINE C_SPHER_DEEP_CURR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.8 CFL CHECK FOR SPHERICAL GRID WITH CURRENT REFRACTION (SHALLOW).      !
!         ---------------------------------------------------------------      !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE C_SPHER_SHALLOW_CURR

   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM
   DELTH0 = 0.5*REAL(IDELPRO)/DELTR
   DELFR0 = 0.5*REAL(IDELPRO)/(0.1*ZPI)

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)

      SP = DELTH0*(SINTH(K)+SINTH(KP(K)))    !! GRID REFRACTION.
      SM = DELTH0*(SINTH(K)+SINTH(KM(K)))
      DRGM = SINPH(KXLT)*DCO

      FRE: DO M = 1,ML
         DFP = DELFR0/FR(M)
         DFM = DELFR0/FR(MM(M))

         DLA(0) = SD*CGOND(0,M)*DELLA0
         DPH(0) = CD*CGOND(0,M)*DELPH0
         DLA(1:) = (U + SD*CGOND(1:,M)) * DELLA0*DCO
         DPH(1:) = (V + CD*CGOND(1:,M)) * DELPH0

         CF(:,K,M) = MAX(0. , DLA(1:) + DLA(KLON(:,2)))                        &
&                  - MIN(0. , DLA(1:) + DLA(KLON(:,1)))                        &
&                  + MAX(0. , DPH(1:) + DPH(KLAT(:,2))*DP2)                    &
&                  - MIN(0. , DPH(1:) + DPH(KLAT(:,1))*DP1)                    &
&                  + MAX(0. , DRGM*SP*CGOND(1:,M)                              &
&                             + TSIHKD(INDEP,M)*THDD(:,K    ) + THDC(:,K    )) &
&                  - MIN(0. , DRGM*SM*CGOND(1:,M)                              &
&                             + TSIHKD(INDEP,M)*THDD(:,KM(K)) + THDC(:,KM(K))) &
&                  + MAX(0. , (SIDC(:,K,M) + SIDC(:,K,MP(M)))*DFP)             &
&                  - MIN(0. , (SIDC(:,K,M) + SIDC(:,K,MM(M)))*DFM)
       END DO FRE
   END DO DIR

END SUBROUTINE C_SPHER_SHALLOW_CURR

! ---------------------------------------------------------------------------- !

END SUBROUTINE CHECK_CFL

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE GRADIENT (FIELD, D_LAT, D_LON)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   GRADIENT - CALCULATES GRADIENT.                                            !
!                                                                              !
!     K.P. HUBBERT              AUGUST   1988                                  !
!     H. GUNTHER    ECMWF/GKSS  DECEMBER 1990  MODIFIED FOR CYCLE_4.           !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO CALCULATE THE GRADIENT VECTOR FIELD FROM INPUT FIELD.               !
!                                                                              !
!     METHOD.                                                                  !
!     ------                                                                   !
!                                                                              !
!       CENTRAL DIFFERENCING.                                                  !
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

REAL,    INTENT(IN)  :: FIELD(:)  !! INPUT FIELD.
REAL,    INTENT(OUT) :: D_LAT(:)  !! LATITUDE  DERIVATIVE.
REAL,    INTENT(OUT) :: D_LON(:)  !! LONGITUDE DERIVATIVE.


! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. NORTH SOUTH GRADIENTS.                                                !
!        ----------------------                                                !

D_LAT = 0.0
WHERE (KLAT(:,1).GT.0 .AND. KLAT(:,2).GT.0)                                    &
&             D_LAT = (FIELD(KLAT(:,2))-FIELD(KLAT(:,1)))/(2.*DELPHI)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. EAST WEST GRADIENTS.                                                  !
!        --------------------                                                  !

D_LON = 0.0
WHERE (KLON(:,2).GT.0 .AND. KLON(:,1).GT.0)                                    &
&             D_LON = (FIELD(KLON(:,2))-FIELD( KLON(:,1)))/(2.*DELLAM)

END SUBROUTINE GRADIENT

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PREPARE_PROPAGATION

! ---------------------------------------------------------------------------- !
!                                                                              !
!   PREPARE_PROPAGATION - PREPARES DOT TERMS FROM DEPTH AND CURRENT GRADIENT.  !
!                                                                              !
!     H. GUNTHER   GKSS/ECMWF   17/02/91                                       !
!     H. GUNTHER   GKSS         DECEMBER 2001    FT90                          !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTATION OF REFRACTION DOT TERMS FOR PROPAGATION.                   !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       THE DEPTH AND CURRENT GRADIENTS ARE COMPUTED.                          !
!       DEPENDING OF THE MODEL OPTIONS THE DEPTH AND CURRENT REFRACTION PART   !
!       FOR THETA DOT AND THE COMPLETE SIGMA DOT TERM ARE COMPUTED.            !
!       FOR A MULTIBLOCK VERSION ALL TERMS ARE WRITTEN TO MASS STORAGE (IU15). !
!       FOR A ONE BLOCK MODEL THE SIGMA DOT TERM IS WRITTEN ONLY THE OTHER     !
!       TERMS ARE STORED IN WAM_MODEL_MODULE.                                  !
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

INTEGER :: M, K
REAL    :: SD, CD, SS, SC, CC, DELTH0
REAL    :: TEMP(1:NSEA)

REAL    :: DDPHI(1:NSEA), DDLAM(1:NSEA)
REAL    :: DUPHI(1:NSEA), DULAM(1:NSEA), DVPHI(1:NSEA), DVLAM(1:NSEA)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     0. NUMBER OF PROPAGATION TIME STEPS PER CALL.                            !
!        ------------------------------------------                            !

IF (ONE_POINT) THEN
   IDELPRO = IDELT
END IF

NADV = MAX(IDELWI,IDELPRO,IDELT)
IF (MOD(NADV,IDELPRO).NE.0 .OR. MOD(NADV,IDELWI).NE.0 .OR.                     &
&                                            MOD(NADV,IDELT).NE.0) THEN
   WRITE(IU06,*) '*************************************************'
   WRITE(IU06,*) '*                                               *'
   WRITE(IU06,*) '*     FATAL ERROR IN SUB. PREPARE_PROPAGATION   *'
   WRITE(IU06,*) '*     =======================================   *'
   WRITE(IU06,*) '*                                               *'
   WRITE(IU06,*) '* WIND INPUT, SOURCE FUNCTION AND PROPAGATION   *'
   WRITE(IU06,*) '* TIMESTEPS DO NOT SYNCRONIZE AT THERE MAXIMUM. *'
   WRITE(IU06,*) '* WIND INPUT TIMESTEP      : ', IDELWI
   WRITE(IU06,*) '* SOURCE FUNCTION TIMESTEP : ', IDELT
   WRITE(IU06,*) '* PROPAGATION TIMESTEP     : ', IDELPRO
   WRITE(IU06,*) '*                                               *'
   WRITE(IU06,*) '*        PROGRAM ABORTS  PROGRAM ABORTS         *'
   WRITE(IU06,*) '*                                               *'
   WRITE(IU06,*) '*************************************************'
   CALL ABORT1
END IF
IF (MOD(IDELRES,NADV).NE.0) THEN
   WRITE(IU06,*) '*************************************************'
   WRITE(IU06,*) '*                                               *'
   WRITE(IU06,*) '*     FATAL ERROR IN SUB. PREPARE_PROPAGATION   *'
   WRITE(IU06,*) '*     =======================================   *'
   WRITE(IU06,*) '*                                               *'
   WRITE(IU06,*) '* NEW OUTPUT FILES ARE REQUESTED EVERY          *'
   WRITE(IU06,*) '*        IDELRES = ',IDELRES,' SECONDS'
   WRITE(IU06,*) '* IDELRES MUST BE MULTIPLE OF THE MAXIMUM OF    *'
   WRITE(IU06,*) '* THE WIND INPUT TIMESTEP   IDELWI = ', IDELWI
   WRITE(IU06,*) '* THE PROPAGATION TIMESTEP IDELPRO = ', IDELPRO
   WRITE(IU06,*) '* THE SOURCE TIMESTEP        IDELT = ', IDELT
   WRITE(IU06,*) '*                                               *'
   WRITE(IU06,*) '*        PROGRAM ABORTS  PROGRAM ABORTS         *'
   WRITE(IU06,*) '*                                               *'
   WRITE(IU06,*) '*************************************************'
   CALL ABORT1
END IF

NADV = MAX(NADV/IDELPRO,1)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. NEIGHBOUR INDEX FOR FREQUENCY AND DIRECTION.                          !
!        --------------------------------------------                          !

IF (.NOT.ALLOCATED(MP)) THEN
   ALLOCATE(MP(1:ML))
   MP = (/(MIN(M,ML),M=2,ML+1)/)
END IF
IF (.NOT.ALLOCATED(MM)) THEN
   ALLOCATE(MM(1:ML))
   MM = (/(MAX(M,1),M=0,ML-1)/)
END IF
IF (.NOT.ALLOCATED(KP)) THEN
   ALLOCATE(KP(1:KL))
   KP = CSHIFT((/(K,K=1,KL)/),1)
END IF
IF (.NOT.ALLOCATED(KM)) THEN
   ALLOCATE(KM(1:KL))
   KM = CSHIFT(KP,-2)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. SHALLOW WATER TABLE INDICES (IF SHALLOW WATER).                       !
!        ----------------------------                                          !

IF (ISHALLO.NE.1) THEN
   IF (ALLOCATED(INDEP)) DEALLOCATE (INDEP)
   ALLOCATE (INDEP(1:NSEA))
   INDEP = 0
   INDEP = NINT(LOG(DEPTH/DEPTHA)/LOG(DEPTHD)+1.)
   INDEP = MAX(INDEP,1)
   INDEP = MIN(INDEP,NDEPTH)
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. GROUP VELOCITIES (IF SHALLOW WATER).                                  !
!         -----------------------------------                                  !

IF (ISHALLO.NE.1) THEN
   IF (.NOT.ALLOCATED(CGOND)) ALLOCATE(CGOND(0:NSEA,1:ML))
   CGOND(0,:) = TCGOND(NDEPTH,:)
   CGOND(1:,:) = TCGOND(INDEP,:)
END IF

IF (ONE_POINT) RETURN

! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. COSINE OF LATITUDE FACTORS (IF SPHERICAL GRID).                       !
!        -----------------------------------------------                       !

IF (ICASE.EQ.1) THEN
   IF (.NOT.ALLOCATED(DCO)) ALLOCATE(DCO(1:NSEA))
   IF (.NOT.ALLOCATED(DP1)) ALLOCATE(DP1(1:NSEA))
   IF (.NOT.ALLOCATED(DP2)) ALLOCATE(DP2(1:NSEA))

   DCO = 1./COSPH(KXLT)       !! COSINE OF LATITUDE.

   DP1 = MERGE(1., DCO/DCO(KLAT(:,1)), KLAT(:,1).EQ.0) !! COS PHI FACTOR SOUTH.
   DP2 = MERGE(1., DCO/DCO(KLAT(:,2)), KLAT(:,2).EQ.0) !! COS PHI FACTOR NORTH.

END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. CHECK REFRACTION AND IF REFRECTION IS OFF DO CFL CHECK AND RETURN.    !
!        ------------------------------------------------------------------    !


IF (IREFRA.EQ.2 .AND. (.NOT.ALLOCATED(U) .OR. .NOT.ALLOCATED(V))) THEN
   WRITE(IU06,*) '*****************************************************'
   WRITE(IU06,*) '*                                                   *'
   WRITE(IU06,*) '*      FATAL ERROR IN SUB.PREPARE_PROPAGATION       *'
   WRITE(IU06,*) '*      ======================================       *'
   WRITE(IU06,*) '*                                                   *'
   WRITE(IU06,*) '* A RUN WITH CURRENT REFRACTION IS REQUESTED,       *'
   WRITE(IU06,*) '* BUT CURRENT FILEDS ARE NOT IN WAM_CURRENT_MODULE. *'
   WRITE(IU06,*) '* IS A CURRENT FILE NAME DEFINED IN THE USER INPUT? *'
   WRITE(IU06,*) '*                                                   *'
   WRITE(IU06,*) '*         PROGRAM ABORTS   PROGRAM ABORTS           *'
   WRITE(IU06,*) '*                                                   *'
   WRITE(IU06,*) '*****************************************************'
   CALL ABORT1
END IF

IF (IREFRA.EQ.0) THEN
   CALL CHECK_CFL
   RETURN
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. ALLOCATE ARRAYS FOR  DOT TERMS.                                       !
!        -------------------------------                                       !

IF (.NOT.ALLOCATED(THDD) ) ALLOCATE (THDD(1:NSEA,1:KL))
IF (IREFRA.EQ.2) THEN
   IF (.NOT.ALLOCATED(THDC) ) ALLOCATE (THDC(1:NSEA,1:KL))
   IF (ISHALLO.NE.1) THEN
      IF (.NOT.ALLOCATED(SIDC) ) ALLOCATE (SIDC(1:NSEA,1:KL,1:ML))
   ELSE
      IF (.NOT.ALLOCATED(SIDC) ) ALLOCATE (SIDC(1:NSEA,1:KL,ML:ML))
   END IF
END IF

DELTH0 = 0.5*REAL(IDELPRO)/DELTH

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. COMPUTE DEPTH AND CURRENT GRADIENTS.                                  !
!        ------------------------------------                                  !

IF (ISHALLO.NE.1) THEN
   CALL GRADIENT (DEPTH, DDPHI, DDLAM)
   IF (ICASE.EQ.1) DDLAM = DDLAM*DCO
END IF
IF (IREFRA.EQ.2) THEN
   CALL GRADIENT (U, DUPHI, DULAM)
   IF (ICASE.EQ.1) DULAM = DULAM*DCO
   CALL GRADIENT (V, DVPHI, DVLAM)
   IF (ICASE.EQ.1) DVLAM = DVLAM*DCO
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. DEPTH OF THETA DOT.                                                   !
!        -------------------                                                   !

IF (ISHALLO.NE.1) THEN
   DO  K = 1,KL
      THDD(:,K) = (SINTH(K)+SINTH(KP(K)))*DDPHI - (COSTH(K)+COSTH(KP(K)))*DDLAM
   END DO
   THDD = THDD * DELTH0
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     9. DEPTH PART OF SIGMA DOT.                                              !
!        ------------------------                                              !

IF (ISHALLO.NE.1 .AND. IREFRA.EQ.2) TEMP = V*DDPHI + U*DDLAM

! ---------------------------------------------------------------------------- !
!                                                                              !
!    10. CURRENT PART OF SIGMA DOT AND THETA DOT.                              !
!        -----------------------------------------                             !


IF (IREFRA.EQ.2) THEN

!    10.1 CURRENT PART OF THETA DOT.                                           !
!         --------------------------                                           !

   DO  K = 1,KL
      SS  = SINTH(K)**2 + SINTH(KP(K))**2
      SC  = SINTH(K)*COSTH(K) + SINTH(KP(K))*COSTH(KP(K))
      CC  = COSTH(K)**2 +COSTH(KP(K))**2
      THDC(:,K)    =   SS*DUPHI + SC*DVPHI - SC*DULAM - CC*DVLAM
   END DO
   THDC = THDC * DELTH0

!    10.2 CURRENT PART OF SIGMA DOT.                                           !
!         --------------------------                                           !

   DO  K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)
      SS  = SD**2
      SC  = SD*CD
      CC  = CD**2
      SIDC(:,K,ML) = - SC*DUPHI - CC*DVPHI - SS*DULAM - SC*DVLAM
   END DO

!   10.3 ADD SHALLOW WATER TO CURRENT PART OF SIGMA DOT.                       !
!        -----------------------------------------------                       !

   IF (ISHALLO.NE.1) THEN
      DO M = 1,ML
         DO  K = 1,KL
            SIDC(:,K,M) = (SIDC(:,K,ML) * TCGOND(INDEP,M)                      &
&                       + TEMP * TSIHKD(INDEP,M) ) * TFAK(INDEP,M)
         END DO
      END DO
   END IF

END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!    11. CHECK CFL.                                                            !
!        ----------                                                            !

CALL CHECK_CFL

END SUBROUTINE PREPARE_PROPAGATION

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PRINT_PROPAGATION_STATUS

WRITE(IU06,*) '  '
WRITE(IU06,*) ' ------------------------------------------------- '
WRITE(IU06,*) '              PROPAGATION MODULE STATUS:'
WRITE(IU06,*) ' ------------------------------------------------- '
WRITE(IU06,*) '  '
IF (NADV.LE.0) THEN
   WRITE(IU06,*) '              PROPAGATION MODULE NOT INITIALIZED'
ELSE
   IF (ONE_POINT) THEN
      WRITE(IU06,*) ' '
      WRITE(IU06,*) '  ONE POINT MODEL: PROPAGATION IS NOT INITIALIZED'
      WRITE(IU06,*) '  NUMBER OF SOURCE TIME STEPS IN ONE CALL ',              &
&                                     'OF SUB WAVEMDL IS ' , 'NADV = ', NADV
   ELSE
      WRITE(IU06,*) '  NUMBER OF PROPAGATION STEPS IN ONE CALL ',              &
&                                     'OF SUB WAVEMDL IS ' , 'NADV = ', NADV
      WRITE(IU06,*) ' '
      WRITE(IU06,*) '  MAX. CFL NUMBER IS CFLMAX = ', CFLMAX
      WRITE(IU06,*) '     POINT     = ', MAXPOINT(1)
      WRITE(IU06,*) '     DIRECTION = ', TH(MAXPOINT(2))*DEG
      WRITE(IU06,*) '     FREQUENCY = ', FR(MAXPOINT(3))
      WRITE(IU06,*) '  '
   END IF
END IF

END SUBROUTINE PRINT_PROPAGATION_STATUS

! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !

SUBROUTINE PROPAGS (F3)

! ---------------------------------------------------------------------------- !
!                                                                              !
!   PROPAGS - COMPUTATION OF A PROPAGATION TIME STEP.                          !
!                                                                              !
!     S.D. HASSELMANN.                                                         !
!     OPTIMIZED BY: L. ZAMBRESKY AND H. GUENTHER                               !
!                                                                              !
!     MODIFIED BY   H. GUNTHER   01/06/90    -   LAND POINTS ARE TAKEN         !
!                             OUT OF BLOCKS AND REFRACTION INTEGRATION         !
!                             CORRECTED FOR N-S AND S-N PROPAGATION.           !
!                                                                              !
!     K.P. HUBBERT                /07/89    -   DEPTH AND CURRENT              !
!     S. HASSELMANN   MPIFM       /04/90        REFRACTION SHALLOW             !
!                                                                              !
!     H. GUNTHER   GKSS/ECMWF   17/01/91    -   MODIFIED FOR CYCLE_4           !
!                                                                              !
!     H. GUENTHER   GKSS  FEBRUARY 2002       FT 90                            !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       COMPUTATION OF A PROPAGATION TIME STEP.                                !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       FIRST ORDER FLUX SCHEME.                                               !
!                                                                              !
!     INTERNAL SUBROUTINES FOR PROPAGATION.                                    !
!     -------------------------------------                                    !
!                                                                              !
!         SPHERICAL GRID.                                                      !
!                                                                              !
!       *P_SPHER_DEEP*         DEEP WATER WITHOUT CURRENT REFRACTION.          !
!       *P_SPHER_SHALLOW*      SHALLOW WATER WITHOUT CURRENT REFRACTION.       !
!       *P_SPHER_DEEP_CURR*    DEEP WATER WITH CURRENT REFRACTION.             !
!       *P_SPHER_SHALLOW_CURR* SHALLOW WATER WITH DEPTH AND CURRENT REFRACTION.!
!                                                                              !
!         CARTESIAN GRID.                                                      !
!                                                                              !
!       *P_CART_DEEP*          DEEP WATER WITHOUT CURRENT REFRACTION.          !
!       *P_CART_SHALLOW*       SHALLOW WATER WITHOUT CURRENT REFRACTION.       !
!       *P_CART_DEEP_CUR*      DEEP WATER WITH CURRENT REFRACTION.             !
!       *P_CART_SHALLOW_CUR*   SHALLOW WATER WITH DEPTH AND CURRENT REFRACTION.!
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

REAL,    INTENT(INOUT)   :: F3(1:NSEA,KL,ML)     !! SPECTRUM AT TIME T+DELT.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER :: IS, IT, IC, ID, K, M

REAL    :: DELPH0, DELLA0, DELTH0, DELFR0
REAL    :: SD, CD, SDA, CDA, DTT, DNO, DEA
REAL    :: DFP, DFM, CGS, CGC, SM, SP

REAL   :: DTC(1:NSEA)      !! WEIGHT OF CENTER.
REAL   :: DPN(1:NSEA)      !! WEIGHT OF NORTH POINT.
REAL   :: DPS(1:NSEA)      !! WEIGHT OF SOUTH POINT.
REAL   :: DLE(1:NSEA)      !! WEIGHT OF EAST POINT.
REAL   :: DLW(1:NSEA)      !! WEIGHT OF WEST POINT.
REAL   :: DTP(1:NSEA)      !! WEIGHT OF DIRECTION +1.
REAL   :: DTM(1:NSEA)      !! WEIGHT OF DIRECTION -1.
REAL   :: DOP(1:NSEA)      !! WEIGHT OF FREQUENCY +1.
REAL   :: DOM(1:NSEA)      !! WEIGHT OF FREQUENCY -1.

REAL   :: DRGP(1:NSEA)     !! DIR. PART OF THETA DOT FROM GRID DIRECTION +1.
REAL   :: DRGM(1:NSEA)     !! DIR. PART OF THETA DOT FROM GRID DIRECTION -1.

REAL   :: WOK(0:NSEA)      !! WORK ARRAY.

REAL :: F1(0:NSEA,1:KL,1:ML)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     0. INITIAL.                                                              !
!        --------                                                              !

F1(0 ,:,:) = 0.            !! SPECTRUM AT LAND TO ZERO.
F1(1:,:,:) = F3            !! COPY INPUT SPECTRA

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1.0 SELECT CASES.                                                        !
!         -------------                                                        !

IF (ICASE.EQ.1) THEN

!     1.1 PROPAGATION ON SPHERICAL GRID.                                       !
!         ------------------------------                                       !

   IF (IREFRA.NE.2) THEN
      IF (ISHALLO.EQ.1) THEN
         CALL P_SPHER_DEEP     !! DEEP WATER WITHOUT CURRENT REFRACTION.
      ELSE
         CALL P_SPHER_SHALLOW  !! SHALLOW WATER WITHOUT CURRENT REFRACTION.
      END IF
   ELSE
      IF (ISHALLO.EQ.1) THEN
         CALL P_SPHER_DEEP_CURR    !! DEEP WATER WITH CURRENT REFRACTION.
      ELSE
         CALL P_SPHER_SHALLOW_CURR !! SHALLOW WATER WITH DEPTH AND CURRENT REF.
      END IF
   END IF

ELSE

!     1.2 PROPAGATION ON CARTESIAN GRID.                                       !
!         ------------------------------                                       !

   IF (IREFRA.NE.2) THEN
      IF (ISHALLO.EQ.1) THEN
         CALL P_CART_DEEP         !! DEEP WATER WITHOUT CURRENT REFRACTION.
      ELSE
         CALL P_CART_SHALLOW      !! SHALLOW WATER WITHOUT CURRENT REFRACTION.
      END IF

   ELSE
      IF (ISHALLO.EQ.1) THEN
         CALL P_CART_DEEP_CUR     !! DEEP WATER WITH CURRENT REFRACTION.
      ELSE
         CALL P_CART_SHALLOW_CUR  !! SHALLOW WATER WITH DEPTH AND CURRENT REF.
      END IF

   END IF
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2.0 RETURN.                                                              !
!         -------                                                              !

RETURN

! ---------------------------------------------------------------------------- !

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.0 INTERNAL SUBROUTINES.                                                !
!         ---------------------                                                !

CONTAINS

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.1 PROPAGATION FOR CARTESIAN GRID WITHOUT CURRENT REFRACTION (DEEP).    !
!         -----------------------------------------------------------------    !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE P_CART_DEEP

   DELLA0 = REAL(IDELPRO)/DELLAM
   DELPH0 = REAL(IDELPRO)/DELPHI

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)

      IF (SD.LT.0) THEN                  !! INDEX FOR ADJOINING LONGITUDE.
         IS = 2
      ELSE
         IS = 1
      END IF
      IF (CD.LT.0) THEN                  !! INDEX FOR ADJOINING LATITUDE.
         IC = 2
      ELSE
         IC = 1
      END IF

      SD = ABS(SD*DELLA0)
      CD = ABS(CD*DELPH0)

      FRE: DO M = 1,ML                   !! LOOP OVER FREQUENCIES.
         DNO = CD*GOM(M)
         DEA = SD*GOM(M)
         DTT = 1. - DEA - DNO
         F3(:,K,M) = DTT*F1(1:,K,M )                                          &
&                  + DNO*F1(KLAT(:,IC),K  ,M) + DEA*F1(KLON(:,IS),K  ,M)

      END DO FRE
   END DO DIR

END SUBROUTINE P_CART_DEEP

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.2 PROPAGATION FOR CARTESIAN GRID WITHOUT CURRENT REFRACTION (SHALLOW). !
!         -------------------------------------------------------------------- !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE P_CART_SHALLOW

   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM
   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)

      IF (SD.LT.0) THEN                  !! INDEX FOR ADJOINING LONGITUDE.
         IS = 2
         IT = 1
      ELSE
         IS = 1
         IT = 2
      END IF
      IF (CD.LT.0) THEN                  !! INDEX FOR ADJOINING LATITUDE.
         IC = 2
         ID = 1
      ELSE
         IC = 1
         ID = 2
      END IF

      SD = ABS(SD*DELLA0)
      CD = ABS(CD*DELPH0)

      FRE: DO M = 1,ML
         DLE  = SD*(CGOND(KLON(:,IS),M) + CGOND(1:,M))
         DPN  = CD*(CGOND(KLAT(:,IC),M) + CGOND(1:,M))

         DTC = 1. - SD*(CGOND(KLON(:,IT),M) + CGOND(1:,M))                     &
&                 - CD*(CGOND(KLAT(:,ID),M) + CGOND(1:,M))

         IF (IREFRA.EQ.1) THEN
            WOK(1:) =  TSIHKD(INDEP,M)
            DTP = WOK(1:)*THDD(:,K  )
            DTM = WOK(1:)*THDD(:,KM(K))
            DTC = DTC - MAX(0. , DTP) + MIN (0. , DTM)
            DTP = - MIN (0. ,  DTP)
            DTM =   MAX (0. , DTM)
         END IF

         F3(:,K,M) = DTC*F1(1:,K,M )                                           &
&                  + DPN*F1(KLAT(:,IC),K  ,M) + DLE*F1(KLON(:,IS),K  ,M)
         IF (IREFRA.EQ.1) THEN
            F3(:,K,M) = F3(:,K,M ) + DTP*F1(1:,KP(K),M) + DTM*F1(1:,KM(K),M)
         END IF
      END DO FRE
   END DO DIR

END SUBROUTINE P_CART_SHALLOW

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.3 PROPAGATION FOR CARTESIAN GRID WITH CURRENT REFRACTION (DEEP).       !
!         --------------------------------------------------------------       !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE P_CART_DEEP_CUR

   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM
   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELFR0 = 0.5*REAL(IDELPRO)*2.1/0.2

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)

      FRE: DO M = 1,ML
         CGS = GOM(M)*SD                !! GROUP VELOCITIES.
         CGC = GOM(M)*CD

         WOK(0) = CGS*DELLA0
         WOK(1:) = (U + CGS)*DELLA0
         DLW = WOK(1:) + WOK(KLON(:,1))
         DLE = WOK(1:) + WOK(KLON(:,2))
         DTC = 1. - MAX(0. , DLE) + MIN(0. , DLW)
         DLE = -MIN(0. , DLE)
         DLW =  MAX(0. , DLW)

         WOK(0) = CGC*DELPH0
         WOK(1:) = (V + CGC)*DELPH0
         DPS = WOK(1:) + WOK(KLAT(:,1))
         DPN = WOK(1:) + WOK(KLAT(:,2))
         DTC = DTC - MAX(0. , DPN) + MIN(0. , DPS)
         DPN = -MIN(0. , DPN)
         DPS =  MAX(0. , DPS)

         DTP = -MIN(0. , THDC(:,K  ))
         DTM =  MAX(0. , THDC(:,KM(K)))
         DTC = DTC - MAX(0. , THDC(:,K  )) + MIN(0. , THDC(:,KM(K)))

         DOM = SIDC(:,K,ML) * DELFR0
         DTC = DTC - ABS(DOM)
         DOP = -MIN(0. , DOM)/1.1
         DOM =  MAX(0. , DOM)*1.1

         F3(:,K,M) = DTC*F1(1:,K,M )                                           &
&              + DPN*F1(KLAT(:,2),K    ,M    ) + DPS*F1(KLAT(:,1),K    ,M    ) &
&              + DLE*F1(KLON(:,2),K    ,M    ) + DLW*F1(KLON(:,1),K    ,M    ) &
&              + DTP*F1(1:       ,KP(K),M    ) + DTM*F1(1:       ,KM(K),M    ) &
&              + DOP*F1(1:       ,K    ,MP(M)) + DOM*F1(1:       ,K    ,MM(M))
      END DO FRE
   END DO DIR

END SUBROUTINE P_CART_DEEP_CUR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.4 PROPAGATION FOR CARTESIAN GRID WITH CURRENT REFRACTION (SHALLOW).    !
!         -----------------------------------------------------------------    !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE P_CART_SHALLOW_CUR

   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM
   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELFR0 = 0.5*REAL(IDELPRO)/(0.1*ZPI)

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)

      FRE: DO M = 1,ML
         DFP = DELFR0/FR(M)
         DFM = DELFR0/FR(MM(M))

         WOK(0) = SD*CGOND(0,M)*DELLA0
         WOK(1:) = (U + SD*CGOND(:,M)) * DELLA0
         DLW = WOK(1:) + WOK(KLON(:,1))
         DLE = WOK(1:) + WOK(KLON(:,2))
         DTC = 1. - MAX(0. , DLE) + MIN(0. , DLW)
         DLE = -MIN(0. , DLE)
         DLW =  MAX(0. , DLW)

         WOK(0) = CD*CGOND(0,M)*DELPH0
         WOK(1:) = (V + CD*CGOND(:,M)) * DELPH0
         DPS = WOK(1:) + WOK(KLAT(:,1))
         DPN = WOK(1:) + WOK(KLAT(:,2))
         DTC = DTC - MAX(0. , DPN) + MIN(0. , DPS)
         DPN = -MIN(0. , DPN)
         DPS =  MAX(0. , DPS)

         WOK(1:) =  TSIHKD(INDEP,M)
         DTP = WOK(1:)*THDD(:,K  ) + THDC(:,K  )
         DTM = WOK(1:)*THDD(:,KM(K)) + THDC(:,KM(K))
         DTC = DTC - MAX(0. , DTP) + MIN(0. , DTM)
         DTP = -MIN(0. , DTP)
         DTM =  MAX(0. , DTM)

         DOP = (SIDC(:,K,M) + SIDC(:,K,MP(M)))*DFP
         DOM = (SIDC(:,K,M) + SIDC(:,K,MM(M)))*DFM
         DTC = DTC - MAX(0. , DOP) + MIN(0. , DOM)
         DOP = -MIN(0. , DOP)/1.1
         DOM =  MAX(0. , DOM)*1.1

         F3(:,K,M) = DTC*F1(1:,K,M )                                           &
&              + DPN*F1(KLAT(:,2),K    ,M    ) + DPS*F1(KLAT(:,1),K    ,M    ) &
&              + DLE*F1(KLON(:,2),K    ,M    ) + DLW*F1(KLON(:,1),K    ,M    ) &
&              + DTP*F1(1:       ,KP(K),M    ) + DTM*F1(1:       ,KM(K),M    ) &
&              + DOP*F1(1:       ,K    ,MP(M)) + DOM*F1(1:       ,K    ,MM(M))

      END DO FRE
   END DO DIR

END SUBROUTINE P_CART_SHALLOW_CUR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.5 PROPAGATION FOR SPHERICAL GRID WITHOUT CURRENT REFRACTION (DEEP).    !
!         -----------------------------------------------------------------    !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE P_SPHER_DEEP

   DELLA0 = REAL(IDELPRO)/DELLAM
   DELTH0 = 0.5*REAL(IDELPRO)/DELTR
   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)
      SDA = ABS(SD*DELLA0)
      CDA = ABS(CD*DELPH0)

      DLE = DCO*SDA                         !! LAT / LONG WEIGHTS.
      IF (SD.LT.0) THEN
         IS = 2
      ELSE
         IS = 1
      END IF
      IF (CD.LT.0) THEN
         IC = 2
         DTC = DLE + CDA*(DP1 + 1.)
         DPN = CDA*(DP2 + 1.)
      ELSE
         IC = 1
         DTC = DLE + CDA*(DP2 + 1.)
         DPN = CDA*(DP1 + 1.)
      END IF

      SP  = DELTH0*(SINTH(K)+SINTH(KP(K)))   !! GRID REFRACTION WEIGHTS
      SM  = DELTH0*(SINTH(K)+SINTH(KM(K)))
      DTM = SINPH(KXLT)*DCO
      DTP = DTM*SP
      DTM = DTM*SM
      DTC = DTC + MAX(0. , DTP) - MIN(0. , DTM)
      DTP = -MIN(0. , DTP)
      DTM =  MAX(0. , DTM)

      FRE: DO M = 1,ML
         F3(:,K,M) = (1. - DTC*GOM(M))*F1(1:,K,M)                              &
&                  + GOM(M) * (DPN * F1(KLAT(:,IC),K    ,M)                    &
&                            + DLE * F1(KLON(:,IS),K    ,M)                    &
&                            + DTP * F1(1:        ,KP(K),M)                    &
&                            + DTM * F1(1:        ,KM(K),M))
      END DO FRE
   END DO DIR

END SUBROUTINE P_SPHER_DEEP

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.6 PROPAGATION FOR SPHERICAL GRID WITHOUT CURRENT REFRACTION (SHALLOW). !
!         -------------------------------------------------------------------- !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE P_SPHER_SHALLOW

   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM
   DELTH0 = 0.5*REAL(IDELPRO)/DELTR
   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)
      SDA = ABS(SD*DELLA0)
      CDA = ABS(CD*DELPH0)

      SP  = DELTH0*(SINTH(K)+SINTH(KP(K)))   !! PRE_COMPUTE GRID REFRACTION.
      SM  = DELTH0*(SINTH(K)+SINTH(KM(K)))
      DRGM = SINPH(KXLT)*DCO
      DRGP = DRGM*SP
      DRGM = DRGM*SM

      IF (SD.LT.0) THEN                    !! INDEX FOR ADJOINING POINTS.
         IS = 2
         IT = 1
      ELSE
         IS = 1
         IT = 2
      END IF
      IF (CD.LT.0) THEN
         IC = 2
         ID = 1
      ELSE
         IC = 1
         ID = 2
      END IF

      FRE: DO M = 1,ML
                                           !! LAT / LONG WEIGHTS.
         DTC = 1. - DCO*SDA*(CGOND(KLON(:,IT),M) + CGOND(1:,M))
         DLE =      DCO*SDA*(CGOND(KLON(:,IS),M) + CGOND(1:,M))

         DTC = DTC - CDA*(CGOND(KLAT(:,ID),M)*DP2 + CGOND(1:,M))
         DPN =       CDA*(CGOND(KLAT(:,IC),M)*DP1 + CGOND(1:,M))

         DTP = DRGP*CGOND(1:,M)           !! REFRACTION WEIGHTS
         DTM = DRGM*CGOND(1:,M)
         IF (IREFRA.NE.0) THEN            !! ADD DEPTH REFRACTION
            WOK(1:) =  TSIHKD(INDEP,M)
            DTP = DTP + WOK(1:)*THDD(:,K  )
            DTM = DTM + WOK(1:)*THDD(:,KM(K))
         END IF
         DTC = DTC - MAX(0. , DTP) + MIN(0. , DTM)
         DTP = -MIN(0. , DTP)
         DTM =  MAX(0. , DTM)

         F3(:,K,M) = DTC * F1(1:,K,M )                                         &
&                  + DPN * F1(KLAT(:,IC),K  ,M) + DLE * F1(KLON(:,IS),K  ,M)   &
&                  + DTP * F1(1:        ,KP(K),M) + DTM * F1(1:        ,KM(K),M)
      END DO FRE
   END DO DIR

END SUBROUTINE P_SPHER_SHALLOW

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.7 PROPAGATION FOR SPHERICAL GRID WITH CURRENT REFRACTION (DEEP).       !
!         --------------------------------------------------------------       !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE P_SPHER_DEEP_CURR

   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM
   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELTH0 = 0.5*REAL(IDELPRO)/DELTR
   DELFR0 = 0.5*REAL(IDELPRO)*2.1/0.2

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)

      SP = DELTH0*(SINTH(K)+SINTH(KP(K)))     !! PRE-COMPUTE GRID REFRACTION.
      SM = DELTH0*(SINTH(K)+SINTH(KM(K)))
      DRGM = SINPH(KXLT)*DCO
      DRGP = DRGM*SP
      DRGM = DRGM*SM

      FRE: DO M = 1,ML
         CGS = GOM(M)*SD                       !! GROUP VELOCITIES.
         CGC = GOM(M)*CD

         WOK(0) = CGS*DELLA0
         WOK(1:) = (U + CGS)*DELLA0 * DCO
         DLW = WOK(1:) + WOK(KLON(:,1))
         DLE = WOK(1:) + WOK(KLON(:,2))
         DTC = 1. - MAX(0. , DLE) + MIN(0. , DLW)
         DLE = -MIN(0. , DLE)
         DLW =  MAX(0. , DLW)

         WOK(0) = CGC*DELPH0
         WOK(1:) = (V + CGC)*DELPH0
         DPS = WOK(1:) + WOK(KLAT(:,1))*DP1
         DPN = WOK(1:) + WOK(KLAT(:,2))*DP2
         DTC = DTC(1:) - MAX(0. , DPN) + MIN(0. , DPS)
         DPN = -MIN(0. , DPN)
         DPS =  MAX(0. , DPS)

         DTP = DRGP*GOM(M) + THDC(:,K  )
         DTM = DRGM*GOM(M) + THDC(:,KM(K))
         DTC = DTC - MAX(0. , DTP) + MIN(0. , DTM)
         DTP = -MIN(0. , DTP)
         DTM =  MAX(0. , DTM)

         DOM =  SIDC(:,K,ML) * DELFR0
         DTC =  DTC - ABS(DOM)
         DOP = -MIN(0.,DOM)/1.1
         DOM =  MAX(0.,DOM)*1.1

         F3(:,K,M) = DTC*F1(1:,K,M )                                           &
&              + DPN*F1(KLAT(:,2),K    ,M    ) + DPS*F1(KLAT(:,1),K    ,M    ) &
&              + DLE*F1(KLON(:,2),K    ,M    ) + DLW*F1(KLON(:,1),K    ,M    ) &
&              + DTP*F1(1:       ,KP(K),M    ) + DTM*F1(1:       ,KM(K),M    ) &
&              + DOP*F1(1:       ,K    ,MP(M)) + DOM*F1(1:       ,K    ,MM(M))
      END DO FRE
   END DO DIR

END SUBROUTINE P_SPHER_DEEP_CURR

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3.8 PROPAGATION FOR SPHERICAL GRID WITH CURRENT REFRACTION (SHALLOW).    !
!         -----------------------------------------------------------------    !
!                                                                              !
! ---------------------------------------------------------------------------- !

SUBROUTINE P_SPHER_SHALLOW_CURR

   DELLA0 = 0.5*REAL(IDELPRO)/DELLAM
   DELPH0 = 0.5*REAL(IDELPRO)/DELPHI
   DELTH0 = 0.5*REAL(IDELPRO)/DELTR
   DELFR0 = 0.5*REAL(IDELPRO)/(0.1*ZPI)

   DIR: DO K = 1,KL
      SD = SINTH(K)
      CD = COSTH(K)
      SP = DELTH0*(SINTH(K)+SINTH(KP(K)))      !! GRID REFRACTION.
      SM = DELTH0*(SINTH(K)+SINTH(KM(K)))
      DRGM = SINPH(KXLT)*DCO
      DRGP = DRGM*SP
      DRGM = DRGM*SM

      FRE: DO M = 1,ML
         DFP = DELFR0/FR(M)
         DFM = DELFR0/FR(MM(M))

         WOK(0) = SD*CGOND(0,M)*DELLA0
         WOK(1:) = (U + SD*CGOND(1:,M)) * DELLA0*DCO
         DLW = WOK(1:) + WOK(KLON(:,1))
         DLE = WOK(1:) + WOK(KLON(:,2))
         DTC = 1. - MAX(0. , DLE) + MIN(0. , DLW)
         DLE = -MIN(0. , DLE)
         DLW =  MAX(0. , DLW)

         WOK(0) = CD*CGOND(0,M)*DELPH0
         WOK(1:) = (V + CD*CGOND(1:,M)) * DELPH0
         DPS = WOK(1:) + WOK(KLAT(:,1))*DP1
         DPN = WOK(1:) + WOK(KLAT(:,2))*DP2
         DTC = DTC - MAX(0. , DPN) + MIN(0. , DPS)
         DPN = -MIN(0. , DPN)
         DPS =  MAX(0. , DPS)

         WOK(1:) =  TSIHKD(INDEP,M)
         DTP = DRGP*CGOND(1:,M) + WOK(1:)*THDD(:,K  ) + THDC(:,K  )
         DTM = DRGM*CGOND(1:,M) + WOK(1:)*THDD(:,KM(K)) + THDC(:,KM(K))
         DTC = DTC - MAX(0. , DTP) + MIN(0. , DTM)
         DTP = -MIN(0. , DTP)
         DTM =  MAX(0. , DTM)

         DOP = (SIDC(:,K,M) + SIDC(:,K,MP(M)))*DFP
         DOM = (SIDC(:,K,M) + SIDC(:,K,MM(M)))*DFM
         DTC =  DTC - MAX(0. , DOP) + MIN(0. , DOM)
         DOP = -MIN(0. , DOP)/1.1
         DOM =  MAX(0. , DOM)*1.1

         F3(:,K,M) = DTC*F1(1:,K,M )                                           &
&              + DPN*F1(KLAT(:,2),K    ,M    ) + DPS*F1(KLAT(:,1),K    ,M    ) &
&              + DLE*F1(KLON(:,2),K    ,M    ) + DLW*F1(KLON(:,1),K    ,M    ) &
&              + DTP*F1(1:       ,KP(K),M    ) + DTM*F1(1:       ,KM(K),M    ) &
&              + DOP*F1(1:       ,K    ,MP(M)) + DOM*F1(1:       ,K    ,MM(M))

       END DO FRE
   END DO DIR

END SUBROUTINE P_SPHER_SHALLOW_CURR

! ---------------------------------------------------------------------------- !

END SUBROUTINE PROPAGS

END MODULE WAM_PROPAGATION_MODULE
