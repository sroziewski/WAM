SUBROUTINE INGRID (IUNIT, IEOF)

! ---------------------------------------------------------------------------- !
!                                                                              !
!      INGRID - READS WAVE MODEL OUTPUT FILE OF INTEGRATED DATA                !
!                                                                              !
!     H. GUNTHER          ECMWF             DECEMBER 1989                      !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       POST PROCESSING ROUTINE FOR WAVE MODEL.                                !
!                                                                              !
!     INTERFACE.                                                               !
!     ----------                                                               !
!                                                                              !
!       *CALL INGRID (IU06, IUNIT, IDATE, ID1, ID2, NX, NY,                    !
!                     AMOWEP, AMOSOP, AMOEAP, AMONOP,                          !
!                     WAVEHT, WAVEDIR, WAVEMFR, WINDSPD,                       !
!                     WINDDIR, WAVEPFR, CDG, TAUWG, FFLAG, IEOF)*              !
!                                                                              !
!         *IU06*    - LOGICAL UNIT PRINTER OUTPUT.                             !
!         *IUNIT*   - LOGICAL UNIT FOR INPUT FROM WAVE MODEL.                  !
!         *IDATE*   - DATE TIME OF FIELDS READ IN (YYMMDDHHMM).                !
!         *ID1*     - FIRST DIMENSION OF ARRAYS.                               !
!         *ID2*     - SECOND DIMENSION OF ARRAYS.                              !
!         *NX*     - NUMBER OF GRID POINTS IN X (FIRST DIMENSION).             !
!         *NY*     - NUMBER OF GRID POINTS IN Y (SECOND DIMENSION).            !
!         *AMOWEP*  - MOST WESTERN LONGITUDE IN GRID (DEGREE).                 !
!         *AMOSOP*  - MOST SOUTHETN LATITUDE IN GRID (DEGREE).                 !
!         *AMOEAP*  - MOST EASTERN LONGITUDE IN GRID (DEGREE).                 !
!         *AMONOP*  - MOST NORTHERN LATITUDE IN GRID (DEGREE).                 !
!         *WAVEHT*  - MEAN WAVE HEIGHT (METRES).                               !
!         *WAVEDIR* - MEAN WAVE DIRECTION (DEGREE CLOCKWISE FROM NORTH).       !
!         *WAVEMFR* - MEAN FREQUENCY (HERTZ).                                  !
!         *WINDSPD* - FRICTION VELOCITY (METRE/SECOND).                        !
!         *WINDDIR* - WIND DIRECTION (DEGREE CLOCKWISE FROM NORTH).            !
!         *WAVEPFR* - WAVE PEAK FREQUENCY (HERTZ).                             !
!         *CDG*     - DRAG COEFFICIENT.                                        !
!         *TAUWG*   - NORMALISED WAVE STRESS (%).                              !
!         *FFLAG    - DATA FLAG.                                               !
!         *IEOF*    - END OF FILE INDICATOR.                                   !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       UNFORMATED READ.                                                       !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !
!                                                                              !
!       *ABORT*     - TERMINATES PROCESSING.                                   !
!                                                                              !
! ---------------------------------------------------------------------------- !

USE WAM_PRINT_MODULE, ONLY: NX, NY, AMOWEP, AMOSOP, AMOEAP, AMONOP,            &
&                           XDELLA, XDELLO,                                    &
&                           U10_GR, UDIR_GR, US_GR, CD_GR,                     &
&                           HS_GR, PPER_GR, MPER_GR, TM1_GR, TM2_GR, MDIR_GR,  &
&                           SPRE_GR, TAUW_GR,                                  &
&                           HS_SEA_GR, PPER_SEA_GR, MPER_SEA_GR, TM1_SEA_GR,   &
&                           TM2_SEA_GR, MDIR_SEA_GR, SPRE_SEA_GR,              &
&                           HS_SWELL_GR, PPER_SWELL_GR, MPER_SWELL_GR,         &
&                           TM1_SWELL_GR, TM2_SWELL_GR, MDIR_SWELL_GR,         &
&                           SPRE_SWELL_GR


USE WAM_OUTPUT_SET_UP_MODULE,ONLY: CDTINTT, PFLAG

IMPLICIT NONE

INTEGER, INTENT(IN)  :: IUNIT
LOGICAL, INTENT(OUT) :: IEOF

REAL    :: DNX, DNY

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. DATA HEADER FROM WAVE MODEL OUTPUT (SUB. OUTINT).                     !
!        -------------------------------------------------                     !
READ(IUNIT, END=3000) CDTINTT, DNX, DNY, AMOWEP, AMOSOP, AMOEAP, AMONOP
READ(IUNIT, END=3000) PFLAG(1:30)

NX = NINT(DNX)
NY = NINT(DNY)
XDELLO=(ABS(AMOWEP)+ABS(AMOEAP))/REAL(NX)
XDELLA=(ABS(AMOSOP)+ABS(AMONOP))/REAL(NY)
! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. DATA FROM WAVE MODEL OUTPUT (SUB OUTINT).                             !
!        ------------------------------------------                            !


IF (PFLAG(1)) THEN                           !! WIND SPEED U10 [M/S].
   IF (.NOT.ALLOCATED(U10_GR))  ALLOCATE(U10_GR(NX,NY))
   READ(IUNIT, END=3000) U10_GR
END IF
IF (PFLAG(2)) THEN                           !! WIND DIRECTION [DEG].
   IF (.NOT.ALLOCATED(UDIR_GR)) ALLOCATE(UDIR_GR(NX,NY))
      READ(IUNIT, END=3000) UDIR_GR
END IF
IF (PFLAG(3)) THEN                           !! FRICTION VELOCITY [M/S].
   IF (.NOT.ALLOCATED(US_GR  )) ALLOCATE(US_GR(NX,NY))
   READ(IUNIT, END=3000) US_GR
END IF
IF (PFLAG(4)) THEN                           !! DRAG COEFFICIENT.
   IF (.NOT.ALLOCATED(CD_GR  )) ALLOCATE(CD_GR(NX,NY))
   READ(IUNIT, END=3000) CD_GR
END IF

IF (PFLAG(7)) THEN                           !! WAVE HEIGHT [M].
   IF (.NOT.ALLOCATED(HS_GR  )) ALLOCATE(HS_GR(NX,NY))
   READ(IUNIT, END=3000) HS_GR
END IF
IF (PFLAG(8)) THEN                           !! PEAK PERIOD [S].
   IF (.NOT.ALLOCATED(PPER_GR)) ALLOCATE(PPER_GR(NX,NY))
   READ(IUNIT, END=3000) PPER_GR
END IF
IF (PFLAG(9)) THEN                           !! MEAN PERIOD [S].
   IF (.NOT.ALLOCATED(MPER_GR)) ALLOCATE(MPER_GR(NX,NY))
   READ(IUNIT, END=3000) MPER_GR
END IF
IF (PFLAG(10)) THEN                          !! TM1 PERIOD [S].
   IF (.NOT.ALLOCATED(TM1_GR )) ALLOCATE(TM1_GR(NX,NY))
   READ(IUNIT, END=3000) TM1_GR
END IF
IF (PFLAG(11)) THEN                          !! TM2 PERIOD [S].
   IF (.NOT.ALLOCATED(TM2_GR )) ALLOCATE(TM2_GR(NX,NY))
   READ(IUNIT, END=3000) TM2_GR
END IF
IF (PFLAG(12)) THEN                          !! WAVE DIRECTION [DEG].
   IF (.NOT.ALLOCATED(MDIR_GR)) ALLOCATE(MDIR_GR(NX,NY))
   READ(IUNIT, END=3000) MDIR_GR
END IF
IF (PFLAG(13)) THEN                          !! DIRECTIONAL SPREAD [DEG].
   IF (.NOT.ALLOCATED(SPRE_GR)) ALLOCATE(SPRE_GR(NX,NY))
   READ(IUNIT, END=3000) SPRE_GR
END IF
IF (PFLAG(14)) THEN                          !! NORMALISED WAVE STRESS [%].
   IF (.NOT.ALLOCATED(TAUW_GR)) ALLOCATE(TAUW_GR(NX,NY))
   READ(IUNIT, END=3000) TAUW_GR
END IF

IF (PFLAG(15)) THEN                          !! SEA WAVE HEIGHT [M].
   IF (.NOT.ALLOCATED(HS_SEA_GR)) ALLOCATE(HS_SEA_GR(NX,NY))
   READ(IUNIT, END=3000) HS_SEA_GR
END IF
IF (PFLAG(16)) THEN                          !! SEA PEAK PERIOD [M/S].
   IF (.NOT.ALLOCATED(PPER_SEA_GR)) ALLOCATE(PPER_SEA_GR(NX,NY))
   READ(IUNIT, END=3000) PPER_SEA_GR
END IF
IF (PFLAG(17)) THEN                          !! SEA MEAN PERIOD [M/S].
   IF (.NOT.ALLOCATED(MPER_SEA_GR)) ALLOCATE(MPER_SEA_GR(NX,NY))
   READ(IUNIT, END=3000) MPER_SEA_GR
END IF
IF (PFLAG(18)) THEN                          !! SEA TM1 PERIOD [M/S].
   IF (.NOT.ALLOCATED(TM1_SEA_GR)) ALLOCATE(TM1_SEA_GR(NX,NY))
   READ(IUNIT, END=3000) TM1_SEA_GR
END IF
IF (PFLAG(19)) THEN                          !! SEA TM2 PERIOD [M/S].
   IF (.NOT.ALLOCATED(TM2_SEA_GR)) ALLOCATE(TM2_SEA_GR(NX,NY))
   READ(IUNIT, END=3000) TM2_SEA_GR
END IF
IF (PFLAG(20)) THEN                          !! SEA DIRECTION [DEG].
   IF (.NOT.ALLOCATED(MDIR_SEA_GR)) ALLOCATE(MDIR_SEA_GR(NX,NY))
   READ(IUNIT, END=3000) MDIR_SEA_GR
END IF
IF (PFLAG(21)) THEN                          !! SEA DIRECTIONAL SPREAD[DEG].
   IF (.NOT.ALLOCATED(SPRE_SEA_GR)) ALLOCATE(SPRE_SEA_GR(NX,NY))
   READ(IUNIT, END=3000) SPRE_SEA_GR
END IF

IF (PFLAG(23)) THEN                          !! SWELL WAVE HEIGHT [M].
   IF (.NOT.ALLOCATED(HS_SWELL_GR)) ALLOCATE(HS_SWELL_GR(NX,NY))
   READ(IUNIT, END=3000) HS_SWELL_GR
END IF
IF (PFLAG(24)) THEN                          !! SWELL PEAK PERIOD [M/S].
   IF (.NOT.ALLOCATED(PPER_SWELL_GR)) ALLOCATE(PPER_SWELL_GR(NX,NY))
   READ(IUNIT, END=3000) PPER_SWELL_GR
END IF
IF (PFLAG(25)) THEN                          !! SWELL MEAN PERIOD [M/S].
   IF (.NOT.ALLOCATED(MPER_SWELL_GR)) ALLOCATE(MPER_SWELL_GR(NX,NY))
   READ(IUNIT, END=3000) MPER_SWELL_GR
END IF
IF (PFLAG(26)) THEN                          !! SWELL TM1 PERIOD [M/S].
   IF (.NOT.ALLOCATED(TM1_SWELL_GR)) ALLOCATE(TM1_SWELL_GR(NX,NY))
   READ(IUNIT, END=3000) TM1_SWELL_GR
END IF
IF (PFLAG(27)) THEN                          !! SWELL TM2 PERIOD [M/S].
   IF (.NOT.ALLOCATED(TM2_SWELL_GR)) ALLOCATE(TM2_SWELL_GR(NX,NY))
   READ(IUNIT, END=3000) TM2_SWELL_GR
END IF
IF (PFLAG(28)) THEN                          !! SWELL DIRECTION [DEG].
   IF (.NOT.ALLOCATED(MDIR_SWELL_GR)) ALLOCATE(MDIR_SWELL_GR(NX,NY))
   READ(IUNIT, END=3000) MDIR_SWELL_GR
END IF
IF (PFLAG(29)) THEN                          !! SWELL DIRECTIONAL SPREAD[DEG].
   IF (.NOT.ALLOCATED(SPRE_SWELL_GR)) ALLOCATE(SPRE_SWELL_GR(NX,NY))
   READ(IUNIT, END=3000) SPRE_SWELL_GR
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. END OF FILE.                                                          !
!        ------------                                                          !
!                                                                              !
      IEOF = .FALSE.
      RETURN

 3000 CONTINUE
      IEOF = .TRUE.

END SUBROUTINE INGRID
