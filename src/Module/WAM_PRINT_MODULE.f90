MODULE WAM_PRINT_MODULE

! ---------------------------------------------------------------------------- !
!
!   THIS MODULE CONTAINS: OUTPUT TIMES, FLAGS, AND ARRAYS NECESSARY FOR GRIDDED
!                         FIELDS OF PARAMTERS.
!
! ---------------------------------------------------------------------------- !

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. PRINTER OUTPUT UNIT, FILE NAME AND TEST FLAGS.                        !
!        ----------------------------------------------                        !

INTEGER      :: IU06   = 6               !! UNIT FOR PRINTER OUTPUT.
CHARACTER*80 :: FILE06 = 'Grid_Prot'

INTEGER      :: ITEST = 0                !! TEST OUTPUT LEVEL:
                                         !!   .LE. 0  NO OUTPUT
                                         !!   .GT. 0  OUTPUT TO FILE05

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. USER INPUT FILE UNIT AND NAME.                                        !
!        ------------------------------                                        !

INTEGER      :: IU05   = 5               !! INPUT OF USER DATA.
CHARACTER*80 :: FILE05 = 'Grid_User'     !!   (SEE SUB USERIN).

! ---------------------------------------------------------------------------- !
!                                                                              !
!     3. NEXT OUTPUT TIMES AND TIME INCREMENTS.                                !
!        --------------------------------------                                !

CHARACTER  :: CDATEA*14    !! START DATE OF PRINT OUPUT  (YYMMDDHHMM).
CHARACTER  :: CDATEE*14    !! END DATE OF PRINT OUPUT (YYMMDDHHMM).
INTEGER    :: IDELDO       !! PRINT OUTPUT TIMESTEP IN SECONDS.
        
! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. FILE NAME INFORMATION.                                                !
!        ----------------------                                                !

INTEGER      :: IU01 = 1          !! INPUT UNIT FOR INTEGRATED PARAMETER
CHARACTER*80 :: FILE01 = 'MAP'    !! FILE IDENTIFIER
CHARACTER*3  :: USERID            !! USERID FOR FILE NAMES.
CHARACTER*3  :: RUNID             !! RUN IDENTIFIER FOR FILE NAMES.
CHARACTER*60 :: PATH              !! PATH NAME FOR FILES.
CHARACTER*14 :: CDTFILE           !! DATE OF FILE.
INTEGER      :: IDFILE            !! FILE DATE INCREMENT IN SECONDS.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. GENERAL GRID INFORMATION.                                             !
!        -------------------------                                             !

INTEGER  :: NX           !! NUMBER OF LONGITUDES IN GRID.
INTEGER  :: NY           !! NUMBER OF LATITUDES  IN GRID.
LOGICAL  :: PER          !! = .TRUE. IF GRID IS PERIODIC.
REAL     :: AMOWEP       !! MOST WESTERN LONGITUDE IN GRID [DEG].
REAL     :: AMOSOP       !! MOST SOUTHERN LATITUDE IN GRID [DEG].
REAL     :: AMOEAP       !! MOST EASTERN LONGITUDE IN GRID [DEG].
REAL     :: AMONOP       !! MOST NORTHERN LATITUDE IN GRID [DEG].
REAL     :: XDELLA       !! GRID INCREMENT FOR LATITUDE [DEG].
REAL     :: XDELLO       !! GRID INCREMENT FOR LONGITUDE AT EQUATOR [DEG].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     6. GRIDDED INTEGRATED PARAMETER TOTAL SPECTRUM.                          !
!        --------------------------------------------                          !

REAL,    ALLOCATABLE :: U10_GR(:,:)  !! WIND SPEED U10 [M/S].
REAL,    ALLOCATABLE :: UDIR_GR(:,:) !! WIND DIRECTION [DEG].
REAL,    ALLOCATABLE :: US_GR(:,:)   !! FRICTION VELOCITY (M/S).
REAL,    ALLOCATABLE :: CD_GR(:,:)   !! DRAG COEFFICENT.

REAL,    ALLOCATABLE :: HS_GR(:,:)   !! SIG. WAVE HEIGHT [M].
REAL,    ALLOCATABLE :: PPER_GR(:,:) !! PEAK PERIOD [S].
REAL,    ALLOCATABLE :: MPER_GR(:,:) !! MEAN PERIOD [S].
REAL,    ALLOCATABLE :: TM1_GR(:,:)  !! TM1 PERIOD [S].
REAL,    ALLOCATABLE :: TM2_GR(:,:)  !! TM2 PERIOD [S].
REAL,    ALLOCATABLE :: MDIR_GR(:,:) !! MEAN DIRECTION [DEG].
REAL,    ALLOCATABLE :: SPRE_GR(:,:) !! MEAN SPREAD [DEG].
REAL,    ALLOCATABLE :: TAUW_GR(:,:) !! NORMALISED WAVE STRESS [%].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     7. GRIDDED INTEGRATED SEA PARAMETERS.                                    !
!        ----------------------------------                                    !

REAL,    ALLOCATABLE :: HS_SEA_GR(:,:)    !! SEA SIG. WAVE HEIGHT [M].
REAL,    ALLOCATABLE :: PPER_SEA_GR(:,:)  !! SEA PEAK PERIOD [S].
REAL,    ALLOCATABLE :: MPER_SEA_GR(:,:)  !! SEA MEAN PERIOD [S].
REAL,    ALLOCATABLE :: TM1_SEA_GR(:,:)   !! SEA TM1 PERIOD [S].
REAL,    ALLOCATABLE :: TM2_SEA_GR(:,:)   !! SEA TM2 PERIOD [S].
REAL,    ALLOCATABLE :: MDIR_SEA_GR(:,:)  !! SEA MEAN DIRECTION [DEG].
REAL,    ALLOCATABLE :: SPRE_SEA_GR(:,:)  !! SEA MEAN SPREAD [DEG].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     8. GRIDDED INTEGRATED SWELL PARAMETERS.                                  !
!        ------------------------------------                                  !

REAL,    ALLOCATABLE :: HS_SWELL_GR(:,:)   !! SWELL SIG. WAVE HEIGHT [M].
REAL,    ALLOCATABLE :: PPER_SWELL_GR(:,:) !! SWELL PEAK PERIOD [S].
REAL,    ALLOCATABLE :: MPER_SWELL_GR(:,:) !! SWELL MEAN PERIOD [S].
REAL,    ALLOCATABLE :: TM1_SWELL_GR(:,:)  !! SWELL TM1 PERIOD [S].
REAL,    ALLOCATABLE :: TM2_SWELL_GR(:,:)  !! SWELL TM2 PERIOD [S].
REAL,    ALLOCATABLE :: MDIR_SWELL_GR(:,:) !! SWELL MEAN DIRECTION [DEG].
REAL,    ALLOCATABLE :: SPRE_SWELL_GR(:,:) !! SWELL MEAN SPREAD [DEG].

! ---------------------------------------------------------------------------- !
!                                                                              !
!     9. GENERAL SPECTRA INFORMATION.                                          !
!        ----------------------------                                          !

INTEGER          :: KL         !! NUMBER OF DIRECTIONS.
INTEGER          :: ML         !! NUMBER OF FREQUENCIES.
REAL             :: CO         !! LOGARTHMIC FREQUENCY INCREMENT.
REAL,ALLOCATABLE :: FR(:)      !! FREQUENCIES [HZ].
REAL,ALLOCATABLE :: THETA(:)   !! DIRECTIONS [DEG].
REAL             :: SPEC_LAT   !! LATITUDE OF SPECTRUM [DEG].
REAL             :: SPEC_LON   !! LONGITUDE ODF SPECTRUM [DEG].
CHARACTER*14     :: SPEC_DATE  !! DATE OF SPECTRUM.
REAL,ALLOCATABLE :: SPEC(:,:)  !! SPECTRUM [M*M/HZ].
REAL             :: U10        !! WIND SPEED U10 [M/S]..
REAL             :: UDIR       !! WIND DIRECTION [DEG].
REAL             :: US         !! FRICTION VELOCITY (M/S).
REAL             :: CD         !! DRAG COEFFICENT.
REAL             :: HS         !! SIG. WAVE HEIGHT [M].
REAL             :: PPER       !! PEAK PERIOD [S].
REAL             :: MPER       !! MEAN PERIOD [S].
REAL             :: TM1        !! TM1 PERIOD [S].
REAL             :: TM2        !! TM2 PERIOD [S].
REAL             :: MDIR       !! MEAN DIRECTION [DEG].
REAL             :: SPRE       !! MEAN SPREAD [DEG].
REAL             :: TAUW       !! NORMALISED WAVE STRESS [%].

REAL,ALLOCATABLE :: SPEC_SEA(:,:)  !! SEA SPECTRUM [M*M/HZ].
REAL             :: HS_SEA         !! SEA  SIG. WAVE HEIGHT [M].
REAL             :: PPER_SEA       !! SEA PEAK PERIOD [S].
REAL             :: MPER_SEA       !! SEA MEAN PERIOD [S].
REAL             :: TM1_SEA        !! SEA TM1 PERIOD [S].
REAL             :: TM2_SEA        !! SEA TM2 PERIOD [S].
REAL             :: MDIR_SEA       !! SEA MEAN DIRECTION [DEG].
REAL             :: SPRE_SEA       !! SEA MEAN SPREAD [DEG].

REAL,ALLOCATABLE :: SPEC_SWELL(:,:)  !! SWELL SPECTRUM [M*M/HZ].
REAL             :: HS_SWELL         !! SWELL SIG. WAVE HEIGHT [M].
REAL             :: PPER_SWELL       !! SWELL PEAK PERIOD [S].
REAL             :: MPER_SWELL       !! SWELL MEAN PERIOD [S].
REAL             :: TM1_SWELL        !! SWELL TM1 PERIOD [S].
REAL             :: TM2_SWELL        !! SWELL TM2 PERIOD [S].
REAL             :: MDIR_SWELL       !! SWELL MEAN DIRECTION [DEG].
REAL             :: SPRE_SWELL       !! SWELL MEAN SPREAD [DEG].

END MODULE WAM_PRINT_MODULE
