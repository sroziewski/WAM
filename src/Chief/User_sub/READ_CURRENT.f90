SUBROUTINE READ_CURRENT

! ---------------------------------------------------------------------------- !
!                                                                              !
!   READ_CURRENT - READ CURRENT FIELD.                                         !
!                                                                              !
!     SUSANNE HASSELMANN        JUNE     1990.                                 !
!     H. GUNTHER    ECMWF/GKSS  DECEMBER 1990  MODIFIED FOR CYCLE_4.           !
!     H. GUNTHER      GKSS      JANUARY  2002  MODIFIED FOR FT90.              !
!                                                                              !
!     PURPOSE.                                                                 !
!     --------                                                                 !
!                                                                              !
!       TO READ A CURRENT FILE AND TRANSFER THE DATA TO THE PREPROC MODULE.    !
!                                                                              !
!     METHOD.                                                                  !
!     -------                                                                  !
!                                                                              !
!       FILE09, WHICH IS DEFINED IN THE USER INPUT, IS ASSIGNED TO IU09.       !
!       THE FILE MUST STORE:                                                   !
!           1. RECORD: THE CURRENT DATA HEADER.                                !
!           FOLLOWING RECORDS: THE CURRENT DATA MATRIX.                        !
!                                                                              !
!       THE FOLLOWING INFORMATION HAS TO BE TRANSFERRED BY SUB. SET_CURRENT    !
!       TO THE PREPROC MODULE:                                                 !
!          INTEGER :: N_LON      !! NUMBER OF LONGITUDES IN GRID.              !
!          INTEGER :: N_LAT      !! NUMBER OF LATITUDES IN GRID.               !
!          REAL    :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].          !
!          REAL    :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].         !
!          REAL    :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].              !
!          REAL    :: NORTH      !! NORTH LATITUDE OF GRID [DEG].              !
!          REAL    :: WEST       !! WEST LONGITUDE OF GRID [DEG].              !
!          REAL    :: EAST       !! EAST LONGITUDE OF GRID [DEG].              !
!          REAL    :: U_MAP(:,:) !! U COMPONENT OF CURRENT MAP [M/S].          !
!          REAL    :: V_MAP(:,:) !! V COMPONENT OF CURRENT MAP [M/S].          !
!                                                                              !
!       THE CURRENTS MUST BE ON A REGULAR LATITUDE-LONGITUDE GRID ARRANGED     !
!       FROM  WEST TO EAST AND FROM SOUTH TO NORTH, WHICH IS                   !
!       THE CURRENT ARRAYS "U_MAP(I,K)" AND  "V_MAP(I,K)" MUST BE ORDERED AS   !
!                 (    1,    1 ) <==> SOUTH WEST                               !
!                 (N_LON,    1 ) <==> SOUTH EAST                               !
!                 (    1, N_LAT) <==> NORTH WEST                               !
!                 (N_LON, N_LAT) <==> NORTH EAST                               !
!                                                                              !
!     REFERENCE.                                                               !
!     ----------                                                               !
!                                                                              !
!       NONE.                                                                  !
!                                                                              !
! ---------------------------------------------------------------------------- !
!                                                                              !
!     EXTERNALS.                                                               !
!     ----------                                                               !

USE WAM_GENERAL_MODULE,   ONLY:  &
&       ABORT1                       !! TERMINATES PROCESSING.
USE WAM_CURRENT_MODULE,   ONLY:  &
&       SET_CURRENT                  !! TRANSFERS CURRENTS TO MODULE.

! ---------------------------------------------------------------------------- !
!                                                                              !
!     MODULE VARIABLES.                                                        !
!     -----------------                                                        !

USE WAM_FILE_MODULE,  ONLY: IU09, FILE09, IU06

IMPLICIT NONE

! ---------------------------------------------------------------------------- !
!                                                                              !
!     LOCAL VARIABLES.                                                         !
!     ----------------                                                         !

INTEGER               :: N_LON      !! NUMBER OF LONGITUDES IN GRID.
INTEGER               :: N_LAT      !! NUMBER OF LATITUDES IN GRID.
REAL                  :: D_LAT      !! LATITUDE INCREMENT OF GRID [DEG].
REAL                  :: D_LON      !! LONGITUDE INCREMENT OF GRID [DEG].
REAL                  :: SOUTH      !! SOUTH LATITUDE OF GRID [DEG].
REAL                  :: NORTH      !! NORTH LATITUDE OF GRID [DEG].
REAL                  :: WEST       !! WEST LONGITUDE OF GRID [DEG].
REAL                  :: EAST       !! EAST LONGITUDE OF GRID [DEG].
REAL,    ALLOCATABLE  :: U_MAP(:,:) !! U COMPONENT OF CURRENT MAP [M/S].
REAL,    ALLOCATABLE  :: V_MAP(:,:) !! V COMPONENT OF CURRENT MAP [M/S].

INTEGER   :: J, L, IOS
CHARACTER*14, PARAMETER :: ZERO = ' '

! ---------------------------------------------------------------------------- !
!                                                                              !
!     1. OPEN CURRENT DATA FILE.                                               !
!        -----------------------                                               !

L = LEN_TRIM(FILE09)
IF (L.EQ.0) RETURN

OPEN (UNIT=IU09, FILE=FILE09(1:L), FORM='FORMATTED', STATUS='OLD', IOSTAT=IOS)
IF (IOS.NE.0) THEN
   WRITE (IU06,*) ' ****************************************************'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *        FATAL ERROR IN SUB. READ_CURRENT          *'
   WRITE (IU06,*) ' *       =================================          *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' * CURRENT INPUT FILE COULD NOT BE OPENED           *'
   WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
   WRITE (IU06,*) ' *    FILE NAME IS  FILE09 = ', FILE09(1:L)
   WRITE (IU06,*) ' *    UNIT IS         IU09 = ', IU09
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' ****************************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. READ HEADER.                                                          !
!        ------------                                                          !

READ (IU09,'(6F10.5,2I3)',IOSTAT=IOS) SOUTH, NORTH, WEST, EAST, D_LON, D_LAT,  &
&                                     N_LAT, N_LON
IF (IOS.NE.0) THEN
   WRITE (IU06,*) ' ****************************************************'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *        FATAL ERROR IN SUB. READ_CURRENT          *'
   WRITE (IU06,*) ' *        ================================          *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' * READ ERROR ON CURRENT FILE.                      *'
   WRITE (IU06,*) ' * FILE HEADER EXPECTED                             *'
   WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
   WRITE (IU06,*) ' *    FILE NAME IS  FILE09 = ', FILE09(1:L)
   WRITE (IU06,*) ' *    UNIT IS         IU09 = ', IU09
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
   WRITE (IU06,*) ' *                                                  *'
   WRITE (IU06,*) ' ****************************************************'
   CALL ABORT1
END IF

! ---------------------------------------------------------------------------- !
!                                                                              !
!     2. ALLOCATE CURRENT INPUT ARRAYS.                                        !
!        ------------------------------                                        !

IF (.NOT.ALLOCATED(U_MAP)) ALLOCATE(U_MAP(N_LON,N_LAT))
IF (.NOT.ALLOCATED(V_MAP)) ALLOCATE(V_MAP(N_LON,N_LAT))

! ---------------------------------------------------------------------------- !
!                                                                              !
!    3. READ CURRENT FIELD.                                                    !
!       -------------------                                                    !

DO J = 1,N_LAT
   READ(IU09,'(8E9.3)',IOSTAT=IOS) U_MAP(1:N_LON,J)
   IF (IOS.NE.0) THEN
      WRITE (IU06,*) ' ****************************************************'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *        FATAL ERROR IN SUB. READ_CURRENT          *'
      WRITE (IU06,*) ' *        ================================          *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' * READ ERROR ON CURRENT FILE.                      *'
      WRITE (IU06,*) ' * U - COMPONENTS EXPECTED                          *'
      WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
      WRITE (IU06,*) ' *    FILE NAME IS  FILE09 = ', FILE09(1:L)
      WRITE (IU06,*) ' *    UNIT IS         IU09 = ', IU09
      WRITE (IU06,*) ' *    RECORD IS          J = ', J
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' ****************************************************'
      CALL ABORT1
   END IF
END DO
DO J = 1,N_LAT
   READ(IU09,'(8E9.3)',IOSTAT=IOS) V_MAP(1:N_LON,J)
   IF (IOS.NE.0) THEN
      WRITE (IU06,*) ' ****************************************************'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *     FATAL ERROR IN SUB. READ_CURRENT_INPUT       *'
      WRITE (IU06,*) ' *     ======================================       *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' * READ ERROR ON CURRENT FILE.                      *'
      WRITE (IU06,*) ' * V - COMPONENTS EXPECTED                          *'
      WRITE (IU06,*) ' *    ERROR CODE IS IOSTAT = ', IOS
      WRITE (IU06,*) ' *    FILE NAME IS  FILE09 = ', FILE09(1:L)
      WRITE (IU06,*) ' *    UNIT IS         IU09 = ', IU09
      WRITE (IU06,*) ' *    RECORD IS          J = ', J
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' *         PROGRAM ABORTS  PROGRAM ABORTS           *'
      WRITE (IU06,*) ' *                                                  *'
      WRITE (IU06,*) ' ****************************************************'
      CALL ABORT1
   END IF
END DO
! ---------------------------------------------------------------------------- !
!                                                                              !
!     4. TRANSFER TO MODEULE.                                                  !
!        --------------------                                                  !

CALL SET_CURRENT (N_LON, N_LAT, D_LON, D_LAT, SOUTH, NORTH, WEST, EAST,        &
&                 U_MAP, V_MAP)

! ---------------------------------------------------------------------------- !
!                                                                              !
!     5. DEALLOCATE ARRAYS AND CLOSE FILE.                                     !
!        ---------------------------------                                     !

DEALLOCATE (U_MAP, V_MAP)
CLOSE (UNIT=IU09)

END SUBROUTINE READ_CURRENT
