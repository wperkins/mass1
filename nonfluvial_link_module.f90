! ----------------------------------------------------------------
! file: nonfluvial_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July 17, 2017 by William A. Perkins
! Last Change: 2020-12-04 13:26:23 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE nonfluvial_link_module
! ----------------------------------------------------------------
MODULE nonfluvial_link_module
  USE utility
  USE point_module
  USE link_module
  USE scalar_module
  USE linear_link_module
  USE flow_coeff
  USE bc_module
  USE storage_factory_module
  
  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE internal_bc_link_t
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: internal_bc_link_t
   CONTAINS
     PROCEDURE :: max_courant => internal_bc_max_courant
     PROCEDURE :: max_diffuse => internal_bc_max_diffuse
  END type internal_bc_link_t

  ! ----------------------------------------------------------------
  ! TYPE discharge_link
  ! Imposed discharge (type = 2)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(internal_bc_link_t) :: discharge_link
   CONTAINS
     PROCEDURE :: coeff => discharge_link_coeff
  END type discharge_link

  ! ----------------------------------------------------------------
  ! TYPE hydro_link
  ! Like discharge link, but with discharge split into spill and
  ! generation (type = 6)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(discharge_link) :: hydro_link
   CONTAINS
     PROCEDURE :: initialize => hydro_link_initialize
     PROCEDURE :: coeff => hydro_link_coeff
  END type hydro_link

  ! ----------------------------------------------------------------
  ! TYPE ustage_link
  ! Imposed stage upstream (type = 3)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(internal_bc_link_t) :: ustage_link
   CONTAINS
     PROCEDURE :: coeff => ustage_link_coeff
  END type ustage_link

  ! ----------------------------------------------------------------
  ! TYPE dstage_link
  ! Imposed stage downstream (type = 4)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(internal_bc_link_t) :: dstage_link
   CONTAINS
     PROCEDURE :: coeff => dstage_link_coeff
  END type dstage_link

  ! ----------------------------------------------------------------
  ! TYPE trib_inflow_link
  ! Tributary inflow (type = 5)
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(internal_bc_link_t) :: trib_inflow_link
   CONTAINS
     PROCEDURE :: coeff => trib_inflow_link_coeff
  END type trib_inflow_link

  ! ----------------------------------------------------------------
  ! TYPE offline_storage_link
  ! 
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(internal_bc_link_t) :: offline_storage_link
     ! The actual storage bucket
     TYPE (storage_ptr) :: storage
     DOUBLE PRECISION :: yconnect
   CONTAINS
     PROCEDURE :: construct => offline_storage_link_construct
     PROCEDURE :: coeff => offline_storage_link_coeff
     PROCEDURE :: readaux => offline_storage_link_readaux
     PROCEDURE :: destroy => offline_storage_link_destroy
  END type offline_storage_link


  DOUBLE PRECISION, PARAMETER :: eps = 1.0D-09

CONTAINS

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION internal_bc_max_courant
  ! ----------------------------------------------------------------
  FUNCTION internal_bc_max_courant(this, dt) RESULT(cnmax)

    IMPLICIT NONE
    DOUBLE PRECISION :: cnmax
    CLASS (internal_bc_link_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt

    cnmax = 0.0
    
  END FUNCTION internal_bc_max_courant

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION internal_bc_max_diffuse
  ! ----------------------------------------------------------------
  FUNCTION internal_bc_max_diffuse(this, dt) RESULT(dmax)

    IMPLICIT NONE
    DOUBLE PRECISION :: dmax
    CLASS (internal_bc_link_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt

    dmax = 0.0
    
  END FUNCTION internal_bc_max_diffuse

  ! ----------------------------------------------------------------
  ! SUBROUTINE discharge_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE discharge_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE
    CLASS (discharge_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf

    DOUBLE PRECISION :: bcval

    bcval = this%usbc%current_value

    cf%a = 0.0
    cf%b = 1.0
    cf%c = 0.0 !eps
    cf%d = 1.0
    cf%g = pt1%hnow%q - pt2%hnow%q

    cf%ap = 0.0
    cf%bp = 0.0
    cf%cp = eps
    cf%dp = 1.0
    cf%gp = pt1%hnow%q - bcval

  END SUBROUTINE discharge_link_coeff

  ! ----------------------------------------------------------------
  !  FUNCTION hydro_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION hydro_link_initialize(this, ldata, bcman, sclrman, metman) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (hydro_link), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman

    CHARACTER (LEN=1024) :: msg

    ierr = 0

    IF (ldata%bcid .GT. 0) THEN
       this%usbc => bcman%find(HYDRO_BC_TYPE, ldata%bcid)
       IF (.NOT. ASSOCIATED(this%usbc) ) THEN
          WRITE (msg, *) 'link ', ldata%linkid, ': unknown hydro BC id: ', ldata%bcid
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
    ELSE 
       WRITE (msg, *) 'hydro link ', ldata%linkid, ' requires a hydro BC, none specified'
       CALL error_message(msg)
       ierr = ierr + 1
    END IF
    
    ierr = ierr + this%linear_link_t%initialize(ldata, bcman, sclrman, metman)
  END FUNCTION hydro_link_initialize


  ! ----------------------------------------------------------------
  ! SUBROUTINE hydro_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE hydro_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE
    CLASS (hydro_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf
    
    CALL this%discharge_link%coeff(dt, pt1, pt2, cf)

  END SUBROUTINE hydro_link_coeff

  ! ----------------------------------------------------------------
  ! SUBROUTINE ustage_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE ustage_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE
    CLASS (ustage_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf

    DOUBLE PRECISION :: bcval
    bcval = this%usbc%current_value

    cf%a = 0.0
    cf%b = 1.0
    cf%c = 0.0
    cf%d = 1.0
    cf%g = pt1%hnow%q - pt2%hnow%q
    
    cf%ap = 0.0
    cf%bp = 0.0
    cf%cp = 1.0
    cf%dp = eps
    cf%gp = pt1%hnow%y - bcval

  END SUBROUTINE ustage_link_coeff

  ! ----------------------------------------------------------------
  ! SUBROUTINE dstage_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE dstage_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE
    CLASS (dstage_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf

    DOUBLE PRECISION :: bcval
    bcval = this%usbc%current_value

    cf%a = 1.0
    cf%b = eps
    cf%c = 0.0
    cf%d = 0.0
    cf%g = bcval - pt2%hnow%y

    cf%ap = 0.0
    cf%bp = 1.0
    cf%cp = 0.0
    cf%dp = 1.0

    cf%gp = pt1%hnow%q - pt2%hnow%q

  END SUBROUTINE dstage_link_coeff

  ! ----------------------------------------------------------------
  ! SUBROUTINE trib_inflow_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE trib_inflow_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE

    CLASS (trib_inflow_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf

    DOUBLE PRECISION :: bcval
    bcval = this%usbc%current_value

     cf%a = 0.0
     cf%b = 1.0
     cf%c = 0.0
     cf%d = 1.0
     cf%g = pt1%hnow%q + bcval - pt2%hnow%q
     
     cf%ap = 1.0
     cf%bp = 0.0
     cf%cp = 1.0
     cf%dp = 0.0
     cf%gp = pt1%hnow%y - pt2%hnow%y

  END SUBROUTINE trib_inflow_link_coeff

  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_link_construct
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_link_construct(this)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this

    CALL this%internal_bc_link_t%construct()
    this%needaux = .TRUE.

  END SUBROUTINE offline_storage_link_construct


  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE

    CLASS (offline_storage_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf
    DOUBLE PRECISION :: dvdy

    IF (pt2%hnow%y .GT. this%yconnect) THEN
       dvdy = this%storage%p%dvdy(pt2%hnow%y)
    ELSE
       dvdy = 0.0
    END IF

    cf%a = 0.0
    cf%b = -theta
    cf%c = dvdy/dt
    cf%d = -theta
    cf%g = pt2%hnow%q - pt1%hnow%q
    
    cf%ap = 1.0
    cf%bp = 0.0
    cf%cp = 1.0
    cf%dp = 0.0
    cf%gp = pt1%hnow%y - pt2%hnow%y
    
  END SUBROUTINE offline_storage_link_coeff


  ! ----------------------------------------------------------------
  !  FUNCTION offline_storage_link_readaux
  ! ----------------------------------------------------------------
  FUNCTION offline_storage_link_readaux(this, linkaux) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (offline_storage_link), INTENT(INOUT) :: this
    TYPE (json_value), POINTER, INTENT(IN) :: linkaux
    TYPE (json_core) :: json
    TYPE (storage_factory) :: factory
    LOGICAL :: found
    CHARACTER (LEN=256) :: msg, fld

    TYPE (json_value), POINTER :: sinfo
    DOUBLE PRECISION :: theelev

    ierr = 0

    IF (.NOT. ASSOCIATED(linkaux)) THEN
       WRITE(msg, *) 'link ', this%id, ': offline storage link requires auxiliary data'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
       RETURN
    END IF

    CALL json%initialize()
    IF (json%failed()) THEN
       WRITE(msg, *) 'link ', this%id, ': cannot initialize json'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
       RETURN
    END IF

    fld = "Storage"
    CALL json%get(linkaux, fld, sinfo, found)
    IF (json%failed()) THEN
       WRITE(msg, *) 'link ', this%id, ': JSON error looking for ', fld
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    ELSE IF (.NOT. found) THEN
       WRITE(msg, *) 'link ', this%id, ': offline storage required value "', fld, '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    END IF

    fld = "InletElevation"
    theelev = -9999.0
    CALL json%get(linkaux, fld, theelev, found)
    IF (json%failed()) THEN
       WRITE(msg, *) 'link ', this%id, ': JSON error looking for  "', fld, '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    END IF

    IF (ierr .EQ. 0) THEN
       this%storage = factory%generate(sinfo)
       this%yconnect = theelev
    END IF

    CALL json%destroy()
    
  END FUNCTION offline_storage_link_readaux

  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_link_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_link_destroy(this)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this

    DEALLOCATE(this%storage%p)
    NULLIFY(this%storage%p)
    CALL this%internal_bc_link_t%destroy()

  END SUBROUTINE offline_storage_link_destroy


  

END MODULE nonfluvial_link_module
