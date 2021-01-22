! ----------------------------------------------------------------
! MODULE offline_storage_link_module
! ----------------------------------------------------------------
MODULE offline_storage_link_module

  USE utility
  USE point_module
  USE link_module
  USE scalar_module
  USE linear_link_module
  USE flow_coeff
  USE bc_module
  USE nonfluvial_link_module
  USE storage_factory_module
  USE bucket_module

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE offline_storage_link
  ! 
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(storage_link_t) :: offline_storage_link
     ! The actual storage bucket
     DOUBLE PRECISION :: yconnect
   CONTAINS
     PROCEDURE :: coeff => offline_storage_link_coeff
     PROCEDURE :: initialize => offline_storage_link_initialize
     PROCEDURE :: backward_sweep => offline_storage_link_backward
  END type offline_storage_link

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION offline_storage_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION offline_storage_link_initialize(this, ldata, bcman, sclrman, metman, auxdata) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (offline_storage_link), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman
    TYPE (json_value), POINTER, INTENT(IN) :: auxdata

    TYPE (json_core) :: json
    LOGICAL :: found
    DOUBLE PRECISION :: theelev
    CHARACTER (LEN=256) :: msg, fld
    
    ierr = 0

    ierr = ierr + this%storage_link_t%initialize(ldata, bcman, sclrman, metman, auxdata)

    IF (.NOT. ASSOCIATED(auxdata)) THEN
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

    fld = "InletElevation"
    theelev = -9999.0
    CALL json%get(auxdata, fld, theelev, found)
    IF (json%failed()) THEN
       WRITE(msg, *) 'link ', this%id, ': JSON error looking for  "', fld, '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    END IF

    IF (ierr .EQ. 0) THEN
       this%yconnect = theelev
    END IF

    CALL json%destroy()

    
  END FUNCTION offline_storage_link_initialize


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
       dvdy = this%bstor%storage%dvdy(pt2%hnow%y)
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
  ! SUBROUTINE offline_storage_link_backward
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_link_backward(this, deltat, dsbc_type)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: dsbc_type
    DOUBLE PRECISION, INTENT(IN) :: deltat

    CALL this%storage_link_t%backward_sweep(deltat, dsbc_type)
    CALL this%bstor%hydro_by_stage(this%pt(this%npoints)%hnow%y, deltat)
    

  END SUBROUTINE offline_storage_link_backward



END MODULE offline_storage_link_module
