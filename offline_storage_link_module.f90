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

  IMPLICIT NONE

  PRIVATE

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

CONTAINS

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

END MODULE offline_storage_link_module
