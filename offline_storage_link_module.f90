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
  USE compartment_transport_module

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
     CLASS (compartment_model), POINTER :: smodel
   CONTAINS
     PROCEDURE :: construct => offline_storage_link_construct
     PROCEDURE :: coeff => offline_storage_link_coeff
     PROCEDURE :: readaux => offline_storage_link_readaux
     PROCEDURE :: pre_transport => offline_storage_pre_transport
     PROCEDURE :: trans_interp => offline_storage_trans_interp
     PROCEDURE :: transport => offline_storage_transport
     PROCEDURE :: read_trans_restart => offline_storage_read_trans_restart
     PROCEDURE :: write_trans_restart => offline_storage_write_trans_restart
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

    ALLOCATE(this%smodel)

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
  ! SUBROUTINE offline_storage_pre_transport
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_pre_transport(this)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this

    DOUBLE PRECISION :: qup, qdn, qin
    DOUBLE PRECISION :: inflow, inflow_old
    DOUBLE PRECISION :: outflow, outflow_old
    DOUBLE PRECISION :: lateral_inflow, lateral_inflow_old
    DOUBLE PRECISION :: storage, storage_old

    CALL this%internal_bc_link_t%pre_transport()

    storage = this%storage%p%volume(this%pt(1)%hnow%y)
    storage_old = this%storage%p%volume(this%pt(1)%hold%y)

    qup = this%pt(1)%hnow%q
    qdn = this%pt(this%points())%hnow%q
    qin = qup - qdn

    IF (qin .LT. 0.0) THEN
       inflow = ABS(qin)
       outflow = 0.0
    ELSE
       outflow = qin
       inflow = ABS(qin)
    ENDIF
    
    qup = this%pt(1)%hold%q
    qdn = this%pt(this%points())%hold%q
    qin = qup - qdn

    IF (qin .LT. 0.0) THEN
       inflow = ABS(qin)
       outflow = 0.0
    ELSE
       outflow = qin
       inflow = ABS(qin)
    ENDIF

    lateral_inflow = 0.0
    lateral_inflow_old = 0.0

    CALL this%smodel%pre_transport(inflow, inflow_old, &
         &outflow, outflow_old, lateral_inflow, lateral_inflow_old, &
         &storage, storage_old)

  END SUBROUTINE offline_storage_pre_transport

  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_trans_interp
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_trans_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1

  END SUBROUTINE offline_storage_trans_interp

  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_read_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_read_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER, INTENT(IN) :: nspecies

    CALL this%internal_bc_link_t%read_trans_restart(iunit, nspecies)
    CALL this%smodel%read_restart(iunit, nspecies)

  END SUBROUTINE offline_storage_read_trans_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_write_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_write_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit, nspecies

    CALL this%internal_bc_link_t%write_trans_restart(iunit, nspecies)
    CALL this%smodel%write_restart(iunit, nspecies)


  END SUBROUTINE offline_storage_write_trans_restart


  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_transport
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_transport(this, ispec, tstep, tdeltat, hdeltat)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec, tstep
    DOUBLE PRECISION, INTENT(IN) :: tdeltat, hdeltat


  END SUBROUTINE offline_storage_transport

  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_link_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_link_destroy(this)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this

    DEALLOCATE(this%storage%p)
    NULLIFY(this%storage%p)
    CALL this%internal_bc_link_t%destroy()
    DEALLOCATE(this%smodel)

  END SUBROUTINE offline_storage_link_destroy

END MODULE offline_storage_link_module
