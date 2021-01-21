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
  TYPE, PUBLIC, EXTENDS(internal_bc_link_t) :: offline_storage_link
     ! The actual storage bucket
     TYPE (bucket_t) :: bstor
     DOUBLE PRECISION :: yconnect
   CONTAINS
     PROCEDURE :: construct => offline_storage_link_construct
     PROCEDURE :: coeff => offline_storage_link_coeff
     PROCEDURE :: initialize => offline_storage_link_initialize
     PROCEDURE :: pre_transport => offline_storage_pre_transport
     PROCEDURE :: trans_interp => offline_storage_trans_interp
     PROCEDURE :: transport => offline_storage_transport
     PROCEDURE :: read_restart => offline_storage_read_restart
     PROCEDURE :: read_trans_restart => offline_storage_read_trans_restart
     PROCEDURE :: write_restart => offline_storage_write_restart
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
  END SUBROUTINE offline_storage_link_construct

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
    TYPE (storage_factory) :: factory
    LOGICAL :: found
    TYPE (json_value), POINTER :: sinfo
    DOUBLE PRECISION :: theelev
    TYPE (storage_ptr) :: astor
    CHARACTER (LEN=256) :: msg, fld
    
    ierr = 0

    ierr = ierr + this%internal_bc_link_t%initialize(ldata, bcman, sclrman, metman, auxdata)

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

    fld = "Storage"
    CALL json%get(auxdata, fld, sinfo, found)
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
    CALL json%get(auxdata, fld, theelev, found)
    IF (json%failed()) THEN
       WRITE(msg, *) 'link ', this%id, ': JSON error looking for  "', fld, '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    END IF

    IF (ierr .EQ. 0) THEN
       astor = factory%generate(sinfo)
       CALL this%bstor%construct(astor%p, sclrman%nspecies)
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
  ! SUBROUTINE offline_storage_pre_transport
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_pre_transport(this)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this

    CALL this%internal_bc_link_t%pre_transport()

    CALL this%bstor%pre_transport()

  END SUBROUTINE offline_storage_pre_transport

  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_trans_interp
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_trans_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1

    CALL this%bstor%trans_interp(tnow, htime0, htime1)

  END SUBROUTINE offline_storage_trans_interp

  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_read_restart(this, iunit)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit

    CALL this%internal_bc_link_t%read_restart(iunit)
    CALL this%bstor%read_restart(iunit)

  END SUBROUTINE offline_storage_read_restart


  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_read_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_read_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER, INTENT(IN) :: nspecies

    CALL this%internal_bc_link_t%read_trans_restart(iunit, nspecies)
    CALL this%bstor%read_trans_restart(iunit, nspecies)

  END SUBROUTINE offline_storage_read_trans_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_write_restart
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_write_restart(this, iunit)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit

    CALL this%internal_bc_link_t%write_restart(iunit)
    CALL this%bstor%write_restart(iunit)

  END SUBROUTINE offline_storage_write_restart


  ! ----------------------------------------------------------------
  ! SUBROUTINE offline_storage_write_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE offline_storage_write_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (offline_storage_link), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit, nspecies

    CALL this%internal_bc_link_t%write_trans_restart(iunit, nspecies)
    CALL this%bstor%write_trans_restart(iunit, nspecies)


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

    CALL this%bstor%destroy()
    CALL this%internal_bc_link_t%destroy()

  END SUBROUTINE offline_storage_link_destroy

END MODULE offline_storage_link_module
