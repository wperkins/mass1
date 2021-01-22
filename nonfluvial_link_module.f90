! ----------------------------------------------------------------
! file: nonfluvial_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July 17, 2017 by William A. Perkins
! Last Change: 2021-01-22 12:34:26 d3g096
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
  USE json_module
  USE storage_factory_module
  USE bucket_module

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! TYPE internal_bc_link_t
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(linear_link_t) :: internal_bc_link_t
   CONTAINS
     PROCEDURE :: max_courant => internal_bc_max_courant
     PROCEDURE :: max_diffuse => internal_bc_max_diffuse
     PROCEDURE :: check => internal_bc_check
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
     DOUBLE PRECISION :: q, qold
   CONTAINS
     PROCEDURE :: coeff => trib_inflow_link_coeff
     PROCEDURE :: initialize => trib_inflow_link_initialize
     PROCEDURE :: pre_transport => trib_inflow_link_pre_transport
     PROCEDURE :: trans_interp => trib_inflow_link_trans_interp
     PROCEDURE :: transport => trib_inflow_link_transport
  END type trib_inflow_link

  ! ----------------------------------------------------------------
  ! TYPE storage_link_t
  !
  ! Links that need a "storage" 
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(internal_bc_link_t) :: storage_link_t
     TYPE (bucket_t) :: bstor
   CONTAINS
     PROCEDURE :: construct => storage_link_construct
     PROCEDURE :: initialize => storage_link_initialize
     PROCEDURE :: pre_transport => storage_pre_transport
     PROCEDURE :: trans_interp => storage_trans_interp
     PROCEDURE :: transport => storage_transport
     PROCEDURE :: read_restart => storage_read_restart
     PROCEDURE :: read_trans_restart => storage_read_trans_restart
     PROCEDURE :: write_restart => storage_write_restart
     PROCEDURE :: write_trans_restart => storage_write_trans_restart
     PROCEDURE :: destroy => storage_link_destroy
  END type storage_link_t

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
  !  FUNCTION internal_bc_check
  ! ----------------------------------------------------------------
  FUNCTION internal_bc_check(this) RESULT (ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (internal_bc_link_t), INTENT(INOUT) :: this
    CHARACTER (LEN=1024) :: msg

    ierr = this%linear_link_t%check()

    IF (this%points() .NE. 2) THEN
       WRITE(msg, *) 'link ', this%id, &
            &': internal boundary link must have 2 points, ',&
            &this%points(), ' points specified'
       CALL error_message(msg)
       ierr = ierr + 1
    END IF

  END FUNCTION internal_bc_check


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
  FUNCTION hydro_link_initialize(this, ldata, bcman, sclrman, metman, auxdata) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (hydro_link), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman
    TYPE (json_value), POINTER, INTENT(IN) :: auxdata


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

    ierr = ierr + this%internal_bc_link_t%initialize(ldata, bcman, sclrman, metman, auxdata)
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
  !  FUNCTION trib_inflow_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION trib_inflow_link_initialize(this, ldata, bcman, sclrman, metman, auxdata) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (trib_inflow_link), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman
    CHARACTER (LEN=1024) :: msg
    TYPE (json_value), POINTER, INTENT(IN) :: auxdata

    ierr = 0

    IF (ldata%bcid .GT. 0) THEN
       this%usbc => bcman%find(LINK_BC_TYPE, ldata%bcid)
       IF (.NOT. ASSOCIATED(this%usbc) ) THEN
          WRITE (msg, *) 'tributary link ', ldata%linkid, ': unknown link BC id: ', ldata%bcid
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
    ELSE
       WRITE (msg, *) 'tributary link ', ldata%linkid, ' requires a link BC, none specified'
       CALL error_message(msg)
       ierr = ierr + 1
    END IF
    ierr = ierr + this%internal_bc_link_t%initialize(ldata, bcman, sclrman, metman, auxdata)

  END FUNCTION trib_inflow_link_initialize

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
  ! SUBROUTINE trib_inflow_link_pre_transport
  ! ----------------------------------------------------------------
  SUBROUTINE trib_inflow_link_pre_transport(this)

    IMPLICIT NONE
    CLASS (trib_inflow_link), INTENT(INOUT) :: this

    CALL this%internal_bc_link_t%pre_transport()

  END SUBROUTINE trib_inflow_link_pre_transport

  ! ----------------------------------------------------------------
  ! SUBROUTINE trib_inflow_link_trans_interp
  ! ----------------------------------------------------------------
  SUBROUTINE trib_inflow_link_trans_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    CLASS (trib_inflow_link), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1

    CALL this%internal_bc_link_t%trans_interp(tnow, htime0, htime1)

  END SUBROUTINE trib_inflow_link_trans_interp


  ! ----------------------------------------------------------------
  ! SUBROUTINE trib_inflow_link_transport
  ! ----------------------------------------------------------------
  SUBROUTINE trib_inflow_link_transport(this, ispec, tstep, tdeltat, hdeltat)

    IMPLICIT NONE
    CLASS (trib_inflow_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec, tstep
    DOUBLE PRECISION, INTENT(IN) :: tdeltat, hdeltat
    DOUBLE PRECISION :: qup, qdn, qin, cin, cup, cdn
    INTEGER :: i
    CHARACTER (LEN=1024) :: msg

    DO i = 1, this%npoints
       this%pt(i)%trans%cold(ispec) = this%pt(i)%trans%cnow(ispec)
    END DO
    
    ! continuity should be conserved
    qup = this%q_up(.TRUE.)
    qdn = this%q_down(.TRUE.)
    qin = this%usbc%current_value ! qdn - qup 

    ! FIXME: check to make sure ucon/dcon are valid
    cup = this%ucon%conc(ispec)
    cdn = this%dcon%conc(ispec)

    cin = 0.0
    IF (ASSOCIATED(this%species(ispec)%usbc)) THEN
       cin = this%species(ispec)%getusbc()
    ELSE
       WRITE(msg, *) 'link ', this%id, &
            &': error: tributary inflow w/o conc BC for species ', &
            &ispec
       CALL error_message(msg)
    END IF

    IF (qup .GE. 0.0) THEN
       IF (qin .GT. 0.0) THEN
          cdn = (qup*cup + qin*cin)/qdn
       ELSE
          cdn = cup
       END IF
    END IF
    IF (qdn .LE. 0.0) THEN
       IF (qin .GT. 0.0) THEN
          cup = (qin*qin + (-qdn)*cdn)/qup
       ELSE
          cup = cdn
       END IF
    END IF
    this%pt(1)%trans%cnow(ispec) = cup
    this%pt(2)%trans%cnow(ispec) = cdn
  END SUBROUTINE trib_inflow_link_transport

  ! ----------------------------------------------------------------
  ! SUBROUTINE storage_link_construct
  ! ----------------------------------------------------------------
  SUBROUTINE storage_link_construct(this)

    IMPLICIT NONE
    CLASS (storage_link_t), INTENT(INOUT) :: this

    CALL this%internal_bc_link_t%construct()
    this%needaux = .TRUE.
    
  END SUBROUTINE storage_link_construct

  ! ----------------------------------------------------------------
  !  FUNCTION storage_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION storage_link_initialize(this, ldata, bcman, sclrman, metman, auxdata) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (storage_link_t), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman
    TYPE (json_value), POINTER, INTENT(IN) :: auxdata

    TYPE (json_core) :: json
    TYPE (storage_factory) :: factory
    LOGICAL :: found
    TYPE (json_value), POINTER :: sinfo
    TYPE (storage_ptr) :: astor
    CHARACTER (LEN=256) :: msg, fld
    
    ierr = 0

    ierr = ierr + this%internal_bc_link_t%initialize(ldata, bcman, sclrman, metman, auxdata)

    IF (.NOT. ASSOCIATED(auxdata)) THEN
       WRITE(msg, *) 'link ', this%id, ': link w/ storage requires auxiliary data'
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
       WRITE(msg, *) 'link ', this%id, ': storage required value "', fld, '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    END IF

    IF (ierr .EQ. 0) THEN
       astor = factory%generate(sinfo)
       CALL this%bstor%construct(astor%p, sclrman%nspecies)
    END IF

    CALL json%destroy()

    
  END FUNCTION storage_link_initialize

  ! ----------------------------------------------------------------
  ! SUBROUTINE storage_pre_transport
  ! ----------------------------------------------------------------
  SUBROUTINE storage_pre_transport(this)

    IMPLICIT NONE
    CLASS (storage_link_t), INTENT(INOUT) :: this

    CALL this%internal_bc_link_t%pre_transport()
    CALL this%bstor%pre_transport()

  END SUBROUTINE storage_pre_transport

  ! ----------------------------------------------------------------
  ! SUBROUTINE storage_trans_interp
  ! ----------------------------------------------------------------
  SUBROUTINE storage_trans_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    CLASS (storage_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1

    CALL this%internal_bc_link_t%trans_interp(tnow, htime0, htime1)
    CALL this%bstor%trans_interp(tnow, htime0, htime1)

  END SUBROUTINE storage_trans_interp

  ! ----------------------------------------------------------------
  ! SUBROUTINE storage_transport
  ! ----------------------------------------------------------------
  SUBROUTINE storage_transport(this, ispec, tstep, tdeltat, hdeltat)

    IMPLICIT NONE
    CLASS (storage_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec, tstep
    DOUBLE PRECISION, INTENT(IN) :: tdeltat, hdeltat

    DOUBLE PRECISION :: qin, cin, cout, qs, cs

    ! Initially assume no outflow from the storage
    CALL this%internal_bc_link_t%transport(ispec, tstep, tdeltat, hdeltat)

    ! This seems overly complicated, but all flow directions need to
    ! be considered to figure the storage inflow concentration.
    
    qin = 0                     ! "in" to the link
    cin = 0
    ASSOCIATE (upt => this%pt(1)%trans, dpt => this%pt(this%npoints)%trans)
      IF (upt%hnow%q .GT. 0.0) THEN
         qin = qin + upt%hnow%q
         cin = cin + upt%cnow(ispec)*upt%hnow%q
      END IF
      IF (dpt%hnow%q .LT. 0.0) THEN
         qin = qin + dpt%hnow%q
         cin = cin + dpt%cnow(ispec)*dpt%hnow%q
      END IF

      IF (qin .GT. 0.0) THEN
         cin = cin/qin
      ELSE
         cin = upt%cnow(ispec)
      END IF

      CALL this%bstor%transport(ispec, tstep, tdeltat, cin, this%species(ispec))

      qs = this%bstor%outflow(.TRUE.)
      IF (qs .GT. 0.0) THEN
         cs = this%bstor%outflow_conc(ispec)
         cin = cin*qin + cs*qs
         cout = cin/(qin + qs)
         IF (upt%hnow%q .LT. 0.0) THEN
            upt%cnow(ispec) = cout
         END IF
         IF (dpt%hnow%q .GT. 0.0) THEN
            dpt%cnow(ispec) = cout
         END IF
      END IF
    END ASSOCIATE
    
  END SUBROUTINE storage_transport

  ! ----------------------------------------------------------------
  ! SUBROUTINE storage_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE storage_read_restart(this, iunit)

    IMPLICIT NONE
    CLASS (storage_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit

    CALL this%internal_bc_link_t%read_restart(iunit)
    CALL this%bstor%read_restart(iunit)

  END SUBROUTINE storage_read_restart


  ! ----------------------------------------------------------------
  ! SUBROUTINE storage_read_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE storage_read_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (storage_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER, INTENT(IN) :: nspecies

    CALL this%internal_bc_link_t%read_trans_restart(iunit, nspecies)
    CALL this%bstor%read_trans_restart(iunit, nspecies)

  END SUBROUTINE storage_read_trans_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE storage_write_restart
  ! ----------------------------------------------------------------
  SUBROUTINE storage_write_restart(this, iunit)

    IMPLICIT NONE
    CLASS (storage_link_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit

    CALL this%internal_bc_link_t%write_restart(iunit)
    CALL this%bstor%write_restart(iunit)

  END SUBROUTINE storage_write_restart


  ! ----------------------------------------------------------------
  ! SUBROUTINE storage_write_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE storage_write_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (storage_link_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit, nspecies

    CALL this%internal_bc_link_t%write_trans_restart(iunit, nspecies)
    CALL this%bstor%write_trans_restart(iunit, nspecies)


  END SUBROUTINE storage_write_trans_restart


  ! ----------------------------------------------------------------
  ! SUBROUTINE storage_link_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE storage_link_destroy(this)

    IMPLICIT NONE
    CLASS (storage_link_t), INTENT(INOUT) :: this

    CALL this%bstor%destroy()
    CALL this%internal_bc_link_t%destroy()

  END SUBROUTINE storage_link_destroy



  
END MODULE nonfluvial_link_module
