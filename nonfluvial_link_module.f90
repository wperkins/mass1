! ----------------------------------------------------------------
! file: nonfluvial_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created July 17, 2017 by William A. Perkins
! Last Change: 2021-01-21 13:02:38 d3g096
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


END MODULE nonfluvial_link_module
