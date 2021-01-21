! ----------------------------------------------------------------
! file: linear_link_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created June 28, 2017 by William A. Perkins
! Last Change: 2021-01-21 12:59:52 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE linear_link_module
! ----------------------------------------------------------------
MODULE linear_link_module
  USE link_module
  USE point_module
  USE bc_module
  USE scalar_module
  USE transport_module
  USE cross_section
  USE section_handler_module
  USE mass1_config
  USE general_vars, ONLY: depth_threshold, depth_minimum
  USE flow_coeff
  USE json_module

  IMPLICIT NONE

  PRIVATE

  TYPE, PUBLIC, EXTENDS(link_t) :: linear_link_t
     DOUBLE PRECISION :: latq, latqold
     INTEGER :: npoints
     INTEGER :: input_option
     TYPE (point_t), DIMENSION(:),POINTER :: pt
   CONTAINS
     PROCEDURE :: initialize => linear_link_initialize
     PROCEDURE :: readpts => linear_link_readpts
     PROCEDURE :: points => linear_link_points
     PROCEDURE :: length => linear_link_length
     PROCEDURE :: q_up => linear_link_q_up
     PROCEDURE :: q_down => linear_link_q_down
     PROCEDURE :: y_up => linear_link_y_up
     PROCEDURE :: y_down => linear_link_y_down
     PROCEDURE :: c_up => linear_link_c_up
     PROCEDURE :: c_down => linear_link_c_down
     PROCEDURE :: set_initial => linear_link_set_initial
     PROCEDURE :: read_restart => linear_link_read_restart
     PROCEDURE :: write_restart => linear_link_write_restart
     PROCEDURE :: read_trans_restart => linear_link_read_trans_restart
     PROCEDURE :: write_trans_restart => linear_link_write_trans_restart
     PROCEDURE :: coeff => linear_link_coeff
     PROCEDURE :: forward_sweep => linear_link_forward
     PROCEDURE :: backward_sweep => linear_link_backward
     PROCEDURE :: hydro_update => linear_link_hupdate
     PROCEDURE :: max_courant => linear_link_max_courant
     PROCEDURE :: max_diffuse => linear_link_max_diffuse
     PROCEDURE :: point => linear_link_point
     PROCEDURE :: check => linear_link_check
     PROCEDURE :: pre_transport => linear_link_pre_transport
     PROCEDURE :: trans_interp => linear_link_trans_interp
     PROCEDURE :: transport => linear_link_transport
     ! FIXME: there's got to be a cleaner way
     PROCEDURE :: set_bed_temp => linear_link_bedtemp
     PROCEDURE :: set_bed_depth => linear_link_beddepth
     PROCEDURE :: volume => linear_link_volume
     PROCEDURE :: destroy => linear_link_destroy
  END type linear_link_t

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION linear_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION linear_link_initialize(this, ldata, bcman, sclrman, metman, auxdata) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (linear_link_t), INTENT(INOUT) :: this
    CLASS (link_input_data), INTENT(IN) :: ldata
    CLASS (bc_manager_t), INTENT(IN) :: bcman
    CLASS (scalar_manager), INTENT(IN) :: sclrman
    CLASS (met_zone_manager_t), INTENT(INOUT) :: metman
    TYPE (json_value), POINTER, INTENT(IN) :: auxdata
    
    INTEGER :: i
    CHARACTER (LEN=1024) :: msg
    ierr = 0
    this%id = ldata%linkid
    this%npoints = ldata%npt
    this%dsid = ldata%dsid
    this%input_option = ldata%inopt
    this%tsubstep = .TRUE.

    ALLOCATE(this%pt(this%npoints))

    ! find the "link" bc, if any; children can set this and it will be preserved

    IF (.NOT. ASSOCIATED(this%usbc)) THEN
       IF (ldata%bcid .NE. 0) THEN
          this%usbc => bcman%find(LINK_BC_TYPE, ldata%bcid)
          IF (.NOT. ASSOCIATED(this%usbc) ) THEN
             WRITE (msg, *) 'link ', ldata%linkid, ': unknown link BC id: ', ldata%bcid
             CALL error_message(msg)
             ierr = ierr + 1
          END IF
       END IF
    END IF

    ! find the downstream bc, if any; children can set this and it will be preserved

    IF (.NOT. ASSOCIATED(this%dsbc)) THEN
       IF (ldata%dsbcid .NE. 0) THEN
          this%dsbc => bcman%find(LINK_BC_TYPE, ldata%dsbcid)
          IF (.NOT. ASSOCIATED(this%dsbc) ) THEN
             WRITE (msg, *) 'link ', ldata%linkid, &
                  &': unknown downstream BC id: ', ldata%dsbcid
             CALL error_message(msg)
             ierr = ierr + 1
          END IF
       END IF
    END IF

    this%latq = 0.0
    this%latqold = 0.0

    IF (sclrman%nspecies .GT. 0) THEN
       DO i = 1, this%npoints
          ALLOCATE(this%pt(i)%trans%cnow(sclrman%nspecies))
          ALLOCATE(this%pt(i)%trans%cold(sclrman%nspecies))
          this%pt(i)%trans%cnow = 0.0
          this%pt(i)%trans%cold = 0.0
          this%pt(i)%trans%bedcond = ldata%bedcond
          this%pt(i)%trans%beddensity = ldata%beddensity
          this%pt(i)%trans%bedspheat = ldata%bedspheat
          this%pt(i)%trans%beddepth = ldata%beddepth
          this%pt(i)%trans%bedtemp = ldata%bedgwtemp
          this%pt(i)%trans%bedtempold = ldata%bedgwtemp
          this%pt(i)%trans%bedgwtemp = ldata%bedgwtemp
       END DO

       
       ALLOCATE(this%species(sclrman%nspecies))

       DO i = 1, sclrman%nspecies
          this%species(i)%scalar => sclrman%species(i)%p

          SELECT CASE (this%species(i)%scalar%bctype)
          CASE (TEMP_BC_TYPE)
             IF (ldata%tbcid .GT. 0) THEN
                this%species(i)%usbc => bcman%find(TEMP_BC_TYPE, ldata%tbcid)
             ELSE
                NULLIFY(this%species(i)%usbc)
             END IF
             IF (ldata%ltbcid .GT. 0) THEN
                this%species(i)%latbc => bcman%find(TEMP_BC_TYPE, ldata%ltbcid)
             ELSE
                NULLIFY(this%species(i)%latbc)
             END IF
             IF (ldata%dsid .LE. 0) THEN
                this%species(i)%dsbc => bcman%find(TEMP_BC_TYPE, DS_BC_ID)
             ELSE
                NULLIFY(this%species(i)%dsbc)
             END IF
          CASE (TRANS_BC_TYPE)
             IF (ldata%gbcid .GT. 0) THEN
                this%species(i)%usbc => bcman%find(TRANS_BC_TYPE, ldata%gbcid)
             ELSE
                NULLIFY(this%species(i)%usbc)
             END IF
             IF (ldata%lgbcid .GT. 0) THEN
                this%species(i)%latbc => bcman%find(TRANS_BC_TYPE, ldata%lgbcid)
             ELSE
                NULLIFY(this%species(i)%latbc)
             END IF
             IF (ldata%dsid .LE. 0) THEN
                this%species(i)%dsbc => bcman%find(TRANS_BC_TYPE, DS_BC_ID)
             ELSE
                NULLIFY(this%species(i)%dsbc)
             END IF
          CASE DEFAULT
          END SELECT

          IF (this%species(i)%scalar%needmet .AND. ldata%mzone .NE. 0) THEN
             this%species(i)%met => metman%find(ldata%mzone)
          ELSE
             NULLIFY(this%species(i)%met)
          END IF

       END DO
    ELSE
       NULLIFY(this%species)
    END IF

  END FUNCTION linear_link_initialize

  ! ----------------------------------------------------------------
  !  FUNCTION linear_link_readpts
  ! ----------------------------------------------------------------
  FUNCTION linear_link_readpts(this, theconfig, sectman, punit, lineno) RESULT(ierr)
    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (linear_link_t), INTENT(INOUT) :: this
    TYPE (configuration_t), INTENT(IN) :: theconfig
    CLASS (section_handler), INTENT(INOUT) :: sectman
    INTEGER, INTENT(IN) :: punit
    INTEGER, INTENT(INOUT) :: lineno
    INTEGER :: iostat
    CHARACTER (LEN=1024) :: msg
    INTEGER :: linkid, pnum, sectid, i
    DOUBLE PRECISION :: x, thalweg, manning, kdiff, ksurf
    DOUBLE PRECISION :: length, delta_x, slope, start_el, end_el
    CLASS (xsection_t), POINTER :: xsect
    DOUBLE PRECISION :: cl_factor

    ierr = 0
    cl_factor = theconfig%channel_len_factor()

    WRITE(msg, 100) this%id, &
         &this%input_option, &
         &this%npoints, &
         &cl_factor
100 FORMAT("Reading/building points for link = ", I6, &
         &", input option = ", I2, &
         &", points = ", I4, &
         &", length factor = ", F7.1)
    CALL status_message(msg)

    SELECT CASE(this%input_option)
    CASE(1)                    ! point based input
       DO i=1,this%npoints
          READ(punit, *, IOSTAT=iostat)&
               &linkid, &
               &pnum, &
               &x, &
               &sectid, &
               &thalweg, &
               &manning, &
               &kdiff, &
               &ksurf

          IF (IS_IOSTAT_END(iostat)) THEN
             WRITE(msg, *) 'Premature end of file near line ', lineno, &
                  &' reading points for link ', this%id
             CALL error_message(msg)
             ierr = ierr + 1
             RETURN
          ELSE IF (iostat .NE. 0) THEN
             WRITE(msg, *) 'Read error near line ', lineno, &
                  &' reading points for link ', this%id
             CALL error_message(msg)
             ierr = ierr + 1
             RETURN
          END IF

          lineno = lineno + 1

          ! Convert lengths to internal length units
          x = x*cl_factor

          this%pt(i)%x = x
          this%pt(i)%thalweg = thalweg
          IF (manning .LE. 0.0) THEN
             WRITE(msg, *) 'link ', this%id, ', point ', pnum, &
                  &': error: invalid value for mannings coefficient: ', &
                  &manning
             CALL error_message(msg)
             ierr = ierr + 1
             CYCLE
          END IF
          this%pt(i)%manning = manning
          this%pt(i)%kstrick = theconfig%res_coeff/this%pt(i)%manning
          this%pt(i)%k_diff = kdiff

          ! ksurf is ignored

          xsect => sectman%find(sectid)
          IF (.NOT. ASSOCIATED(xsect)) THEN
             WRITE(msg, *) "link ", this%id, ", point = ", pnum, &
                  &": error: cannot find cross section ", sectid
             CALL error_message(msg)
          END IF
          this%pt(i)%xsection%p => xsect

       END DO

    CASE(2)                    ! link based input

       READ(punit, *, IOSTAT=iostat) &
            &linkid, &
            &length, &
            &start_el, &
            &end_el, &
            &sectid, &
            &manning, &
            &kdiff, &
            &ksurf

       ! Convert length to internal length units
       length = length*cl_factor

       IF (manning .LE. 0.0) THEN
          WRITE(msg, *) 'link ', this%id,  &
               &': error: invalid value for mannings coefficient: ', &
               &manning
          CALL error_message(msg)
          ierr = ierr + 1
       END IF

       IF (length .GT. 0.0) THEN
          delta_x = length/(this%npoints - 1)
          slope = (start_el - end_el)/length
       ELSE
          delta_x = 0.0
          slope = 0.0
       END IF

       xsect =>  sectman%find(sectid)
       IF (.NOT. ASSOCIATED(xsect)) THEN
          WRITE(msg, *) "link ", this%id, "error: cannot find cross section ", sectid
          CALL error_message(msg)
          ierr = ierr + 1
       END IF

       DO i=1, this%npoints
          IF (i .EQ. 1)THEN
             this%pt(i)%x = 0.0
             this%pt(i)%thalweg = start_el
          ELSE
             this%pt(i)%x = this%pt(i-1)%x + delta_x
             this%pt(i)%thalweg = this%pt(i-1)%thalweg - slope*delta_x
          ENDIF

          this%pt(i)%manning = manning
          this%pt(i)%kstrick = theconfig%res_coeff/this%pt(i)%manning
          this%pt(i)%k_diff = kdiff
          this%pt(i)%xsection%p => xsect
          ! ksurf is ignored

       END DO

    CASE DEFAULT
       
       WRITE (msg, *) 'link ', this%id, &
            &': error: unknown input option: ', this%input_option
       CALL error_message(msg)
       ierr = ierr + 1
    END SELECT

  END FUNCTION linear_link_readpts

  ! ----------------------------------------------------------------
  !  FUNCTION linear_link_points
  ! ----------------------------------------------------------------
  FUNCTION linear_link_points(this) RESULT(n)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER :: n
    n = this%npoints
  END FUNCTION linear_link_points

  ! ----------------------------------------------------------------
  !  FUNCTION linear_link_length
  ! ----------------------------------------------------------------
  FUNCTION linear_link_length(this) RESULT(len)

    IMPLICIT NONE
    DOUBLE PRECISION :: len
    CLASS (linear_link_t), INTENT(IN) :: this
    
    len = ABS(this%pt(1)%x - this%pt(this%npoints)%x)

  END FUNCTION linear_link_length


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_q_up
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_q_up(this, interp)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    LOGICAL, INTENT(IN), OPTIONAL :: interp
    INTEGER :: n
    LOGICAL :: useint

    IF (PRESENT(interp)) THEN
       useint = interp
    ELSE
       useint = .FALSE.
    END IF
    n = 1
    IF (useint) THEN
       linear_link_q_up = this%pt(n)%trans%hnow%q
    ELSE
       linear_link_q_up = this%pt(n)%hnow%q
    END IF
  END FUNCTION linear_link_q_up


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_q_down
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_q_down(this, interp)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    LOGICAL, INTENT(IN), OPTIONAL :: interp
    INTEGER :: n
    LOGICAL :: useint

    IF (PRESENT(interp)) THEN
       useint = interp
    ELSE
       useint = .FALSE.
    END IF
    n = this%npoints
    IF (useint) THEN
       linear_link_q_down = this%pt(n)%trans%hnow%q
    ELSE
       linear_link_q_down = this%pt(n)%hnow%q
    END IF
  END FUNCTION linear_link_q_down


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_y_up
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_y_up(this, interp)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    LOGICAL, INTENT(IN), OPTIONAL :: interp
    INTEGER :: n
    LOGICAL :: useint

    IF (PRESENT(interp)) THEN
       useint = interp
    ELSE
       useint = .FALSE.
    END IF

    n = 1

    IF (useint) THEN
       linear_link_y_up = this%pt(n)%trans%hnow%y
    ELSE 
       linear_link_y_up = this%pt(n)%hnow%y
    END IF
  END FUNCTION linear_link_y_up


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_y_down
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_y_down(this, interp)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER :: n
    LOGICAL, INTENT(IN), OPTIONAL :: interp
    LOGICAL :: useint

    IF (PRESENT(interp)) THEN
       useint = interp
    ELSE
       useint = .FALSE.
    END IF

    n = this%npoints

    IF (useint) THEN
       linear_link_y_down = this%pt(n)%trans%hnow%y
    ELSE 
       linear_link_y_down = this%pt(n)%hnow%y
    END IF
  END FUNCTION linear_link_y_down


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_c_up
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION linear_link_c_up(this, ispecies)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: ispecies

    linear_link_c_up = this%pt(1)%trans%cnow(ispecies)
  END FUNCTION linear_link_c_up


  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_c_down
  ! ----------------------------------------------------------------
  FUNCTION linear_link_c_down(this, ispecies) RESULT(c)
    IMPLICIT NONE
    DOUBLE PRECISION :: c
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: ispecies
    INTEGER :: npts
    npts = this%points()
    ASSOCIATE (pt => this%pt(npts)%trans)
      c = pt%cnow(ispecies)
    END ASSOCIATE
  END FUNCTION linear_link_c_down

  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_set_initial
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_set_initial(this, stage, discharge, c)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: stage, discharge, c(:)
    INTEGER :: i, nspecies, s

    IF (ASSOCIATED(this%species)) THEN
       nspecies = SIZE(this%species)
    ELSE
       nspecies = 0
    END IF

    DO i = 1, this%npoints
       ASSOCIATE(pt => this%pt(i))
         CALL pt%hnow%zero()
         CALL pt%hnow%zero()
         pt%hnow%y = MAX(stage, this%pt(i)%thalweg + depth_minimum)
         pt%hold%y = this%pt(i)%hnow%y
         pt%hnow%q = discharge
         pt%hold%q = this%pt(i)%hnow%q

         pt%trans%hnow = pt%hnow
         pt%trans%hold = pt%hnow

         ! FIXME: just do this eventually
         ! DO s = 1, nspecies
         !    this%pt(i)%trans%cnow(s) = c(s)
         !    this%pt(i)%trans%cold(s) = c(s)
         ! END DO
         
         IF (nspecies .GT. 0) THEN
            pt%trans%hnow = pt%hnow
            pt%trans%hold = pt%trans%hnow
         END IF

         DO s = 1, nspecies
            pt%trans%cnow(s) = c(s)
            pt%trans%cold(s) = c(s)
         END DO
       END ASSOCIATE
    END DO

  END SUBROUTINE linear_link_set_initial


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_read_restart(this, iunit)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER :: i, in_id, in_pt,iostat, ierr
    DOUBLE PRECISION ::  q, y
    CHARACTER (LEN=1024) :: msg

    ierr = 0

    DO i = 1, this%npoints
       READ(iunit, IOSTAT=iostat) in_id, in_pt, q, y
       this%pt(i)%hnow%q = q
       this%pt(i)%hnow%y = y
       IF (IS_IOSTAT_END(iostat)) THEN
          WRITE(msg, *) 'link ', this%id, &
               &': premature end of file reading (hydrodynamics) restart for point ', i
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       ELSE IF (iostat .NE. 0) THEN
          WRITE(msg, *) 'link ', this%id, &
               &': error reading (hydrodynamics) restart for point ', i
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       END IF

       IF (this%id .NE. in_id) THEN
          WRITE(msg, *) "link ", this%id, &
               &': error: hotstart link id mismatch: ', in_id
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       END IF

       IF (i .NE. in_pt) THEN
          WRITE(msg, *) "link ", this%id, ", point ", i, &
               &': error: hotstart point index mismatch: ', in_pt
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       END IF
       
       this%pt(i)%hnow%lateral_inflow = 0.0

       ! FIXME: should the transport states be saved too?
       this%pt(i)%trans%hnow%q = this%pt(i)%hnow%q
       this%pt(i)%trans%hold%q = this%pt(i)%hnow%q
       this%pt(i)%trans%hnow%y = this%pt(i)%hnow%y
       this%pt(i)%trans%hold%y = this%pt(i)%hnow%y
       
    END DO

    IF (ierr .GT. 0) THEN
       WRITE(msg, *) 'problem reading restart for link', this%id
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE linear_link_read_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_read_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_read_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE

    CLASS (linear_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER, INTENT(IN) :: nspecies

    INTEGER :: i, s, iostat, ierr = 0
    CHARACTER (LEN=1024) :: msg
    
    DOUBLE PRECISION :: c(nspecies), cold(nspecies), tbed

    ierr = 0

    DO i = 1, this%npoints
       READ(iunit, IOSTAT=iostat) &
            &(c(s), s = 1, nspecies), &
            &(cold(s), s = 1, nspecies), &
            & tbed
       
       IF (IS_IOSTAT_END(iostat)) THEN
          WRITE(msg, *) 'link ', this%id, &
               &': error reading (transport) restart for point ', i
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       ELSE IF (iostat .NE. 0) THEN
          WRITE(msg, *) 'link ', this%id, &
               &': error reading (transport) restart for point ', i
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       END IF
       ASSOCIATE (pt => this%pt(i)%trans)
         DO s = 1, nspecies
            pt%cnow(s) = c(s)
            pt%cold(s) = cold(s)
         END DO
         pt%bedtemp = tbed
         pt%bedtempold = tbed
       END ASSOCIATE

    END DO
    
    IF (ierr .GT. 0) THEN
       WRITE(msg, *) 'problem reading restart (transport) for link', this%id
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE linear_link_read_trans_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_write_restart
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_write_restart(this, iunit)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER :: i, iostat, ierr
    CHARACTER (LEN=1024) :: msg

    ierr = 0
    
    DO i = 1, this%npoints
       WRITE(iunit, IOSTAT=iostat) this%id, i, &
            &this%pt(i)%hnow%q, &
            &this%pt(i)%hnow%y
       IF (iostat .NE. 0) THEN
          WRITE(msg, *) 'link ', this%id, &
               &': error writing (hydrodynamis) restart for point ', i
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       END IF
    END DO

    IF (ierr .GT. 0) THEN
       WRITE(msg, *) 'problem writing restart (hydrodynamics) for link', this%id
       CALL error_message(msg, fatal=.TRUE.)
    END IF
  END SUBROUTINE linear_link_write_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_write_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_write_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER, INTENT(IN) :: nspecies

    INTEGER :: i, s, iostat, ierr = 0
    CHARACTER (LEN=1024) :: msg

    ierr = 0
    DO i = 1, this%npoints
       ASSOCIATE (pt => this%pt(i)%trans)
         WRITE(iunit, IOSTAT=iostat) &
              &(pt%cnow(s), s = 1, nspecies), &
              &(pt%cold(s), s = 1, nspecies), &
              & pt%bedtemp
       END ASSOCIATE
       
       IF (iostat .NE. 0) THEN
          WRITE(msg, *) 'link ', this%id, &
               &': error writing (hydrodynamis) restart for point ', i
          CALL error_message(msg)
          ierr = ierr + 1
          EXIT
       END IF
    END DO

    IF (ierr .GT. 0) THEN
       WRITE(msg, *) 'problem reading restart (transport) for link', this%id
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE linear_link_write_trans_restart


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt
    TYPE (point_t), INTENT(IN) :: pt1, pt2
    TYPE (coeff), INTENT(OUT) :: cf

    CALL error_message("This should not happen: linear_link_coeff should be overridden", &
         &fatal=.TRUE.)
  END SUBROUTINE linear_link_coeff


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_forward
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_forward(this, deltat)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: deltat

    INTEGER :: point
    DOUBLE PRECISION :: bcval, denom
    TYPE (coeff) :: cf

    DO point = 1, this%npoints
       ASSOCIATE (pt => this%pt(point))
         pt%hold = pt%hnow
         pt%xspropold = pt%xsprop
         IF (ASSOCIATED(this%species)) THEN
            pt%trans%hnow = pt%hold
            pt%trans%xsprop = pt%xspropold
         END IF
       END ASSOCIATE
    END DO

    point = 1
    IF (ASSOCIATED(this%ucon)) THEN
       this%pt(point)%sweep%e = this%ucon%coeff_e()
       this%pt(point)%sweep%f = this%ucon%coeff_f()
    ELSE
       IF (ASSOCIATED(this%usbc)) THEN
          bcval = this%usbc%current_value
       ELSE 
          bcval = 0.0
       END IF
       this%pt(point)%hnow%q = bcval
       this%pt(point)%sweep%e = 0.0
       this%pt(point)%sweep%f = bcval - this%pt(point)%hnow%q
    END IF

    IF (ASSOCIATED(this%latbc)) THEN
       this%latqold = this%latq
       this%latq = this%latbc%current_value
       DO point = 1, this%npoints
          this%pt(point)%hnow%lateral_inflow = this%latq
       END DO
    END IF

    DO point = 1, this%npoints - 1
       CALL this%coeff(deltat, this%pt(point), this%pt(point + 1), cf)
       denom = (cf%c*cf%dp - cf%cp*cf%d)
       this%pt(point)%sweep%l = (cf%a*cf%dp - cf%ap*cf%d)/denom
       this%pt(point)%sweep%m = (cf%b*cf%dp - cf%bp*cf%d)/denom
       this%pt(point)%sweep%n = (cf%d*cf%gp - cf%dp*cf%g)/denom

       denom = cf%b - this%pt(point)%sweep%m*(cf%c + cf%d*this%pt(point)%sweep%e)
       this%pt(point+1)%sweep%e = &
            &(this%pt(point)%sweep%l*(cf%c + cf%d*this%pt(point)%sweep%e) - cf%a)/denom
       this%pt(point+1)%sweep%f = &
            &(this%pt(point)%sweep%n*(cf%c + cf%d*this%pt(point)%sweep%e) + &
            &cf%d*this%pt(point)%sweep%f + cf%g)/denom

    END DO


  END SUBROUTINE linear_link_forward


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_backward
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_backward(this, dsbc_type)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: dsbc_type
    DOUBLE PRECISION :: bcval, dy, dq
    INTEGER :: point
    DOUBLE PRECISION :: bslope, qtmp, dqdy
    TYPE (point_t), POINTER :: pt, ptm1
    CHARACTER (LEN=1024) :: msg
    

    point = this%npoints
    pt => this%pt(point)
    
    IF (ASSOCIATED(this%dcon)) THEN

       dy = this%dcon%elev() - pt%hnow%y
       dq = pt%sweep%e*dy + pt%sweep%f

    ELSE IF (dsbc_type .EQ. DSBC_NORMAL) THEN
       ! normal discharge
       ptm1 => this%pt(point - 1)
       bslope = ABS(ptm1%thalweg - pt%thalweg)/&
            &ABS(ptm1%x - pt%x)
       qtmp = pt%xsprop%conveyance*sqrt(bslope)
       dqdy = pt%xsprop%dkdy*sqrt(bslope)

       dy = (qtmp - pt%sweep%f - pt%hnow%q)/(pt%sweep%e - dqdy)
       dq = pt%sweep%e*dy + pt%sweep%f

    ELSE IF (ASSOCIATED(this%dsbc)) THEN

       bcval = this%dsbc%current_value
       SELECT CASE(dsbc_type)
       CASE(DSBC_STAGE)
          ! given downstream stage
          dy = bcval - pt%hnow%y
          dq = pt%sweep%e*dy + pt%sweep%f
       CASE(DSBC_DISCHARGE)
          ! given downstream discharge
          dq = bcval - pt%hnow%q
          dy = (dq - pt%sweep%f)/pt%sweep%e
       CASE DEFAULT
          WRITE(*,*) "Whoa, how did this happen?"
          STOP
       END SELECT
    ELSE
       WRITE(msg, *) "Link ", this%id, ": missing downstream boundary condition"
       CALL error_message(msg, fatal=.TRUE.)
    END IF

    pt%hnow%y = pt%hnow%y + dy
    pt%hnow%q = pt%hnow%q + dq

    DO point = this%npoints - 1, 1, -1

       pt => this%pt(point)
       
       dy = pt%sweep%l*dy + pt%sweep%m*dq + pt%sweep%n
       dq = pt%sweep%e*dy + pt%sweep%f

       pt%hnow%y = pt%hnow%y + dy
       pt%hnow%q = pt%hnow%q + dq
       
    END DO

    

  END SUBROUTINE linear_link_backward


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_hupdate
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_hupdate(this, grav, unitwt, dt)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: grav, unitwt, dt

    INTEGER :: p
    DOUBLE PRECISION :: dx

    DO p = 1, this%npoints
       IF (p .GE. this%npoints) THEN
          dx = ABS(this%pt(p-1)%x - this%pt(p)%x)
       ELSE 
          dx = ABS(this%pt(p+1)%x - this%pt(p)%x)
       END IF
       CALL this%pt(p)%hydro_update(grav, unitwt, dt, dx)
    END DO

  END SUBROUTINE linear_link_hupdate

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_max_courant
  ! ----------------------------------------------------------------
  FUNCTION linear_link_max_courant(this, dt) RESULT(cnmax)

    IMPLICIT NONE
    DOUBLE PRECISION :: cnmax
    CLASS (linear_link_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt

    INTEGER :: i

    cnmax = 0.0
    DO i = 1, this%npoints
       cnmax = MAX(cnmax, this%pt(i)%hnow%courant_num)
    END DO

  END FUNCTION linear_link_max_courant

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION linear_link_max_diffuse
  ! ----------------------------------------------------------------
  FUNCTION linear_link_max_diffuse(this, dt) RESULT(dmax)

    IMPLICIT NONE
    DOUBLE PRECISION :: dmax
    CLASS (linear_link_t), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: dt

    INTEGER :: i

    dmax = 0.0
    DO i = 1, this%npoints
       dmax = MAX(dmax, this%pt(i)%hnow%diffuse_num)
    END DO

  END FUNCTION linear_link_max_diffuse


  ! ----------------------------------------------------------------
  !  FUNCTION linear_link_point
  ! ----------------------------------------------------------------
  FUNCTION linear_link_point(this, idx) RESULT(pt)
    IMPLICIT NONE
    TYPE (point_t), POINTER :: pt
    CLASS (linear_link_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: idx
    CHARACTER (LEN=1024) :: msg
    
    NULLIFY(pt)
    IF (idx .GE. 0 .AND. idx .LE. this%npoints) THEN
       pt => this%pt(idx)
    ELSE 
       WRITE(msg, *) 'Invalid point index (', idx, ') for link ', &
            &this%id, ', npoint = ', this%npoints
       CALL error_message(msg, FATAL=.FALSE.)
    END IF
  END FUNCTION linear_link_point

  ! ----------------------------------------------------------------
  !  FUNCTION linear_link_check
  ! ----------------------------------------------------------------
  FUNCTION linear_link_check(this) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (linear_link_t), INTENT(INOUT) :: this
    INTEGER :: i
    CHARACTER (LEN=1024) :: msg
    TYPE (point_t), POINTER :: pt

    ! why cant I do this
    ! ierr = this%link_t%check()
    ierr = 0
    IF (this%points() .LE. 0) THEN
       WRITE(msg, *) 'link ', this%id, ': has zero points?'
       CALL error_message(msg)
       ierr = ierr + 1
    ELSE
       DO i = 1, this%points()
          pt => this%point(i)
          IF (.NOT. ASSOCIATED(pt%xsection%p)) THEN
             WRITE (msg, *) 'link ', this%id, ': point ', i, &
                  &': no cross section assigned'
             CALL error_message(msg)
             ierr = ierr + 1
          END IF
       END DO
            
    END IF
  END FUNCTION linear_link_check

  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_pre_transport
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_pre_transport(this)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this

    ! do nothing

  END SUBROUTINE linear_link_pre_transport


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_trans_interp
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_trans_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1
    INTEGER :: i
    CLASS (point_t), POINTER :: pt

    DO i = 1, this%points()
       pt => this%point(i)
       pt%trans%hold = pt%trans%hnow
       pt%trans%xspropold = pt%trans%xsprop
       CALL pt%transport_interp(tnow, htime0, htime1)
    END DO

  END SUBROUTINE linear_link_trans_interp

  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_transport
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_transport(this, ispec, tstep, tdeltat, hdeltat)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec, tstep
    DOUBLE PRECISION, INTENT(IN) :: tdeltat, hdeltat

    INTEGER :: i
    DOUBLE PRECISION :: c, qup, qdn

    DO i = 1, this%npoints
       this%pt(i)%trans%cold(ispec) = this%pt(i)%trans%cnow(ispec)
    END DO
    
    ! the default is to just pass the boundary concentration through
    ! to all points. There needs to be a general way of dealing
    ! reverse flow, but get it working first

    qup = this%q_up(.TRUE.)
    qdn = this%q_down(.TRUE.)
    c = this%pt(1)%trans%cnow(ispec)

    IF (qup .GE. 0.0) THEN
       IF (ASSOCIATED(this%species(ispec)%usbc)) THEN
          c = this%species(ispec)%usbc%current_value
       ELSEIF (ASSOCIATED(this%ucon)) THEN
          c = this%ucon%conc(ispec)
       ELSEIF (qup .GT. 0.0) THEN
          ! this is bad, but shouldn't happen often
          CALL error_message("Upstream link w/o transport BC")
       END IF
    END IF
    IF (qdn .LE. 0.0) THEN
       IF (ASSOCIATED(this%dcon)) THEN
          c = this%dcon%conc(ispec)
       ELSEIF (qdn .LT. 0.0) THEN
          ! also bad, but shouldn't happen often
          CALL error_message("Reverse flow w/o transport BC")
       END IF
    END IF
    DO i = 1, this%npoints
       this%pt(i)%trans%cnow(ispec) = c
    END DO

  END SUBROUTINE linear_link_transport


  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_destroy(this)
    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    INTEGER :: i

    IF (ASSOCIATED(this%species)) THEN
       DO i = 1, this%npoints
          DEALLOCATE(this%pt(i)%trans%cnow, &
               &this%pt(i)%trans%cold)
       END DO
       DEALLOCATE(this%species)
    END IF
    DEALLOCATE(this%pt)

  END SUBROUTINE linear_link_destroy


  ! ----------------------------------------------------------------
  !  FUNCTION linear_link_volume
  ! ----------------------------------------------------------------
  FUNCTION linear_link_volume(this) RESULT(v)

    IMPLICIT NONE
    DOUBLE PRECISION :: v
    CLASS (linear_link_t), INTENT(IN) :: this

    INTEGER :: i
    DOUBLE PRECISION :: x0, x1, a0, a1

    v = 0
    DO i = 2, this%npoints
       x0 = this%pt(i-1)%x
       x1 = this%pt(i)%x
       a0 = this%pt(i-1)%xsprop%area
       a1 = this%pt(i)%xsprop%area
       v = v + 0.5*(a1+a0)*ABS(x1-x0)
    END DO
  END FUNCTION linear_link_volume

  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_bedtemp
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_bedtemp(this, tbed)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tbed

    INTEGER :: i
    
    DO i = 1, this%npoints
       this%pt(i)%trans%bedgwtemp = tbed
    END DO
    

  END SUBROUTINE linear_link_bedtemp

  ! ----------------------------------------------------------------
  ! SUBROUTINE linear_link_beddepth
  ! ----------------------------------------------------------------
  SUBROUTINE linear_link_beddepth(this, dbed)

    IMPLICIT NONE
    CLASS (linear_link_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dbed

    INTEGER :: i
    
    DO i = 1, this%npoints
       this%pt(i)%trans%beddepth = dbed
    END DO
    

  END SUBROUTINE linear_link_beddepth




END MODULE linear_link_module
