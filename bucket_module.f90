! ----------------------------------------------------------------
! MODULE bucket_module
! ----------------------------------------------------------------
MODULE bucket_module

  USE point_module
  USE storage_module
  USE compartment_transport_module

  IMPLICIT NONE

  TYPE, PUBLIC :: bucket_state
     DOUBLE PRECISION :: y_now, y_old
     DOUBLE PRECISION :: volume_now, volume_old
     DOUBLE PRECISION :: inflow_now, inflow_old
     DOUBLE PRECISION :: outflow_now, outflow_old
   CONTAINS
     PROCEDURE :: step => bucket_state_step
  END type bucket_state
     
  ! ----------------------------------------------------------------
  ! TYPE bucket_t
  !
  ! This represents a well mixed body of water.  
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: bucket_t
     TYPE (bucket_state) :: state
     CLASS (storage_t), POINTER :: storage
     CLASS (compartment_model), POINTER :: cmodel
   CONTAINS
     PROCEDURE :: construct => bucket_construct
     PROCEDURE :: set_initial => bucket_set_initial
     PROCEDURE :: hydro => bucket_hydro_balance
     PROCEDURE :: pre_transport => bucket_pre_transport
     PROCEDURE :: trans_interp => bucket_trans_interp
     PROCEDURE :: transport => bucket_transport
     PROCEDURE :: read_restart => bucket_read_restart
     PROCEDURE :: write_restart => bucket_write_restart
     PROCEDURE :: read_trans_restart => bucket_read_trans_restart
     PROCEDURE :: write_trans_restart => bucket_write_trans_restart
     PROCEDURE :: destroy => bucket_destroy
  END type bucket_t
  
CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_state_step
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_state_step(this)

    IMPLICIT NONE
    CLASS (bucket_state), INTENT(INOUT) :: this

    this%y_old = this%y_now
    this%volume_old = this%volume_now
    this%inflow_old = this%inflow_now
    this%outflow_old = this%outflow_now

  END SUBROUTINE bucket_state_step


  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_construct
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_construct(this, storage, nspecies)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(INOUT) :: this
    CLASS (storage_t), POINTER, INTENT(IN) :: storage
    INTEGER, INTENT(IN) :: nspecies

    ! NOTE: this pointer is now managed here!
    this%storage => storage

    IF (nspecies .GT. 0) THEN
       ALLOCATE(this%cmodel)
       CALL this%cmodel%construct(nspecies)
       NULLIFY(this%cmodel%avgpt%xsection%p)
       this%cmodel%avgpt%thalweg = this%storage%bottom()
    ELSE
       NULLIFY(this%cmodel)
       
    END IF
  END SUBROUTINE bucket_construct

  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_set_initial
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_set_initial(this, y, c)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(INOUT) :: this
    DOUBLE PRECISION :: y, c(:)

    this%state%y_now = y
    this%state%volume_now = this%storage%volume(y)
    this%state%inflow_now = 0.0
    this%state%outflow_now = 0.0
    CALL this%state%step()
    
    this%cmodel%avgpt%trans%cnow = c
    this%cmodel%avgpt%trans%cold = c

  END SUBROUTINE bucket_set_initial


  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_hydro_balance
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_hydro_balance(this, inflow, outflow, deltat)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: inflow, outflow, deltat

    ASSOCIATE (s => this%state)
      CALL s%step()
      s%inflow_now = inflow
      s%outflow_now = outflow

      s%volume_now = s%volume_old + (inflow - outflow)*deltat
      s%y_now = this%storage%stage(s%volume_now)
    END ASSOCIATE

  END SUBROUTINE bucket_hydro_balance

  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_stage_balance
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_stage_balance(this, ynow, deltat)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: ynow, deltat

    DOUBLE PRECISION :: q

    ASSOCIATE (s => this%state)
      CALL s%step()
      s%y_now = ynow
      s%volume_now = this%storage%volume(s%y_now)
      q = (s%volume_now - s%volume_old)/deltat
      IF (q .GT. 0.0) THEN
         s%inflow_now = q
         s%outflow_now = 0.0
      ELSE
         s%inflow_now = 0.0
         s%outflow_now = -q
      END IF
    END ASSOCIATE

  END SUBROUTINE bucket_stage_balance


  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_pre_transport
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_pre_transport(this)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(INOUT) :: this

    ASSOCIATE (s => this%state, pt => this%cmodel%avgpt)
      ! Try come up with some reasonable cross section properties for
      ! the compartement. We need depth, width, and area that will
      ! produce the correct temperature and tdg source terms
      pt%xsprop%depth = this%storage%depth(s%y_now)
      pt%xsprop%topwidth = sqrt(this%storage%area(s%y_now))
      pt%xsprop%area = pt%xsprop%depth*pt%xsprop%topwidth
      pt%xspropold%depth = this%storage%depth(s%y_old)
      pt%xspropold%topwidth = sqrt(this%storage%area(s%y_old))
      pt%xspropold%area = pt%xsprop%depth*pt%xsprop%topwidth
      
      CALL this%cmodel%pre_transport(&
           &s%inflow_now, s%inflow_old, &
           &s%outflow_now, s%outflow_old, &
           &0.0, 0.0, &
           &s%volume_now, s%volume_old)
    END ASSOCIATE
    

  END SUBROUTINE bucket_pre_transport

  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_trans_interp
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_trans_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1

    CALL this%cmodel%trans_interp(tnow, htime0, htime1)

  END SUBROUTINE bucket_trans_interp



  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_transport
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_transport(this, ispec, tstep, tdeltat, cin, scalar)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec, tstep
    DOUBLE PRECISION, INTENT(IN) :: tdeltat, cin
    CLASS (link_scalar), INTENT(INOUT), POINTER :: scalar

    CALL this%cmodel%transport(ispec, cin, cin, tstep, tdeltat, scalar)

  END SUBROUTINE bucket_transport


  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_read_restart(this, iunit)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit

    INTEGER :: iostat

    ASSOCIATE (s => this%state)
      READ(iunit, IOSTAT=iostat) &
         &s%y_now, s%y_old, &
         &s%volume_now, s%volume_old, &
         &s%inflow_now, s%inflow_old, &
         &s%outflow_now, s%outflow_old
    END ASSOCIATE

    IF (IS_IOSTAT_END(iostat)) THEN
       CALL error_message('bucket: premature end of file reading (hydro) restart', fatal=.TRUE.)
    ELSE IF (iostat .NE. 0) THEN
       CALL error_message('bucket: reading (hydro) restart', fatal=.TRUE.)
    END IF


  END SUBROUTINE bucket_read_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_read_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_read_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit, nspecies

    CALL this%cmodel%read_restart(iunit, nspecies)

  END SUBROUTINE bucket_read_trans_restart


  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_write_restart
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_write_restart(this, iunit)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER :: iostat

    ASSOCIATE (s => this%state)
      WRITE(iunit, IOSTAT=iostat) &
         &s%y_now, s%y_old, &
         &s%volume_now, s%volume_old, &
         &s%inflow_now, s%inflow_old, &
         &s%outflow_now, s%outflow_old
    END ASSOCIATE

    IF (iostat .NE. 0) THEN
       CALL error_message('bucket: writing (hydro) restart', fatal=.TRUE.)
    END IF

  END SUBROUTINE bucket_write_restart

  
  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_write_trans_restart
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_write_trans_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: iunit, nspecies

    CALL this%cmodel%write_restart(iunit, nspecies)

  END SUBROUTINE bucket_write_trans_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE bucket_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE bucket_destroy(this)

    IMPLICIT NONE
    CLASS (bucket_t), INTENT(INOUT) :: this
    IF (ASSOCIATED(this%storage)) THEN
       CALL this%storage%destroy()
       DEALLOCATE(this%storage)
    END IF
    IF (ASSOCIATED(this%cmodel)) THEN
       CALL this%cmodel%destroy()
       DEALLOCATE(this%cmodel)
    END IF
  END SUBROUTINE bucket_destroy

END MODULE bucket_module
