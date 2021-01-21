! ----------------------------------------------------------------
! file: compartment_transport_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 15, 2020 by  William Perkins 
! Last Change: 2021-01-20 09:09:06 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE compartment_transport_module
! ----------------------------------------------------------------
MODULE compartment_transport_module

  USE utility
  USE point_module
  USE transport_module

  IMPLICIT NONE

  ! ----------------------------------------------------------------
  ! TYPE compartment_model
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: compartment_model

     ! for storing input, output, and storage conditions, because it's easier
     TYPE (point_t) :: inpt, outpt, avgpt

     ! From hydrodynamics, or whatever
     DOUBLE PRECISION :: storage, storage_old
     
     ! Transport time interpolated storages
     DOUBLE PRECISION :: my_storage, my_storage_old

   CONTAINS
     PROCEDURE :: construct => compartment_model_construct
     PROCEDURE :: pre_transport => compartment_model_pretransport
     PROCEDURE :: trans_interp => compartment_model_transport_interp
     PROCEDURE :: transport => compartment_model_transport
     PROCEDURE :: bed_depth => compartment_model_bed_depth
     PROCEDURE :: bed_temp => compartment_model_bed_temp
     PROCEDURE :: conc => compartment_model_conc
     PROCEDURE :: read_restart => compartment_read_restart
     PROCEDURE :: write_restart => compartment_write_restart
     PROCEDURE :: destroy => compartment_model_destroy
  END type compartment_model
  
  DOUBLE PRECISION, PRIVATE, PARAMETER :: compartment_min_storage = 1.0D-05

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_model_construct
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_model_construct(this, nspec)

    IMPLICIT NONE
    CLASS (compartment_model), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: nspec

    ALLOCATE(this%inpt%trans%cnow(nspec),this%inpt%trans%cold(nspec))
    ALLOCATE(this%outpt%trans%cnow(nspec),this%outpt%trans%cold(nspec))
    ALLOCATE(this%avgpt%trans%cnow(nspec),this%avgpt%trans%cold(nspec))
    

  END SUBROUTINE compartment_model_construct



  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_model_bed_temp
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_model_bed_temp(this, tbed)

    IMPLICIT NONE
    CLASS (compartment_model), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tbed

    this%avgpt%trans%bedtemp = tbed
    this%avgpt%trans%bedtempold = tbed

  END SUBROUTINE compartment_model_bed_temp

  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_model_bed_depth
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_model_bed_depth(this, dbed)

    IMPLICIT NONE
    CLASS (compartment_model), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: dbed

    this%avgpt%trans%beddepth = dbed

  END SUBROUTINE compartment_model_bed_depth

  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_model_pretransport
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_model_pretransport(this, &
       &inflow, inflow_old, outflow, outflow_old, &
       &lateral_inflow, lateral_inflow_old, &
       &storage, storage_old)

    IMPLICIT NONE
    CLASS (compartment_model), INTENT(INOUT) :: this
    DOUBLE PRECISION :: inflow, inflow_old
    DOUBLE PRECISION :: outflow, outflow_old
    DOUBLE PRECISION :: lateral_inflow, lateral_inflow_old
    DOUBLE PRECISION :: storage, storage_old

    ! From whatever hydrodynamics/hydrology is done on this storage.

    this%inpt%hnow%q = inflow   ! current, @ end of hydrodynamic step
    this%inpt%hnow%lateral_inflow = lateral_inflow
    this%outpt%hnow%q = outflow
    this%storage = storage ! NOTE
    
    this%inpt%hold%q = inflow_old   ! previous @ beginning of transport steps
    this%inpt%hold%lateral_inflow = lateral_inflow_old
    this%outpt%hold%q = outflow_old
    this%storage_old = storage_old
    this%my_storage = storage_old
    this%my_storage_old = storage_old

    ! WRITE(*, *) 'compartment_model_pretransport: ', inflow, outflow, storage

  END SUBROUTINE compartment_model_pretransport

  

  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_model_transport_interp
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_model_transport_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    CLASS (compartment_model), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1
    DOUBLE PRECISION :: dlinear_interp


    this%inpt%trans%hold = this%inpt%trans%hnow
    CALL hydro_interp(tnow, htime0, htime1, &
         &this%inpt%hold, this%inpt%hnow, this%inpt%trans%hnow)

    ! Need cross section info here
    CALL this%avgpt%transport_interp(tnow, htime0, htime1)

    ! Interpolate storage
    this%my_storage_old = this%my_storage
    this%my_storage = dlinear_interp(this%storage_old, htime0,&
         &this%storage, htime1, tnow)

    this%outpt%trans%hold = this%outpt%trans%hnow
    CALL hydro_interp(tnow, htime0, htime1, &
         &this%outpt%hold, this%outpt%hnow, this%outpt%trans%hnow)
  END SUBROUTINE compartment_model_transport_interp



  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_model_transport
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_model_transport(this, ispec, cin, clat, &
       &tstep, tdeltat, scalar)

    IMPLICIT NONE

    CLASS (compartment_model), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec, tstep
    DOUBLE PRECISION, INTENT(IN) :: cin, clat
    DOUBLE PRECISION, INTENT(IN) :: tdeltat
    CLASS (link_scalar), INTENT(INOUT) :: scalar

    DOUBLE PRECISION :: inflow, outflow, latflow, snow, sold
    DOUBLE PRECISION :: co, csnow, csold

    this%inpt%trans%cold(ispec) = this%inpt%trans%cnow(ispec)
    this%inpt%trans%cnow(ispec) = cin
    this%outpt%trans%cold(ispec) = this%outpt%trans%cnow(ispec)
    this%avgpt%trans%cold(ispec) = this%avgpt%trans%cnow(ispec)
    
    inflow = this%inpt%trans%hnow%q
    outflow = this%outpt%trans%hnow%q
    latflow = this%inpt%trans%hnow%lateral_inflow
    snow = this%my_storage
    sold = this%my_storage_old

    ! if lateral outflow, just add it to outflow
    IF (latflow .LE. 0.0) THEN
       outflow = outflow - latflow
       latflow = 0.0
    END IF

    csold = this%avgpt%trans%cold(ispec)
    csnow = csold               ! fall back
    
    IF ((tdeltat*outflow + snow) .GT. compartment_min_storage) THEN
       csnow = tdeltat*inflow*cin &
            &+ tdeltat*latflow*clat &
            &+ csold*sold
       csnow = csnow/(tdeltat*outflow+snow)
       co = csnow
    END IF
    ! WRITE(*,*) inflow, outflow, latflow, this%trans_storage_old, this%trans_storage
    ! WRITE(*,*) ci, co, csold, csnow

    this%avgpt%trans%cnow(ispec) = csnow
    this%outpt%trans%cnow(ispec) = co

    ! do scalar specifiec source term, update the storage concentration
    csold = this%avgpt%trans%cnow(ispec)
    csnow = scalar%scalar%source(csold, this%avgpt%trans, tdeltat, scalar%met)
    this%avgpt%trans%cnow(ispec) = csnow
    

  END SUBROUTINE compartment_model_transport

  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_model_conc
  ! Get the current concentrations
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_model_conc(this, ispec, cin, cout, cs)

    IMPLICIT NONE
    CLASS (compartment_model), INTENT(IN) :: this
    INTEGER, INTENT(IN) :: ispec
    DOUBLE PRECISION, INTENT(OUT) :: cin, cout, cs

    cin = this%inpt%trans%cnow(ispec)
    cout = this%outpt%trans%cnow(ispec)
    cs = this%avgpt%trans%cnow(ispec)

  END SUBROUTINE compartment_model_conc


  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_read_restart
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_read_restart(this, iunit, nspecies)

    IMPLICIT NONE
    CLASS (compartment_model), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER, INTENT(IN) :: nspecies

    INTEGER :: s, iostat
    CHARACTER (LEN=1024) :: msg
    DOUBLE PRECISION, DIMENSION(:) :: cnow(nspecies), cold(nspecies)

    READ(iunit, IOSTAT=iostat) &
         &(cnow(s), s = 1, nspecies), &
         &(cold(s), s = 1, nspecies), &
         &this%avgpt%trans%bedtemp

    this%avgpt%trans%cnow = cnow
    this%avgpt%trans%cold = cold
    
    IF (iostat .NE. 0) THEN
       WRITE(msg, *) 'compartment_model: problem reading restart'
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE compartment_read_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_write_restart
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_write_restart(this, iunit, nspecies)

    IMPLICIT NONE

    CLASS (compartment_model), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: iunit
    INTEGER, INTENT(IN) :: nspecies

    INTEGER :: s, iostat
    CHARACTER (LEN=1024) :: msg
    DOUBLE PRECISION, DIMENSION(:) :: cnow(nspecies), cold(nspecies)
    
    cnow = this%avgpt%trans%cnow
    cold = this%avgpt%trans%cold
    
    WRITE(iunit, IOSTAT=iostat) &
         &(cnow(s), s = 1, nspecies), &
         &(cold(s), s = 1, nspecies), &
         &this%avgpt%trans%bedtemp

    IF (iostat .NE. 0) THEN
       WRITE(msg, *) 'compartment_model: problem writing restart'
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE compartment_write_restart

  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_model_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_model_destroy(this)

    IMPLICIT NONE
    CLASS (compartment_model) :: this

    DEALLOCATE(this%inpt%trans%cnow,this%inpt%trans%cold)
    DEALLOCATE(this%outpt%trans%cnow,this%outpt%trans%cold)
    DEALLOCATE(this%avgpt%trans%cnow,this%avgpt%trans%cold)

  END SUBROUTINE compartment_model_destroy



END MODULE compartment_transport_module
