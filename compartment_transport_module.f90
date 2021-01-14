! ----------------------------------------------------------------
! file: compartment_transport_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created December 15, 2020 by  William Perkins 
! Last Change: 2021-01-14 12:28:43 d3g096
! ----------------------------------------------------------------

! ----------------------------------------------------------------
! MODULE compartment_transport_module
! ----------------------------------------------------------------
MODULE compartment_transport_module

  USE utility
  USE point_module
  USE scalar_module

  IMPLICIT NONE

  ! ----------------------------------------------------------------
  ! TYPE compartment_model
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: compartment_model

     ! for storing input, output, and storage conditions, because it's easier
     TYPE (point_t) :: inpt, outpt, avgpt

   CONTAINS
     PROCEDURE :: pre_transport => compartment_model_pretransport
     PROCEDURE :: trans_interp => compartment_model_transport_interp
     PROCEDURE :: transport => compartment_model_transport
     PROCEDURE :: conc => compartment_model_conc
     PROCEDURE :: read_restart => compartment_read_restart
     PROCEDURE :: write_restart => compartment_write_restart
  END type compartment_model
  
  DOUBLE PRECISION, PRIVATE, PARAMETER :: compartment_min_storage = 1.0D-05

CONTAINS

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
    this%avgpt%hnow%q = storage ! NOTE
    
    this%inpt%hold%q = inflow_old   ! previous @ beginning of transport steps
    this%inpt%hold%lateral_inflow = lateral_inflow_old
    this%outpt%hold%q = outflow_old
    this%avgpt%hold%q = storage_old

  END SUBROUTINE compartment_model_pretransport

  

  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_model_transport_interp
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_model_transport_interp(this, tnow, htime0, htime1)

    IMPLICIT NONE
    CLASS (compartment_model), INTENT(INOUT) :: this
    DOUBLE PRECISION, INTENT(IN) :: tnow, htime0, htime1

    CALL this%inpt%transport_interp(tnow, htime0, htime1)
    CALL this%avgpt%transport_interp(tnow, htime0, htime1)
    CALL this%outpt%transport_interp(tnow, htime0, htime1)

  END SUBROUTINE compartment_model_transport_interp



  ! ----------------------------------------------------------------
  ! SUBROUTINE compartment_model_transport
  ! ----------------------------------------------------------------
  SUBROUTINE compartment_model_transport(this, ispec, cin, clat, &
       &tstep, tdeltat, species, met)

    IMPLICIT NONE

    CLASS (compartment_model), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: ispec, tstep
    DOUBLE PRECISION, INTENT(IN) :: cin, clat
    DOUBLE PRECISION, INTENT(IN) :: tdeltat
    CLASS (scalar_t), INTENT(INOUT), POINTER :: species
    CLASS (met_zone_t), INTENT(INOUT), POINTER :: met

    DOUBLE PRECISION :: inflow, outflow, latflow, snow, sold
    DOUBLE PRECISION :: co, csnow, csold

    this%inpt%trans%cold(ispec) = this%inpt%trans%cnow(ispec)
    this%inpt%trans%cnow(ispec) = cin
    this%outpt%trans%cold(ispec) = this%outpt%trans%cnow(ispec)
    this%avgpt%trans%cold(ispec) = this%avgpt%trans%cnow(ispec)
    
    inflow = this%inpt%trans%hnow%q
    outflow = this%outpt%trans%hnow%q
    latflow = this%inpt%trans%hnow%lateral_inflow
    snow = this%avgpt%trans%hnow%q
    sold = this%avgpt%trans%hold%q

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
    csnow = species%source(csold, this%avgpt%trans, tdeltat, met)
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
    
    READ(iunit, IOSTAT=iostat) &
         &(this%avgpt%trans%cnow(s), s = 1, nspecies), &
         &(this%avgpt%trans%cold(s), s = 1, nspecies), &
         &this%avgpt%trans%bedtemp

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
    
    WRITE(iunit, IOSTAT=iostat) &
         &(this%avgpt%trans%cnow(s), s = 1, nspecies), &
         &(this%avgpt%trans%cold(s), s = 1, nspecies), &
         &this%avgpt%trans%bedtemp

    IF (iostat .NE. 0) THEN
       WRITE(msg, *) 'compartment_model: problem writing restart'
       CALL error_message(msg, fatal=.TRUE.)
    END IF

  END SUBROUTINE compartment_write_restart


END MODULE compartment_transport_module
