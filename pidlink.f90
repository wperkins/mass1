! ----------------------------------------------------------------
! file: pidlink.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created October 10, 2001 by William A. Perkins
! Last Change: 2017-06-21 13:04:57 d3g096
! ----------------------------------------------------------------


! ----------------------------------------------------------------
! MODULE pidlink
! PID stands for proportional, integral, and differential which is a
! mathematical description of process controllers.  This module
! implements two special links.  The first, type 13, uses the PID
! process control to cause the simulated water surface elevation to
! follow, but deviate as necessary from, observed stage.  The second,
! type 12, follows observed discharge.
! ----------------------------------------------------------------
MODULE pidlink

  IMPLICIT NONE
  CHARACTER (LEN=80), PRIVATE, SAVE :: rcsid = "$Id$"
  CHARACTER (LEN=80), PARAMETER, PRIVATE :: default_filename = "pidlink.dat"
  INTEGER, PARAMETER, PRIVATE :: maxlags = 5

  ! ----------------------------------------------------------------
  ! pidlink_lag_rec
  ! This is used to hold lagged flow/stage information
  ! ----------------------------------------------------------------
  TYPE pidlink_lag_rec

                                ! if usebc is .TRUE. the value of link
                                ! represents a link BC table rather
                                ! than a link
     LOGICAL :: usebc

                                ! link identifies where to get the
                                ! flow from (point 1) and lag is the
                                ! lag time (days)
     INTEGER :: link 
     DOUBLE PRECISION :: lag

                                ! this is used to keep the lagged
                                ! flows in an FIFO queue, only the
                                ! number needed are saved
                                ! (lag/time_step)
     INTEGER :: nlag
     DOUBLE PRECISION, POINTER :: flow(:)
  END TYPE pidlink_lag_rec

  TYPE pidlink_rec
                                ! if .TRUE., the PID error term is
                                ! discharge, rather than stage
     LOGICAL :: followflow

     INTEGER :: link            ! the link number
     DOUBLE PRECISION :: kc, ti, tr         ! constant coefficients
     DOUBLE PRECISION :: errsum             ! integral term
     DOUBLE PRECISION :: oldsetpt

                                ! this is a list of flows to be lagged
     INTEGER :: numflows
     TYPE (pidlink_lag_rec), POINTER :: lagged(:)
  END TYPE pidlink_rec

  INTEGER, PRIVATE, ALLOCATABLE :: linkidmap(:)
  INTEGER, PRIVATE :: npidlink
  TYPE (pidlink_rec), PRIVATE, POINTER :: piddata(:)

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE read_pidlink_info
  ! ----------------------------------------------------------------
  SUBROUTINE read_pidlink_info()

    USE utility
    USE general_vars, ONLY: maxlinks
    USE link_vars, ONLY: linktype
    USE bctable

    IMPLICIT NONE


    INTEGER :: l, link, count, laglink, i
    LOGICAL :: file_exist
    DOUBLE PRECISION :: kc, ti, tr, lagvalues(2*maxlags)
    DOUBLE PRECISION :: lagtime
    CHARACTER (LEN=256) :: fname
    INTEGER :: iounit
    CHARACTER (LEN=1024) :: msg

    fname = default_filename
    iounit = 33

                                ! determine the number of pid type
                                ! links we have in the link data, and
                                ! map the link id's into the array of
                                ! pidlinks

    ALLOCATE(linkidmap(maxlinks))
    linkidmap = 0
    count = 0
    DO l = 1, maxlinks
       IF (linktype(l) .EQ. 13 .OR. linktype(l) .EQ. 12) THEN 
          count = count + 1
          linkidmap(l) = count
       END IF
    END DO

    npidlink = count
    NULLIFY(piddata)

                                ! if there are none, we need not do
                                ! anything else

    IF (count .LE. 0) RETURN

                                ! open and read the pidlink data file

    CALL open_existing(fname, iounit, fatal=.TRUE.)

    ALLOCATE(piddata(count))

    DO l = 1, count
       lagvalues = -999.0
       READ (iounit, *, END=100) link, kc, ti, tr, lagvalues
       IF (linkidmap(link) .EQ. 0) THEN
          WRITE(msg, *) 'error reading pidlink coefficient file ', fname, &
               &' record ', l, ' is for link ', link, &
               &', but link ', link, ' is not the correct type'
          CALL error_message(msg, fatal=.TRUE.)
       END IF

       piddata(l)%followflow = (linktype(link) .EQ. 12)
       piddata(l)%link = link
       piddata(l)%kc = kc
       piddata(l)%ti = ti
       piddata(l)%tr = tr
       piddata(l)%errsum = 0.0

       IF (piddata(l)%followflow) THEN
          WRITE (99,*) 'PID link #', l, ' is link ', piddata(l)%link, ' and follows dischange'
       ELSE
          WRITE (99,*) 'PID link #', l, ' is link ', piddata(l)%link, ' and follows stage'
       END IF
       WRITE (99, *) 'PID Coefficients are as follows: '
       WRITE (99, *) '   Kc = ', piddata(l)%kc
       WRITE (99, *) '   Ti = ', piddata(l)%ti
       WRITE (99, *) '   Tr = ', piddata(l)%tr

                                ! count the number of flows that are
                                ! to be lagged
       
       DO i = 1, maxlags
          IF (lagvalues(i*2 - 1) .LE. -999.0) EXIT
       END DO
       piddata(l)%numflows = i - 1

       IF (piddata(l)%numflows .LE. 0) THEN
          WRITE(msg,*) 'error reading pidlink coefficient file ', TRIM(fname), &
               &' no lagged flows specified for link ', link
          CALL error_message(msg, fatal=.TRUE.)
       ELSE IF (piddata(l)%followflow .AND. piddata(l)%numflows .GT. 1) THEN
          WRITE(msg,*) 'error reading pidlink coefficient file ', TRIM(fname), &
               &' too many lagged stages specified for link ', link
          CALL error_message(msg, fatal=.TRUE.)
       END IF

                                ! make a list of the important
                                ! information for storing lagged flows
       
       ALLOCATE(piddata(l)%lagged(piddata(l)%numflows))

       IF (piddata(l)%followflow) THEN
          WRITE (99,*) 'PID link #', l, 'uses the following lagged stage data: '
       ELSE
          WRITE (99,*) 'PID link #', l, 'uses the following lagged flow data: '
       END IF

       DO i = 1, piddata(l)%numflows

                                ! identify and check the specified link

          laglink = INT(lagvalues(i*2 - 1))
          piddata(l)%lagged(i)%usebc = .FALSE.
          IF (laglink .LT. 0) THEN
             laglink = -laglink
             piddata(l)%lagged(i)%usebc = .TRUE.
             IF (.NOT. bc_table_id_ok(linkbc, laglink)) THEN
                WRITE(msg, *) 'error reading pidlink coefficient file ', TRIM(fname), &
                     &': link ', link, 'uses lagged flow from invalid link BC ', laglink
                CALL error_message(msg, fatal=.TRUE.)
             END IF
          ELSE IF (piddata(l)%followflow) THEN
             WRITE(msg, *) 'error reading pidlink coefficient file ', TRIM(fname), &
                  &' link ', link, ' must specify lagged stage link BC'
             CALL error_message(msg, fatal=.TRUE.)
          ELSE
             IF (laglink .EQ. 0  .OR. laglink .GT. maxlinks) THEN
                WRITE(msg, *) 'error reading pidlink coefficient file ', TRIM(fname), &
                     &' link ', link, 'uses lagged flow from link ', laglink, &
                     &', which is not a valid link '
                CALL error_message(msg, fatal=.TRUE.)
             END IF
          END IF
          piddata(l)%lagged(i)%link = laglink

                                ! check the specified lag

          lagtime = lagvalues(i*2)
          IF (lagtime .LT. 0) THEN
             WRITE (99,*) 'ABORT: error reading pidlink coefficient file ', TRIM(fname)
             WRITE (99,*) 'link ', link, 'uses lagged flow from link ', laglink, &
                  &', but the specified lag (', lagvalues(i*2), ') is invalid '
             WRITE (*,*) 'ABORT: error reading pidlink coefficient file ', TRIM(fname)
             WRITE (*,*) 'link ', link, 'uses lagged flow from link ', laglink, &
                  &', but the specified lag (', lagvalues(i*2), ') is invalid '
             CALL EXIT(1)
          END IF
          piddata(l)%lagged(i)%lag = lagtime

                                ! initialize the remainder of the
                                ! record

          piddata(l)%lagged(i)%nlag = 0
          nullify(piddata(l)%lagged(i)%flow)

          IF (piddata(l)%lagged(i)%usebc) THEN
             WRITE (99,*) '     Link Boundary Condition #', piddata(l)%lagged(i)%link, &
                  &' lagged ', piddata(l)%lagged(i)%lag, ' days'
          ELSE
             WRITE (99,*) '     Point 1 on Link #', piddata(l)%lagged(i)%link, &
                  &' lagged ', piddata(l)%lagged(i)%lag, ' days'
          END IF
       END DO

       WRITE (99,*)

    END DO

    WRITE (99,*)
    WRITE (99,*) 'Reading PID Link data complete.' 
    WRITE (99,*)
    RETURN

                                ! this should be executed when too few
                                ! records are in the input file
100 CONTINUE
    WRITE (99,*) 'ABORT: error reading pidlink coefficient file ', TRIM(fname)
    WRITE (99,*) 'error reading record ', l, ' of ', count, ' expected'
    WRITE (*,*) 'ABORT: error reading pidlink coefficient file ', TRIM(fname)
    WRITE (*,*) 'error reading record ', l, ' of ', count, ' expected'
    CALL EXIT(1)
  END SUBROUTINE read_pidlink_info

  ! ----------------------------------------------------------------
  ! SUBROUTINE pidlink_assemble_lagged
  ! This routine needs to be called after each time step
  ! ----------------------------------------------------------------
  SUBROUTINE pidlink_assemble_lagged()

    USE general_vars, ONLY: time, time_step, time_mult
    USE point_vars, ONLY: q
    USE link_vars, ONLY: linkbc_table
    USE bctable
    IMPLICIT NONE
    TYPE (pidlink_rec), POINTER :: rec

    INTEGER :: i, j, k

    INTEGER :: table_type

    IF (.NOT. ASSOCIATED(piddata)) RETURN

    DO k = 1, npidlink
       rec => piddata(k)

       DO i = 1, rec%numflows

                                ! index 1 holds the oldest flow/stage, get
                                ! rid of it and put the newest at the
                                ! end of the queue

          DO j = 2, rec%lagged(i)%nlag
             rec%lagged(i)%flow(j - 1) = rec%lagged(i)%flow(j)
          END DO

                                ! at this point, we should have made
                                ! sure that stages were specified as
                                ! BC's

          IF (rec%lagged(i)%usebc) THEN
             call bc_table_interpolate(linkbc, rec%lagged(i)%link, time/time_mult)
             rec%lagged(i)%flow = bc_table_current(linkbc, rec%lagged(i)%link, 1)
          ELSE
             rec%lagged(i)%flow(rec%lagged(i)%nlag) = q(rec%lagged(i)%link, 1)
          END IF
       END DO
    END DO

  END SUBROUTINE pidlink_assemble_lagged


  ! ----------------------------------------------------------------
  ! SUBROUTINE pidlink_initialize
  ! This routine does all necessary initialization of the piddata
  ! list.  It must be called after initial conditions have been
  ! applied.
  ! ----------------------------------------------------------------
  SUBROUTINE pidlink_initialize()

    USE general_vars, ONLY: time_begin, time_mult, time_step
    USE link_vars, ONLY: linkbc_table
    USE point_vars, ONLY: q
    USE bctable

    IMPLICIT NONE

    TYPE (pidlink_rec), POINTER :: rec
    INTEGER :: i, j, link

    INTEGER :: table_type

    IF (.NOT. ASSOCIATED(piddata)) RETURN

    DO j = 1, npidlink
       rec => piddata(j)
       link = rec%link
       rec%errsum = 0.0
       call bc_table_interpolate(linkbc, linkbc_table(link), time_begin/time_mult)
       rec%oldsetpt = bc_table_current(linkbc, linkbc_table(link), 1)
       
       DO i = 1, rec%numflows

                                ! allocate a queue and make it just
                                ! the right length: the number of
                                ! time_steps in the lag

          rec%lagged(i)%nlag = MAX(INT(rec%lagged(i)%lag/time_step + 0.5), 1)
          ALLOCATE(rec%lagged(i)%flow(rec%lagged(i)%nlag))

                                ! go ahead and fill the lagged
                                ! flow/stage queue with the initial
                                ! conditions (we should have made sure
                                ! that stages were specified as BC's)

          IF (rec%lagged(i)%usebc) THEN
             call bc_table_interpolate(linkbc, rec%lagged(i)%link, time_begin/time_mult)
             rec%lagged(i)%flow = bc_table_current(linkbc, rec%lagged(i)%link, 1)
          ELSE
             rec%lagged(i)%flow = q(rec%lagged(i)%link, 1)
          END IF

       END DO
    END DO
       
  END SUBROUTINE pidlink_initialize

  ! ----------------------------------------------------------------
  ! DOUBLE PRECISION FUNCTION pidlink_lagged_flow
  ! This just adds up the lagged flows in the list
  ! ----------------------------------------------------------------
  DOUBLE PRECISION FUNCTION pidlink_lagged_flow(rec)

    IMPLICIT NONE
    TYPE (pidlink_rec) :: rec
    DOUBLE PRECISION :: time

    INTEGER :: i

    pidlink_lagged_flow = 0.0
  
    DO i = 1, rec%numflows
       pidlink_lagged_flow = pidlink_lagged_flow + rec%lagged(i)%flow(1)
    END DO

  END FUNCTION pidlink_lagged_flow


  ! ----------------------------------------------------------------
  ! SUBROUTINE pidlink_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE pidlink_coeff(link, point, setpt, a, b, c, d, g, ap, bp, cp, dp, gp)

    USE general_vars, ONLY: time, time_step
    USE point_vars, ONLY: q, y
    USE date_vars, ONLY: date_string, time_string

    IMPLICIT NONE
    INTEGER, INTENT(IN) :: link, point
    DOUBLE PRECISION, INTENT(IN) :: setpt
    DOUBLE PRECISION, INTENT(OUT) ::  a, b, c, d, g, ap, bp, cp, dp, gp
    TYPE (pidlink_rec), POINTER :: rec

    INTEGER :: table_type

    DOUBLE PRECISION :: lag, eval, lval

    rec => piddata(linkidmap(link))

                                ! continuity is the same in all cases

    a = 0.0
    b = 1.0
    c = 0.0
    d = 1.0
    g = q(link, point) - q(link, point + 1)

    rec%errsum = rec%errsum + (y(link, point) -  rec%oldsetpt)*time_step
    lag = pidlink_lagged_flow(rec)

                                ! momentum coefficients 
    
    ap = 0.0
    bp = 0.0


    IF (rec%followflow) THEN
                                ! when using discharge as the error term

       cp = -1.0
       eval = q(link, point)
       lval = y(link, point)
       IF (rec%ti .GT. 0.0) THEN
          dp = rec%kc*(1.0 + time_step/rec%ti + rec%tr/time_step)
       ELSE
          dp = rec%kc*(1.0 + rec%tr/time_step)
       END IF

    ELSE
                                ! when using stage as the error term

       dp = -1.0
       eval = y(link, point)
       lval = q(link, point)
       IF (rec%ti .GT. 0.0) THEN
          cp = rec%kc*(1.0 + time_step/rec%ti + rec%tr/time_step)
       ELSE
          cp = rec%kc*(1.0 + rec%tr/time_step)
       END IF

    END IF

    IF (rec%ti .GT. 0.0) THEN
       gp = lag - lval + &
            & rec%kc*eval*(1.0 + time_step/rec%ti) - &
            & rec%kc*setpt*(1.0 + time_step/rec%ti + rec%tr/time_step) + &
            & rec%kc/rec%ti*rec%errsum + rec%kc*rec%tr/time_step*rec%oldsetpt
    ELSE
       gp = lag - lval + &
            & rec%kc*eval - &
            & rec%kc*setpt*(1.0 + rec%tr/time_step) + &
            & rec%kc*rec%tr/time_step*rec%oldsetpt
    END IF
       

    rec%oldsetpt = setpt

    WRITE (1,100) date_string, time_string, link, point, y(link, point), q(link, point), setpt, rec%oldsetpt, lag, rec%errsum
100 FORMAT(A10, 1X, A8, 2(1X,I5), 6(1X,F10.2))
  END SUBROUTINE pidlink_coeff


END MODULE pidlink






