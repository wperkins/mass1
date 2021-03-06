!***************************************************************
! Copyright (c) 2017 Battelle Memorial Institute
! Licensed under modified BSD License. A copy of this license can be
! found in the LICENSE file in the top level directory of this
! distribution.
!***************************************************************
!
! NAME:  MASS1 scalars module file
!
! VERSION and DATE: 0.84 08-15-99
!
! PURPOSE:  header file for MASS2 model
!
! RETURNS:
!
! REQUIRED:
!
! LOCAL VARIABLES:
!
! COMMENTS:
!
! MOD HISTORY: 10-5-98 added to MASS1 from MASS2
!							fixed delta x bug in diffusion calc if you reverse distance
!							reference frame; mcr 10-10-98
!              august 99 - added direct internal bc input, overides upstream C if active
!
!***************************************************************
!
MODULE scalars
  USE utility
  IMPLICIT NONE

  INTEGER  :: max_species = 2

  TYPE scalar_struct
     DOUBLE PRECISION, POINTER :: conc(:,:)       ! c depth-ave concentration
     DOUBLE PRECISION, POINTER :: concold(:,:)    ! c old depth-ave concentration
  END TYPE scalar_struct

  TYPE(scalar_struct), ALLOCATABLE :: species(:)

  ! these are used for transport
  ! sub-time-stepping

  DOUBLE PRECISION :: scalar_time, scalar_delta_t
  DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE, SAVE, PRIVATE :: vel, area, width, area_old, q, q_old, y, y_old
  DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE, SAVE, PRIVATE :: latq, latq_old

  ! air-water gas exchange coefficient parameters
  DOUBLE PRECISION :: gasx_a = 0.0, gasx_b = 0.0 , gasx_c = 0.0, gasx_d = 0.0

  DOUBLE PRECISION, PRIVATE, PARAMETER :: max_courant = 0.99
  DOUBLE PRECISION, PRIVATE, PARAMETER :: max_diffuse = 0.99

CONTAINS
  SUBROUTINE allocate_species_components(ispec, maxlinks, maxpoint)
    ! this routine allocates each component in the array of blocks
    ! allows minimal memory use for each block
    IMPLICIT NONE
    INTEGER :: ispec, maxlinks, maxpoint, alloc_stat
    CHARACTER (LEN=1024) :: msg

    WRITE(msg,*)'starting component allocation for species number - ', ispec
    CALL status_message(msg)
    CALL status_message("")
    WRITE(msg,*)'         number of links = ', maxlinks
    CALL status_message(msg)
    WRITE(msg,*)'         number of points = ', maxpoint
    CALL status_message(msg)

    ALLOCATE(species(ispec)%conc(maxlinks,0:maxpoint+2), STAT = alloc_stat)
    IF(alloc_stat /= 0)THEN
       CALL error_message('allocation failed for the concentration', fatal=.TRUE.)
    ELSE
       CALL status_message('allocation successful for concentration')
    ENDIF

    ALLOCATE(species(ispec)%concold(maxlinks,0:maxpoint+2), STAT = alloc_stat)
    IF(alloc_stat /= 0)THEN
       CALL error_message('allocation failed for the old concentration', fatal=.TRUE.)
    ELSE
       CALL status_message('allocation successful for old concentration')
    ENDIF

    WRITE(msg,*) 'completed component allocation for species number - ', ispec
    CALL status_message(msg)

  END SUBROUTINE allocate_species_components

  !#####################################################################################################

  SUBROUTINE allocate_species()

    USE general_vars, ONLY: maxlinks, maxpoint

    IMPLICIT NONE
    CHARACTER (LEN=1024) :: msg
    INTEGER :: alloc_stat

    ALLOCATE(species(max_species), STAT = alloc_stat)
    IF(alloc_stat /= 0)THEN
       WRITE(msg,*) 'allocation failed for the array of species - max_species=', max_species
       CALL error_message(msg, fatal=.TRUE.)
    ELSE
       WRITE(msg,*) 'allocation successful for array of species - max_species=', max_species
       CALL status_message(msg)
    ENDIF

    ! allocate module PRIVATE hydrodynamic
    ! variables

    ALLOCATE(vel(maxlinks,maxpoint), width(maxlinks,maxpoint), &
         &area(maxlinks,maxpoint), area_old(maxlinks,maxpoint),&
         &q(maxlinks,maxpoint), q_old(maxlinks,maxpoint), &
         &y(maxlinks,maxpoint), y_old(maxlinks,maxpoint), &
         &latq(maxlinks,maxpoint), latq_old(maxlinks,maxpoint), STAT = alloc_stat)
    IF(alloc_stat /= 0)THEN
       CALL error_message('allocation failed for scalar hydrodynamic variables', fatal=.TRUE.)
    ELSE
       CALL status_message('allocation successful for scalar hydrodynamic variables')
    ENDIF

  END SUBROUTINE allocate_species

  ! ----------------------------------------------------------------
  ! INTEGER FUNCTION tvd_steps
  !
  ! This computes the number of transport steps that should be taken in
  ! this hydrodynamic time step (delta_t) in order satisfy Courant
  ! number constraints.  The maximum courant number is used to
  ! determine.  The transport time step is computed so that the maximum
  ! Courant number is reduced to an exceptable target.
  !
  ! The transport time step is also adjusted for diffusion stability.
  ! ----------------------------------------------------------------
  INTEGER FUNCTION tvd_steps(delta_t)

    USE point_vars, ONLY: courant_num, diffuse_num

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: delta_t
    DOUBLE PRECISION :: cmax, dmax, a_transdt, d_transdt

    cmax = MAXVAL(courant_num)

    IF (cmax .GT. max_courant) THEN 
       a_transdt = max_courant/cmax*delta_t
    ELSE 
       a_transdt = delta_t
    END IF

    dmax = MAXVAL(diffuse_num) 
    IF (dmax .GT. max_diffuse) THEN 
       d_transdt = max_diffuse/dmax*delta_t
    ELSE 
       d_transdt = delta_t
    END IF

    a_transdt = MIN(a_transdt, d_transdt)

    tvd_steps = FLOOR(delta_t/a_transdt)

    IF (FRACTION(delta_t/a_transdt) .GT. EPSILON(a_transdt)) &
         &tvd_steps = tvd_steps + 1

    tvd_steps = MAX(tvd_steps, 1)

  END FUNCTION tvd_steps


  ! ----------------------------------------------------------------
  ! SUBROUTINE tvd_interp
  ! This routine is used to interpolate the hydrodynamics for the
  ! current transport time step and store them in the PRIVATE module
  ! variables
  ! ----------------------------------------------------------------
  SUBROUTINE tvd_interp(time, htime0, htime1)

    USE general_vars, ONLY: maxlinks, htime=>time, htime_begin=>time_begin
    USE link_vars, ONLY: maxpoints, linktype
    USE point_vars, ONLY: thalweg, &
         &harea=>area, harea_old=>area_old, hq=>q, hq_old=>q_old, &
         &hvel=>vel, hy=>y, hy_old=>y_old, hlatq=>lateral_inflow, &
         &hlatq_old=>lateral_inflow_old

    IMPLICIT NONE

    DOUBLE PRECISION :: time, htime0, htime1
    DOUBLE PRECISION :: val, val0, val1
    INTEGER :: link, point
    EXTERNAL dlinear_interp
    DOUBLE PRECISION dlinear_interp

    IF (time .eq. htime_begin) THEN
       DO link = 1, maxlinks
          y(link,:) = hy_old(link,:)
          area(link,:) = harea_old(link,:)
          q(link,:) = hq_old(link,:)
          latq(link,:) = hlatq_old(link,:)
       END DO
    END IF

    DO link = 1, maxlinks
       area_old(link,:) = area(link,:)
       q_old(link,:) = q(link,:)
       y_old(link,:) = y(link,:)
       latq_old(link,:) = latq(link,:)
       DO point = 1, maxpoints(link)
          val0 = hq_old(link, point)
          val1 = hq(link, point)
          val = dlinear_interp(val0, htime0, val1, htime1, time)
          q(link,point) = val

          val0 = hlatq_old(link, point)
          val1 = hlatq(link, point)
          val = dlinear_interp(val0, htime0, val1, htime1, time)
          latq(link,point) = val

          SELECT CASE (linktype(link))
          CASE(1,20,21)              ! do fluvial links only
             val0 = hy_old(link, point)
             val1 = hy(link, point)
             val = dlinear_interp(val0, htime0, val1, htime1, time)
             y(link,point) = val

             val = y(link,point) - thalweg(link,point)
             CALL section(link, point, val, area(link,point), width(link,point), &
                  &val0, val0, val0)

             ! val0 = DBLE(harea_old(link, point))
             ! val1 = DBLE(harea(link, point))
             ! val = dlinear_interp(val0, htime0, val1, htime1, time)
             ! area(link,point) = SNGL(val)

             vel(link, point) = q_old(link,point)/area_old(link,point)
          END SELECT
       END DO
    END DO
  END SUBROUTINE tvd_interp
  !###################################################################################

  !----------------------------------------------------------------------------------
  ! uses EXPLICIT tvd scheme and split operator method
  ! needs "false" points before and after start and endpoints of link
  !
  ! species = 1 is Total Dissolved Gas
  ! species = 2 is Ave. Water Temperature
  ! 
  !----------------------------------------------------------------------------------
  SUBROUTINE tvd_transport(species_num, c, c_old,status_iounit, error_iounit)

    USE transport_vars , ONLY : k_surf, dxx
    USE general_vars, ONLY : maxlinks, maxpoint, time_mult, time_begin
    USE link_vars, ONLY : maxpoints, comporder, linktype, num_con_links, con_links,&
         &linkbc_table, met_zone, tempbc_table, transbc_table, &
         &lattempbc_table, lattransbc_table
    USE point_vars, ONLY: x, hy=>y, k_diff, thalweg
    USE logicals
    USE bctable


    USE met_data_module
    USE energy_flux
    USE tdg_equation_coeff
    USE gas_functions
    USE hydro_output_module

    IMPLICIT NONE

    INTEGER :: species_num
    INTEGER :: status_iounit, error_iounit
    DOUBLE PRECISION :: c(:,0:), c_old(:,0:)

    INTEGER :: i,j,link,point
    DOUBLE PRECISION :: velave,sum,cflx,s,corr,tmp,phi,dtdx,ave_vel
    DOUBLE PRECISION :: f(maxpoint)
    DOUBLE PRECISION :: k_e,k_w,area_e,area_w,flux_e,flux_w
    DOUBLE PRECISION :: qspill,qgen 
    DOUBLE PRECISION :: energy_source, depth, transfer_coeff
    DOUBLE PRECISION :: tdg_saturation = 100.0, upstream_c
    DOUBLE PRECISION :: avg_area, avg_latq
    DOUBLE PRECISION :: salinity = 0.0, ccstar
    DOUBLE PRECISION :: met_coeff(met_ncoeff)

    LOGICAL :: diffusion, fluvial, nonfluvial

    DOUBLE PRECISION :: time, delta_t

    time = scalar_time
    delta_t = scalar_delta_t

    !WRITE(*,*)'done with alloc; link loop'
    !WRITE(*,*)c(1,0),c(1,151),dxx(1,0),dxx(1,151)
    ! run through links from top to bottom

    links_forward: DO i=1,maxlinks

       link = comporder(i)

       !fluvial or nonfluvial link
       SELECT CASE(linktype(link))
       CASE(1,20,21)
          fluvial = .TRUE.
          nonfluvial = .FALSE.
       CASE(2,3,4,5,6,7,13)
          fluvial = .FALSE.
          nonfluvial = .TRUE.
       END SELECT
       !----------------------------------------------------------------------------
       !do nonfluvial first just pass through concentrations
       ! 
       !IF(linktype(link) /= 1 )THEN
       IF( nonfluvial )THEN
          sum = 0.0
          point = 1
          DO j=1,num_con_links(link)
             sum = sum + &
                  &q(con_links(link,j),maxpoints(con_links(link,j)))*&
                  &c(con_links(link,j),maxpoints(con_links(link,j)))
          END DO
          c(link,point) = sum/q(link,point)
!!$        DO j=1,num_con_links(link)
!!$           c(link,1) =  c(con_links(link,j),maxpoints(con_links(link,j)))
!!$        END DO

          IF((linktype(i) == 6) .AND. (species_num == 1))THEN

             call bc_table_interpolate(hydrobc, linkbc_table(link), time/time_mult)
             qgen = bc_table_current(hydrobc, linkbc_table(link), 1)
             qspill = bc_table_current(hydrobc, linkbc_table(link), 2)

             IF(do_temp .AND. temp_exchange) CALL update_met_data(time, met_zone(link))
             IF(do_temp) t_water = species(2)%conc(link,2)
             IF(qspill > 0.0)THEN
                !			equations are for %Sat and effective Spill Q in Kcfs
                !			Qspill is in effective KCFS = Qspill + qgen_frac*Qgen
                !			gas_eqn_type = 1 general quadratic function
                !			tdg = a_gas + b_gas*Qspill + c_gas*Qspill**2
                !			OR 
                !			gas_eqn_type = 2 exponetial equation
                !			tdg = a_gas + b_gas*EXP(c_gas*Qspill)
                !			OR 
                !			gas_eqn_type = 3 exponetial equation
                !			tdg = a_gas + b_gas*EXP(c_gas*Qspill)
                !			OR 
                !			gas_eqn_type = 4 logaritmic equation
                !			tdg = a_gas + b_gas*LOG(c_gas*Qspill)

                qspill = qspill + qgen_frac(link)*qgen
                qgen   = qgen - qgen_frac(link)*qgen

                SELECT CASE(gas_eqn_type(link))

                CASE(1)
                   tdg_saturation = a_gas(link) + b_gas(link)*qspill/1000.0 + c_gas(link)*(qspill/1000.0)**2 
                   c(link,2) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                CASE(2)
                   tdg_saturation = a_gas(link) + b_gas(link)*EXP(c_gas(link)*qspill/1000.0)
                   c(link,2) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                CASE(3)
                   tdg_saturation = a_gas(link) + b_gas(link)*EXP(c_gas(link)*qspill/1000.0)
                   c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                CASE(4) 
                   tdg_saturation = a_gas(link) + b_gas(link)*LOG(c_gas(link)*qspill/1000.0)
                   c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                CASE DEFAULT
                   WRITE(*,*)'ABORT - no gas eqn error in type ', linktype(link), ' link BCs at link = ',link
                   WRITE(99,*)'ABORT - no gas eqn error in type ', linktype(link), ' link BCs at link = ',link
                   CALL EXIT(1)
                END SELECT
                hydro_sat(link) = tdg_saturation
                hydro_conc(link) = c(link,2)
             ELSE
                hydro_sat(link) = 100.0
                hydro_conc(link) = TDGasConcfromSat(hydro_sat(link), t_water, salinity, baro_press)
             ENDIF
             hydro_temp(link) = t_water
             hydro_baro(link) = baro_press
             hydro_gen(link) = qgen
             hydro_spill(link) = qspill
             hydro_disch(link) = q(link, 1)

             IF (qspill + qgen .gt. 0.0) THEN
                ! full mixing of spill and generation waters
                c(link,2) = (c(link,2)*qspill + c(link,1)*qgen)/(qspill+qgen)
             ELSE
                c(link,2) = TDGasConcfromSat( DBLE(100.0), t_water, &
                     &salinity, baro_press)              
             END IF

          ELSE
             c(link,2) =	c(link,1)
          ENDIF


          !------ do fluvial links --------------------------------------
       ELSE


          !set distance difference array
          IF(time == time_begin)THEN
             point = 1
             dxx(link,1) = ABS(x(link,point+1) - x(link,point))
             dxx(link,0) = dxx(link,1)
             DO point=2,maxpoints(link)-1

                dxx(link,point) = ABS(0.5*(x(link,point) - x(link,point-1))) +  &
                     ABS(0.5*(x(link,point+1) - x(link,point)))
             END DO
             dxx(link,maxpoints(link)) =	ABS(x(link,maxpoints(link)) - x(link,maxpoints(link)-1))
             dxx(link,maxpoints(link)+1) = dxx(link,maxpoints(link)) 
             dxx(link,maxpoints(link)+2) = dxx(link,maxpoints(link))
          ENDIF

          ! set upstream influx for the link from c(t) bc table or junction conditions
          point = 1

          SELECT CASE(species_num)
          CASE(1) ! gas species
             IF(do_temp .AND. temp_exchange) CALL update_met_data(time, met_zone(link))
             IF(do_temp)THEN
                IF((num_con_links(link) == 0) .AND. (tempbc_table(link) /= 0))THEN
                   call bc_table_interpolate(tempbc, tempbc_table(link), time/time_mult)
                   t_water = bc_table_current(tempbc, tempbc_table(link), 1)
                ELSE
                   t_water = species(2)%conc(link,1)
                ENDIF
             ENDIF

             IF((num_con_links(link) == 0) .AND. (transbc_table(link) /= 0))THEN
                SELECT CASE(linktype(link))
                CASE(1)
                   call bc_table_interpolate(transbc, transbc_table(link), time/time_mult)
                   c(link,point) = bc_table_current(transbc, transbc_table(link), 1)

                CASE(20) ! %TDG Saturation is specified
                   call bc_table_interpolate(transbc, transbc_table(link), time/time_mult)
                   upstream_c = bc_table_current(transbc, transbc_table(link), 1)
                   c(link,point) = TDGasConcfromSat(upstream_c, t_water, salinity, baro_press)


                   !-------------------------------------------------------------------
                CASE(21) ! Hydro project inflow link
                   call bc_table_interpolate(hydrobc, linkbc_table(link), time/time_mult)
                   qgen = bc_table_current(hydrobc, linkbc_table(link), 1)
                   qspill = bc_table_current(hydrobc, linkbc_table(link), 2)

                   IF(qspill > 0.0)THEN

                      !			equations are for %Sat and effective Spill Q in Kcfs
                      !			Qspill is in effective KCFS = Qspill + qgen_frac*Qgen
                      !			gas_eqn_type = 2 general quadratic function
                      !			tdg = a_gas + b_gas*Qspill + c_gas*Qspill**2
                      !			OR 
                      !			gas_eqn_type = 3 exponetial equation
                      !			tdg = a_gas + b_gas*EXP(c_gas*Qspill)
                      !			OR 
                      !			gas_eqn_type = 4 logaritmic equation
                      !			tdg = a_gas + b_gas*LOG(c_gas*Qspill)

                      qspill = qspill + qgen_frac(link)*qgen
                      qgen   = qgen - qgen_frac(link)*qgen

                      SELECT CASE(gas_eqn_type(link))
                      CASE(2)
                         tdg_saturation = a_gas(link) + b_gas(link)*qspill/1000.0 + c_gas(link)*(qspill/1000.0)**2 
                         c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                      CASE(3)
                         tdg_saturation = a_gas(link) + b_gas(link)*EXP(c_gas(link)*qspill/1000.0)
                         c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                      CASE(4) 
                         tdg_saturation = a_gas(link) + b_gas(link)*LOG(c_gas(link)*qspill/1000.0)
                         c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                      CASE DEFAULT
                         WRITE(*,*)'ABORT - no eqn error in type 21 link BCs at link = ',link
                         WRITE(99,*)'ABORT - no eqn error in type 21 link BCs at link = ',link
                         CALL EXIT(1)
                      END SELECT
                      hydro_sat(link) = tdg_saturation
                      hydro_conc(link) = c(link,point)
                   ELSE
                      hydro_sat(link) = 100.0
                      hydro_conc(link) = TDGasConcfromSat(hydro_sat(link), t_water, salinity, baro_press)
                   ENDIF

                   hydro_temp(link) = t_water
                   hydro_baro(link) = baro_press
                   hydro_gen(link) = qgen
                   hydro_spill(link) = qspill
                   hydro_disch(link) = q(link, 1)

                   call bc_table_interpolate(transbc, transbc_table(link), time/time_mult)
                   upstream_c = bc_table_current(transbc, transbc_table(link), 1)
                   upstream_c = TDGasConcfromSat(upstream_c, t_water, salinity, baro_press)

                   IF (qspill + qgen .gt. 0.0) THEN
                      ! full mixing of spill and generation waters
                      c(link,point) = (c(link,point)*qspill + upstream_c*qgen)/(qspill+qgen)
                   ELSE 
                      c(link,point) = TDGasConcfromSat( DBLE(100.0), t_water, &
                           &salinity, baro_press)              
                   END IF

                   !-----hydro inflow end

                END SELECT
                !
                ! pure internal connection between fluvial links - just mix and pass through
             ELSE IF((num_con_links(link) /= 0) .AND. (transbc_table(link) == 0)) THEN
                sum = 0.0
                DO j=1,num_con_links(link)
                   sum = sum + q(con_links(link,j),maxpoints(con_links(link,j)))*c(con_links(link,j),maxpoints(con_links(link,j)))
                   c(link,point) = sum/q(link,point)
                END DO

                ! internal fluvial link that has an active table BC
             ELSE IF((num_con_links(link) /= 0) .AND. (transbc_table(link) /= 0)) THEN ! internal link with table spec

                SELECT CASE(linktype(link))
                CASE(1) ! % internal C (mg/L) specified
                   call bc_table_interpolate(transbc, transbc_table(link), time/time_mult)
                   c(link,point) = bc_table_current(transbc, transbc_table(link), 1)

                CASE(20) ! internal %TDG Saturation is specified
                   call bc_table_interpolate(transbc, transbc_table(link), time/time_mult)
                   upstream_c = bc_table_current(transbc, transbc_table(link), 1)
                   c(link,point) = TDGasConcfromSat(upstream_c, t_water, salinity, baro_press)

                CASE(21) ! Hydro project inflow for an internal link
                   call bc_table_interpolate(hydrobc, linkbc_table(link), time/time_mult)
                   qgen = bc_table_current(hydrobc, linkbc_table(link), 1)
                   qspill = bc_table_current(hydrobc, linkbc_table(link), 2)

                   IF(qspill > 0.0)THEN

                      !			equations are for %Sat and effective Spill Q in Kcfs
                      !			Qspill is in effective KCFS = Qspill + qgen_frac*Qgen
                      !           gas_eqn_type = 0 No eqn given - read specified gas Conc (mg/L)
                      !           gas_eqn_type = 1 No eqn given - read specified gas %Saturation
                      !			gas_eqn_type = 2 general quadratic function
                      !			tdg = a_gas + b_gas*Qspill + c_gas*Qspill**2
                      !			OR 
                      !			gas_eqn_type = 3 exponetial equation
                      !			tdg = a_gas + b_gas*EXP(c_gas*Qspill)
                      !			OR 
                      !			gas_eqn_type = 4 logaritmic equation
                      !			tdg = a_gas + b_gas*LOG(c_gas*Qspill)

                      qspill = qspill + qgen_frac(link)*qgen
                      qgen   = qgen - qgen_frac(link)*qgen

                      SELECT CASE(gas_eqn_type(link))
                      CASE(0)
                         call bc_table_interpolate(transbc, transbc_table(link), time/time_mult)
                         c(link,point) = bc_table_current(transbc, transbc_table(link), 1)
                         hydro_conc(link) = c(link,point)
                         hydro_sat(link) = TDGasSaturation(hydro_conc(link), t_water, salinity, baro_press)
                      CASE(1)
                         call bc_table_interpolate(transbc, transbc_table(link), time/time_mult)
                         tdg_saturation = bc_table_current(transbc, transbc_table(link), 1)
                         hydro_conc(link) = c(link,point)
                         hydro_sat(link) = tdg_saturation
                      CASE(2)
                         tdg_saturation = a_gas(link) + b_gas(link)*qspill/1000.0 + c_gas(link)*(qspill/1000.0)**2 
                         c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                         hydro_conc(link) = c(link,point)
                         hydro_sat(link) = tdg_saturation
                      CASE(3)
                         tdg_saturation = a_gas(link) + b_gas(link)*EXP(c_gas(link)*qspill/1000.0)
                         c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                         hydro_conc(link) = c(link,point)
                         hydro_sat(link) = tdg_saturation
                      CASE(4) 
                         tdg_saturation = a_gas(link) + b_gas(link)*LOG(c_gas(link)*qspill/1000.0)
                         c(link,point) = TDGasConcfromSat(tdg_saturation, t_water, salinity, baro_press)
                         hydro_conc(link) = c(link,point)
                         hydro_sat(link) = tdg_saturation
                      CASE DEFAULT
                         WRITE(*,*)'ABORT - no eqn error in type 21 link BCs at link = ',link
                         WRITE(99,*)'ABORT - no eqn error in type 21 link BCs at link = ',link
                         CALL EXIT(1)
                      END SELECT
                   ELSE
                      hydro_sat(link) = 100.0
                      hydro_conc(link) = TDGasConcfromSat(hydro_sat(link), t_water, salinity, baro_press)
                   ENDIF

                   hydro_gen(link) = qgen
                   hydro_temp(link) = t_water
                   hydro_baro(link) = baro_press
                   hydro_spill(link) = qspill
                   hydro_disch(link) = q(link, 1)

                   sum = 0.0
                   DO j=1,num_con_links(link)
                      sum = sum + q(con_links(link,j),maxpoints(con_links(link,j)))*&
                           &c(con_links(link,j),maxpoints(con_links(link,j)))
                      upstream_c = sum/q(link,point)
                   END DO

                   IF (qspill + qgen .gt. 0.0) THEN
                      ! full mixing of spill and generation waters
                      c(link,point) = (c(link,point)*qspill + upstream_c*qgen)/(qspill+qgen)
                   ELSE 
                      c(link,point) = TDGasConcfromSat( DBLE(100.0), t_water, &
                           &salinity, baro_press)
                   END IF

                   !-----hydro inflow end

                END SELECT
             ELSE 
                WRITE(*,*)'no trans BC specification for link',link,' -- ABORT'
                WRITE(99,*)'no trans BC specification for link',link,' -- ABORT'
                CALL EXIT(1)
             ENDIF

          CASE(2) ! temperature species
             IF((num_con_links(link) == 0) .AND. (tempbc_table(link) /= 0))THEN
                call bc_table_interpolate(tempbc, tempbc_table(link), time/time_mult)
                c(link,point) = bc_table_current(tempbc, tempbc_table(link), 1)
             ELSE IF((num_con_links(link) /= 0) .AND. (tempbc_table(link) == 0)) THEN! internal link
                sum = 0.0
                DO j=1,num_con_links(link)
                   sum = sum + q(con_links(link,j),maxpoints(con_links(link,j)))*c(con_links(link,j),maxpoints(con_links(link,j)))
                   c(link,point) = sum/q(link,point)
                END DO
             ELSE IF((num_con_links(link) /= 0) .AND. (tempbc_table(link) /= 0)) THEN! internal link with table spec
                call bc_table_interpolate(tempbc, tempbc_table(link), time/time_mult)
                c(link,point) = bc_table_current(tempbc, tempbc_table(link), 1)
             ELSE 
                WRITE(*,*)'no temp BC specification for link',link,' -- ABORT'
                WRITE(99,*)'no temp BC specification for link',link,' -- ABORT'
                CALL EXIT(1)
             ENDIF
          END SELECT

          c(link,point-1) = c(link,point)
          c_old(link,point) = c(link,point)
          c_old(link,point-1) = c_old(link,point)

          ! compute fluxes



          DO point = 1,maxpoints(link)-1

             IF(point == maxpoints(link))THEN
                velave = q_old(link,point)
                ave_vel = vel(link,point)
                !velave = 0.5*(q_old(link,point)+q_old(link,point-1))
                !ave_vel = 0.5*(vel(link,point)+vel(link,point-1))
             ELSE
                !velave = q_old(link,point)
                !ave_vel = vel(link,point)

                velave = 0.5*(q_old(link,point)+q_old(link,point+1))
                ave_vel = 0.5*(vel(link,point)+vel(link,point+1))      
             ENDIF

             !velave = 1.0

             cflx =  ave_vel*delta_t/dxx(link,point)
             !cflx = 0.0

             if(velave.ge.0)then   !!! +ve velocity case 
                f(point)=c(link,point+1)-c(link,point)
                if(abs(f(point)).gt.1.d-40)then
                   corr = 0.5*(dxx(link,point+1)+dxx(link,point))/	 &
                        (0.5*(dxx(link,point)+dxx(link,point-1)))
                   s = corr *	&
                        (c(link,point)-c(link,point-1))/f(point)
                   tmp = min(2.d0,2.d0*s,0.33333333333333333d0*	 &
                        (2.d0-cflx+(1.d0+cflx)*s))
                   phi = max(0.0,tmp)
                else
                   phi=0
                end if
                f(point)=velave*	 &
                     ( c(link,point) + 0.5*(1.0-cflx)*phi*f(point) )

             else        !!! -ve velocity case
                f(point)=c(link,point)-c(link,point+1)
                if(abs(f(point)).gt.1.d-40)then
                   corr = 0.5*(dxx(link,point+1)+dxx(link,point))/	&
                        (0.5*(dxx(link,point+1)+dxx(link,point+2)))	  
                   s=corr*						 &
                        (c(link,point+1)-c(link,point+2))/f(point)	 
                   phi=max(0.0d0,min(2.d0,2.d0*s,  &
                        0.33333333333333333d0*		  &
                        (2.d0-cflx+(1.d0+cflx)*s)))
                else
                   phi=0
                end if
                f(point)=velave*( c(link,point+1) +	 &
                     0.5*(1.0-cflx)*phi*f(point) )
             end if

          END DO

          !update concentration values
          DO point=2,maxpoints(link)-1
             !area = q(link,point)/vel(link,point)           
             dtdx = delta_t/(dxx(link,point))

             c(link,point)=c(link,point)*area_old(link,point)/area(link,point) - &
                  &dtdx*(f(point) - f(point-1))/area(link,point)
             c_old(link,point) = c(link,point)                    
          END DO

          c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
          c_old(link,maxpoints(link)) = c_old(link,maxpoints(link)-1)
          ! c(link,maxpoints(link)) = &
          !      &(c(link,maxpoints(link)-1) - c(link,maxpoints(link)-2))/dxx(link, maxpoints(link)-2)*&
          !      &(dxx(link, maxpoints(link)-2) + dxx(link, maxpoints(link)-1) + c(link,maxpoints(link)-2))
          ! c_old(link,maxpoints(link)) = c(link,maxpoints(link))
          !c(link,maxpoints(link)) = 0.0


          !---------------------------------------------------------------------------
          ! Diffusion Step
          !	explicit finite-volume solution
          !
          diffusion = .FALSE.
          IF( (species_num == 1) .AND. gas_diffusion ) diffusion = .TRUE.
          IF( (species_num == 2) .AND. temp_diffusion ) diffusion = .TRUE.
          IF(diffusion)THEN
             DO point = 2, maxpoints(link)-1

                k_e = 0.5*(k_diff(link,point+1)+k_diff(link,point))
                k_w = 0.5*(k_diff(link,point)+k_diff(link,point-1))
                area_e =0.5*(area_old(link,point+1)+area_old(link,point))
                area_w =0.5*(area_old(link,point)+area_old(link,point-1))
                !area_e = area_old(link,point)
                !area_w = area_e
                flux_e = k_e*area_e*(c_old(link,point+1)-c_old(link,point))/ABS(x(link,point+1)-x(link,point))
                flux_w = k_w*area_w*(c_old(link,point)-c_old(link,point-1))/ABS(x(link,point)-x(link,point-1))
                dtdx = delta_t/(dxx(link,point))

                c(link,point)=c(link,point) + dtdx*(flux_e - flux_w)/area_old(link,point)
                !c(link,point)=c(link,point)*area_old(link,point)/area(link,point) + dtdx*(flux_e - flux_w)/area(link,point)

             END DO
             c(link,maxpoints(link)) = c(link,maxpoints(link)-1)

          ENDIF !diffusion step if

          !-----------------------------------------------------------------------
          ! Source Term Step - could put in a RK4 routine to do reactions
          ! degassing, surface heat exchange, or other source/sink processes


          ! Correction for lateral inflow -- the
          ! concentration of for outgoing
          ! lateral inflow is the same as the
          ! current concentration

          IF (do_latflow) THEN
             DO point=2,maxpoints(link)-1
                avg_area = (area(link,point) + area_old(link,point))/2.0
                avg_latq = (latq(link,point) + latq_old(link,point))/2.0
                IF (avg_latq < 0.0) THEN
                   upstream_c = c(link,point)
                ELSE IF (species_num .EQ. 1 .AND. lattransbc_table(link) .GT. 0) THEN ! tdg
                   call bc_table_interpolate(transbc, lattransbc_table(link), time/time_mult)
                   upstream_c = bc_table_current(transbc, lattransbc_table(link), 1);
                ELSE IF (species_num .EQ. 2 .AND. lattempbc_table(link) .GT. 0) THEN ! temperature
                   call bc_table_interpolate(tempbc, lattempbc_table(link), time/time_mult)
                   upstream_c = bc_table_current(tempbc, lattempbc_table(link), 1);
                ELSE 
                   upstream_c = c(link,point)
                   ! FIXME: should this be reported as an error?
                END IF
                c(link,point) = (c(link,point)*avg_area + upstream_c*avg_latq*delta_t)/avg_area
                ! IF(c(link,point) < 0.0) c(link,point) = 0.0
             END DO
             c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
          END IF


          ! Gas exchange

          IF((species_num == 1) .AND. (gas_exchange) )THEN
             DO point=2,maxpoints(link)-1
                CALL update_met_data(time, met_zone(link))
                t_water = species(2)%conc(link,point)
                ccstar  = TDGasConc(baro_press, t_water, salinity) !c* will be conc at baro press
                transfer_coeff = gasx_a + gasx_b*windspeed + gasx_c*windspeed**2 + gasx_d*windspeed**3
                transfer_coeff = transfer_coeff*3.2808/86400.0
                !c(link,point) = c(link,point) + k_surf(link,point)*(100.0 - c(link,point))*delta_t
                c(link,point) = c(link,point) + &
                     transfer_coeff*(ccstar - c(link,point))*width(link,point)*delta_t/area(link,point)
                IF(c(link,point) < 0.0) c(link,point) = 0.0
             END DO
             c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
          ENDIF !degass if

          ! Heating/Cooling to Atmosphere

          !-------------------------------------------------------------------------
          IF((species_num == 2) .AND. (temp_exchange) )THEN
             DO point=2,maxpoints(link)-1
                CALL update_met_data(time, met_zone(link))
                t_water = c(link,point)
                depth = y(link,point) - thalweg(link,point)
                call met_zone_coeff(met_zone(link), met_coeff)
                energy_source = net_heat_flux(met_coeff,&
                     &net_solar, t_water, t_air, t_dew, windspeed) &
                     /(1000.0*4186.0/3.2808) ! rho*specifc heat*depth in feet

                !c(link,point) = c(link,point) + energy_source*delta_t/depth
                c(link,point) = c(link,point) + energy_source*delta_t*width(link,point)/area(link,point)

                ! IF(c(link,point) <   0.0) c(link,point) =   0.0 ! frozen - should add warning printout
                ! IF(c(link,point) > 100.0) c(link,point) = 100.0 ! boiling - should add warning printout
             END DO
             c(link,maxpoints(link)) = c(link,maxpoints(link)-1)
          ENDIF

       ENDIF !fluvial links if

    END DO  links_forward

    !-------------------------------------------------------------------------------
    ! 

    !IF(time==time_end)THEN
    !	DEALLOCATE(b,m)
    !ENDIF

  END SUBROUTINE tvd_transport

END MODULE scalars
