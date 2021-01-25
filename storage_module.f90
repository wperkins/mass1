! ----------------------------------------------------------------
! file: storage_module.f90
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Battelle Memorial Institute
! Pacific Northwest Laboratory
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! Created April  3, 2020 by William A. Perkins
! Last Change: 2021-01-25 11:59:41 d3g096
! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE storage_module
! ----------------------------------------------------------------
MODULE storage_module

  USE table_module
  USE utility

  IMPLICIT NONE

  PRIVATE

  ! ----------------------------------------------------------------
  ! ABSTRACT TYPE storage_t
  !
  ! The basis for a "storage", a level pool. This has no state (WSE),
  ! which is expected to be managed by the client. This only defines
  ! the relationship between stage, surface, area, volume, etc.
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, ABSTRACT :: storage_t
   CONTAINS
     ! surface area as a function of stage
     PROCEDURE (y_proc), DEFERRED :: area

     ! total volume as a function of stage
     PROCEDURE (y_proc), DEFERRED :: volume

     ! average deptha as a function of stage
     PROCEDURE (y_proc), DEFERRED :: depth

     ! rate of change in volume per change in stage, also a function
     ! of stage
     PROCEDURE (y_proc), DEFERRED :: dvdy

     ! compute elevation from a volume
     PROCEDURE (v_proc), DEFERRED :: stage

     ! get bottom elevation
     PROCEDURE (bottom_proc), DEFERRED :: bottom

     ! whatever it takes to get rid of an instance
     PROCEDURE :: destroy => storage_destroy
     
  END type storage_t

  ABSTRACT INTERFACE
     FUNCTION y_proc(this, y)
       IMPORT :: storage_t
       IMPLICIT NONE
       DOUBLE PRECISION :: y_proc
       CLASS (storage_t), INTENT(IN) :: this
       DOUBLE PRECISION, INTENT(IN) :: y
     END FUNCTION y_proc
     FUNCTION v_proc(this, v)
       IMPORT :: storage_t
       IMPLICIT NONE
       DOUBLE PRECISION :: v_proc
       CLASS (storage_t), INTENT(IN) :: this
       DOUBLE PRECISION, INTENT(IN) :: v
     END FUNCTION v_proc
     FUNCTION bottom_proc(this) 
       IMPORT :: storage_t
       IMPLICIT NONE
       DOUBLE PRECISION :: bottom_proc
       CLASS (storage_t), INTENT(IN) :: this
     END FUNCTION bottom_proc
  END INTERFACE

  ! ----------------------------------------------------------------
  ! TYPE storage_ptr
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: storage_ptr
     CLASS (storage_t), POINTER :: p
  END type storage_ptr

  ! ----------------------------------------------------------------
  ! TYPE simple_storage
  ! A very simple storage defined by constant area and bottom elev
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(storage_t) :: simple_storage
     DOUBLE PRECISION :: ybottom
     DOUBLE PRECISION :: the_area
   CONTAINS
     PROCEDURE :: area => simple_storage_area
     PROCEDURE :: volume => simple_storage_volume
     PROCEDURE :: depth => simple_storage_depth
     PROCEDURE :: dvdy => simple_storage_dvdy
     PROCEDURE :: stage => simple_storage_stage
     PROCEDURE :: bottom => simple_storage_bottom
  END type simple_storage

  INTERFACE simple_storage
     MODULE PROCEDURE new_simple_storage
  END INTERFACE simple_storage

  ! ----------------------------------------------------------------
  ! TYPE function_storage
  !
  ! Storage represented by general equations for area and WSE as
  ! described by
  !
  ! Zhao, Gang, Huilin Gao, Bibi S. Naz, Shih-Chieh Kao, and Nathalie
  ! Voisin. 2016.  Integrating a Reservoir Regulation Scheme into a
  ! Spatially Distributed Hydrological Model.  Advances in Water
  ! Resources 98 (December): 16
  ! 31. https://doi.org/10.1016/j.advwatres.2016.10.014.
  ! ----------------------------------------------------------------
  TYPE, PUBLIC, EXTENDS(storage_t) :: function_storage
     DOUBLE PRECISION :: alpha_a, beta_a, gamma_a
     DOUBLE PRECISION :: alpha_h, beta_h, gamma_h
   CONTAINS
     PROCEDURE :: area => function_storage_area
     PROCEDURE :: volume => function_storage_volume
     PROCEDURE :: depth => function_storage_depth
     PROCEDURE :: dvdy => function_storage_dvdy
     PROCEDURE :: stage => function_storage_stage
     PROCEDURE :: bottom => function_storage_bottom
  END type function_storage

  INTERFACE function_storage
     MODULE PROCEDURE new_function_storage
  END INTERFACE function_storage

CONTAINS

  ! ----------------------------------------------------------------
  ! SUBROUTINE storage_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE storage_destroy(this)

    IMPLICIT NONE
    CLASS (storage_t), INTENT(INOUT) :: this

    ! do nothing
    

  END SUBROUTINE storage_destroy


  ! ----------------------------------------------------------------
  !  FUNCTION new_simple_storage
  ! ----------------------------------------------------------------
  FUNCTION new_simple_storage(area, ybottom) RESULT(s)

    IMPLICIT NONE
    TYPE(simple_storage) :: s
    DOUBLE PRECISION, INTENT(IN) :: area, ybottom

    IF (area .LE. 0.0) THEN
       CALL error_message("simple storage: area must be positive", fatal=.TRUE.)
    END IF
    s%ybottom = ybottom
    s%the_area = area

  END FUNCTION new_simple_storage

  ! ----------------------------------------------------------------
  !  FUNCTION simple_storage_area
  ! ----------------------------------------------------------------
  FUNCTION simple_storage_area(this, y) RESULT(a)

    IMPLICIT NONE
    DOUBLE PRECISION :: a
    CLASS (simple_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    a = this%the_area
    IF (y .LE. this%ybottom) THEN
       a = 0.0
    ENDIF
  END FUNCTION simple_storage_area

  ! ----------------------------------------------------------------
  !  FUNCTION simple_storage_volume
  ! ----------------------------------------------------------------
  FUNCTION simple_storage_volume(this, y) RESULT(v)

    IMPLICIT NONE
    DOUBLE PRECISION :: v
    CLASS (simple_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    v = 0.0
    IF (y .GT. this%ybottom) THEN
       v = (y - this%ybottom)*this%the_area
    ENDIF
  END FUNCTION simple_storage_volume

  ! ----------------------------------------------------------------
  !  FUNCTION simple_storage_depth
  ! ----------------------------------------------------------------
  FUNCTION simple_storage_depth(this, y) RESULT(d)

    IMPLICIT NONE
    DOUBLE PRECISION :: d
    CLASS (simple_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    d = 0.0
    IF (y .GT. this%ybottom) THEN
       d = (y - this%ybottom)*this%the_area
    ENDIF
  END FUNCTION simple_storage_depth

  ! ----------------------------------------------------------------
  !  FUNCTION simple_storage_dvdy
  ! ----------------------------------------------------------------
  FUNCTION simple_storage_dvdy(this, y) RESULT(dvdy)

    IMPLICIT NONE
    DOUBLE PRECISION :: dvdy
    CLASS (simple_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    IF (y .GT. this%ybottom) THEN
       dvdy = this%area(y)
    ELSE
       dvdy = 0.0
    END IF
    
  END FUNCTION simple_storage_dvdy

  ! ----------------------------------------------------------------
  !  FUNCTION simple_storage_stage
  ! ----------------------------------------------------------------
  FUNCTION simple_storage_stage(this, v) RESULT(y)

    IMPLICIT NONE
    DOUBLE PRECISION :: y
    CLASS (simple_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: v

    DOUBLE PRECISION :: d

    d = v/this%the_area
    y = this%ybottom + d
    
  END FUNCTION simple_storage_stage

  ! ----------------------------------------------------------------
  !  FUNCTION simple_storage_bottom
  ! ----------------------------------------------------------------
  FUNCTION simple_storage_bottom(this) RESULT(b)

    IMPLICIT NONE
    DOUBLE PRECISION :: b
    CLASS (simple_storage), INTENT(IN) :: this
    
    b = this%ybottom

  END FUNCTION simple_storage_bottom

  ! ----------------------------------------------------------------
  !  FUNCTION new_function_storage
  ! ----------------------------------------------------------------
  FUNCTION new_function_storage(alpha_a, beta_a, gamma_a, &
       & alpha_h, beta_h, gamma_h) RESULT(s)

    IMPLICIT NONE
    TYPE(function_storage) :: s
    DOUBLE PRECISION, INTENT(IN) :: alpha_a, beta_a, gamma_a, alpha_h, beta_h, gamma_h

    s%alpha_a = alpha_a
    s%beta_a = beta_a
    s%gamma_a = gamma_a

    s%alpha_h = alpha_h
    s%beta_h = beta_h
    s%gamma_h = gamma_h

    ! FIXME: need to do some checks:
    !  - area > 0 @ S >= 0
    !  - monotonically increasing: alpha > 0.0? 

  END FUNCTION new_function_storage

  ! ----------------------------------------------------------------
  !  FUNCTION function_storage_area
  ! ----------------------------------------------------------------
  FUNCTION function_storage_area(this, y) RESULT(a)

    IMPLICIT NONE
    DOUBLE PRECISION :: a
    CLASS (function_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    DOUBLE PRECISION :: v

    a = 0.0
    v = this%volume(y)
    IF (v .GT. 0.0) THEN
       a = this%alpha_a*v**this%beta_a + this%gamma_a
    END IF
    
  END FUNCTION function_storage_area


  ! ----------------------------------------------------------------
  !  FUNCTION function_storage_volume
  ! ----------------------------------------------------------------
  FUNCTION function_storage_volume(this, y) RESULT(v)

    IMPLICIT NONE
    DOUBLE PRECISION :: v
    CLASS (function_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    v = 0.0
    IF (y .GT. this%gamma_h) THEN
       v = exp((log(y - this%gamma_h) - log(this%alpha_h))/this%beta_h)
    END IF
  END FUNCTION function_storage_volume


  ! ----------------------------------------------------------------
  !  FUNCTION function_storage_depth
  ! average depth = V/A
  ! ----------------------------------------------------------------
  FUNCTION function_storage_depth(this, y) RESULT(d)

    IMPLICIT NONE
    DOUBLE PRECISION :: d
    CLASS (function_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y

    DOUBLE PRECISION :: a, s

    d = 0.0
    IF (y .GT. this%bottom()) THEN
       a = this%area(y)
       s = this%volume(y)
       d = s/a
    END IF

  END FUNCTION function_storage_depth


  ! ----------------------------------------------------------------
  !  FUNCTION function_storage_dvdy
  ! ----------------------------------------------------------------
  FUNCTION function_storage_dvdy(this, y) RESULT(dvdy)

    IMPLICIT NONE

    DOUBLE PRECISION :: dvdy
    CLASS (function_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: y
    
    dvdy = this%area(y)
    
  END FUNCTION function_storage_dvdy


  ! ----------------------------------------------------------------
  !  FUNCTION function_storage_stage
  ! ----------------------------------------------------------------
  FUNCTION function_storage_stage(this, v) RESULT (y)

    IMPLICIT NONE
    
    DOUBLE PRECISION :: y
    CLASS (function_storage), INTENT(IN) :: this
    DOUBLE PRECISION, INTENT(IN) :: v

    y = this%alpha_h*v**this%beta_h + this%gamma_h
  
  END FUNCTION function_storage_stage


  ! ----------------------------------------------------------------
  ! FUNCTION function_storage_bottom
  !
  ! WSE when storage is zero, presumably
  ! ----------------------------------------------------------------
  FUNCTION function_storage_bottom(this) RESULT (ybot)

    IMPLICIT NONE
    DOUBLE PRECISION :: ybot
    CLASS (function_storage), INTENT(IN) :: this

    ybot = this%gamma_h
    
  END FUNCTION function_storage_bottom



END MODULE storage_module

