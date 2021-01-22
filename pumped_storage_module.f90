! ----------------------------------------------------------------
! MODULE pumped_storage_module
! ----------------------------------------------------------------
MODULE pumped_storage_module

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

  TYPE, PUBLIC, EXTENDS(storage_link_t) :: pumped_storage_link
   CONTAINS
     PROCEDURE :: initialize => pumped_storage_link_initialize
     PROCEDURE :: coeff => pumped_storage_link_coeff
     PROCEDURE :: backward_sweep => pumped_storage_link_backward
  END type pumped_storage_link

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION pumped_storage_link_initialize
  ! ----------------------------------------------------------------
  FUNCTION pumped_storage_link_initialize(this, ldata, bcman, sclrman, metman, auxdata) RESULT(ierr)

    IMPLICIT NONE
    INTEGER :: ierr
    CLASS (pumped_storage_link), INTENT(INOUT) :: this
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
          WRITE (msg, *) 'pumped storage link ', ldata%linkid, ': unknown link BC id: ', ldata%bcid
          CALL error_message(msg)
          ierr = ierr + 1
       END IF
    ELSE
       WRITE (msg, *) 'pumped storage link ', ldata%linkid, ' requires a link BC, none specified'
       CALL error_message(msg)
       ierr = ierr + 1
    END IF
    ierr = ierr + this%storage_link_t%initialize(ldata, bcman, sclrman, metman, auxdata)

  END FUNCTION pumped_storage_link_initialize

  ! ----------------------------------------------------------------
  ! SUBROUTINE pumped_storage_link_coeff
  !
  ! Should be the same as trib_inflow_link_coeff
  ! ----------------------------------------------------------------
  SUBROUTINE pumped_storage_link_coeff(this, dt, pt1, pt2, cf)

    IMPLICIT NONE

    CLASS (pumped_storage_link), INTENT(INOUT) :: this
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

  END SUBROUTINE pumped_storage_link_coeff
  

  ! ----------------------------------------------------------------
  ! SUBROUTINE pumped_storage_link_backward
  ! ----------------------------------------------------------------
  SUBROUTINE pumped_storage_link_backward(this, deltat, dsbc_type)

    IMPLICIT NONE
    CLASS (pumped_storage_link), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: dsbc_type
    DOUBLE PRECISION, INTENT(IN) :: deltat

    DOUBLE PRECISION :: qs, qsin, qsout

    CALL this%storage_link_t%backward_sweep(deltat, dsbc_type)
    
    qs = this%usbc%current_value
    IF (qs .GT. 0.0) THEN
       qsin = 0.0
       qsout = qs
    ELSE
       qsin = qs
       qsout = 0.0
    END IF
    
    CALL this%bstor%hydro(qsin, qsout, deltat)
    

  END SUBROUTINE pumped_storage_link_backward
  
END MODULE pumped_storage_module
