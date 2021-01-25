! ----------------------------------------------------------------
! MODULE storage_factory_module
! ----------------------------------------------------------------
MODULE storage_factory_module

  USE utility
  USE storage_module
  USE json_module
  
  IMPLICIT NONE

  ! ----------------------------------------------------------------
  ! TYPE storage_factory
  ! ----------------------------------------------------------------
  TYPE, PUBLIC :: storage_factory
   CONTAINS
     PROCEDURE, PRIVATE :: generate_simple => storage_factory_generate_simple
     PROCEDURE, PRIVATE :: generate_function => storage_factory_generate_function
     PROCEDURE :: generate => storage_factory_generate
     
  END type storage_factory

  PRIVATE json_get_double
  
CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION json_get_double
  ! ----------------------------------------------------------------
  FUNCTION json_get_double(json, svalue, fld, routine, x) RESULT(ierr)

    IMPLICIT NONE
    
    INTEGER :: ierr
    TYPE (json_core), INTENT(INOUT) :: json
    TYPE (json_value), POINTER, INTENT(IN) :: svalue
    CHARACTER (LEN=*), INTENT(IN) :: fld, routine
    DOUBLE PRECISION, INTENT(OUT) :: x
    
    CHARACTER (LEN=256) :: msg
    LOGICAL :: found

    x = -9999.0
    ierr = 0
    CALL json%get(svalue, fld, x, found)
    IF (json%failed()) THEN
       WRITE(msg, *) TRIM(routine), ': JSON error looking for ', TRIM(fld)
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    ELSE IF ( (.NOT. found)) THEN
       WRITE(msg, *) TRIM(routine), ': required value "', TRIM(fld), '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    END IF

  END FUNCTION json_get_double


  ! ----------------------------------------------------------------
  !  FUNCTION storage_factory_generate_simple
  !
  ! An example simple storage JSON:
  !
  !      "Storage" : {
  !        "type" : "simple",
  !        "Area" : 1000000.0,
  !        "BottomElevation" : 4.0,
  !    }
  !
  ! ----------------------------------------------------------------
  FUNCTION storage_factory_generate_simple(this, json, svalue) RESULT (sptr)

    IMPLICIT NONE
    TYPE (storage_ptr) :: sptr
    CLASS (storage_factory), INTENT(INOUT) :: this
    TYPE (json_core), INTENT(INOUT) :: json
    TYPE (json_value), POINTER, INTENT(IN) :: svalue
    DOUBLE PRECISION :: thearea, theelev
    TYPE (simple_storage), POINTER :: s
    INTEGER :: ierr

    ierr = 0

    NULLIFY(sptr%p)

    ierr = ierr + &
         &json_get_double(json, svalue, "Area", "storage_factory_generate_simple", thearea)
    ierr = ierr + &
         &json_get_double(json, svalue, "BottomElevation", "storage_factory_generate_simple", thearea)

    IF (ierr .EQ. 0) THEN
       ALLOCATE(s)
       s = simple_storage(thearea, theelev)
       sptr%p => s
       NULLIFY(s)
    END IF

  END FUNCTION storage_factory_generate_simple

  ! ----------------------------------------------------------------
  !  FUNCTION storage_factory_generate_function
  !
  ! An example function storage JSON:
  !
  !  "Storage" : {
  !      "type" : "function",
  !      "Area" : {
  !          "alpha" : 0.0,
  !          "beta"  : 0.0,
  !          "gamma" : 0.0
  !      }
  !      "Stage" : {
  !          "alpha" : 0.0,
  !          "beta"  : 0.0,
  !          "gamma" : 0.0
  !      }
  !  }
  !
  ! ----------------------------------------------------------------
  FUNCTION storage_factory_generate_function(this, json, svalue) RESULT (sptr)

    IMPLICIT NONE

    TYPE (storage_ptr) :: sptr
    CLASS (storage_factory), INTENT(INOUT) :: this
    TYPE (json_core), INTENT(INOUT) :: json
    TYPE (json_value), POINTER, INTENT(IN) :: svalue

    DOUBLE PRECISION :: alpha_a, beta_a, gamma_a
    DOUBLE PRECISION :: alpha_h, beta_h, gamma_h

    TYPE (function_storage), POINTER :: s
    
    CHARACTER (LEN=256) :: msg, fld
    TYPE (json_value), POINTER :: jv
    LOGICAL :: found
    INTEGER :: ierr

    ierr = 0

    NULLIFY(sptr%p)

    IF (json%failed()) THEN
       WRITE(msg, *) 'storage_factory_generate_function: cannot initialize json'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
       RETURN
    END IF

    fld = "Area"
    CALL json%get(svalue, fld, jv, found)
    IF (.NOT. found) THEN
       WRITE(msg, *) 'storage_factory_generate_function: JSON error looking for "', TRIM(fld), '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    ELSE 
       ierr = ierr + &
            &json_get_double(json, svalue, "Alpha", "storage_factory_generate_function", alpha_a)
       ierr = ierr + &
            &json_get_double(json, svalue, "Beta", "storage_factory_generate_function", beta_a)
       ierr = ierr + &
            &json_get_double(json, svalue, "Gamma", "storage_factory_generate_function", gamma_a)
    END IF

    fld = "Stage"
    CALL json%get(svalue, fld, jv, found)
    IF (.NOT. found) THEN
       WRITE(msg, *) 'storage_factory_generate_function: JSON error looking for "', TRIM(fld), '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    ELSE 
       ierr = ierr + &
            &json_get_double(json, svalue, "Alpha", "storage_factory_generate_function", alpha_h)
       ierr = ierr + &
            &json_get_double(json, svalue, "Beta", "storage_factory_generate_function", beta_h)
       ierr = ierr + &
            &json_get_double(json, svalue, "Gamma", "storage_factory_generate_function", gamma_h)
    END IF

    IF (ierr .EQ. 0) THEN
       ALLOCATE(s)
       s = function_storage(alpha_a, beta_a, gamma_a, &
            &alpha_h, beta_h, gamma_h)
       sptr%p => s
       NULLIFY(s)
    END IF

  END FUNCTION storage_factory_generate_function


  
  ! ----------------------------------------------------------------
  !  FUNCTION storage_factory_generate
  ! ----------------------------------------------------------------
  FUNCTION storage_factory_generate(this, svalue) RESULT (sptr)

    IMPLICIT NONE
    TYPE (storage_ptr) :: sptr
    CLASS (storage_factory), INTENT(INOUT) :: this
    TYPE (json_value), POINTER, INTENT(IN) :: svalue
    TYPE (json_core) :: json
    CHARACTER (LEN=256) :: fld, msg
    CHARACTER(KIND=json_CK,LEN=:), ALLOCATABLE :: tword
    LOGICAL :: found
    INTEGER :: ierr

    ierr = 0
    
    NULLIFY(sptr%p)

    CALL json%initialize()
    IF (json%failed()) THEN
       WRITE(msg, *) 'storage_factory_generate: cannot initialize json'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
       RETURN
    END IF

    fld = "type"
    CALL json%get(svalue, fld, tword, found)
    IF (json%failed()) THEN
       WRITE(msg, *) 'storage_factory_generate: JSON error looking for "', TRIM(fld), '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    ELSE IF (.NOT. found) THEN
       WRITE(msg, *) 'storage_factory_generate: required value "', TRIM(fld), '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    END IF
    

    IF (ierr .EQ. 0) THEN
       SELECT CASE (tword)
       CASE ("simple")
          sptr = this%generate_simple(json, svalue)
       CASE ("function")
          sptr = this%generate_function(json, svalue)
       CASE DEFAULT
          WRITE(msg, *) 'storage_factory_generate: unknown type:"', TRIM(tword), '"'
          CALL error_message(msg, fatal=.FALSE.)
          ierr = ierr + 1
       END SELECT
    END IF

    CALL json%destroy()

  END FUNCTION storage_factory_generate

END MODULE storage_factory_module
