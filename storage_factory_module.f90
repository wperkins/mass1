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
     PROCEDURE :: generate => storage_factory_generate
     
  END type storage_factory
  
CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION storage_factory_generate_simple
  ! ----------------------------------------------------------------
  FUNCTION storage_factory_generate_simple(this, svalue) RESULT (sptr)

    IMPLICIT NONE
    TYPE (storage_ptr) :: sptr
    CLASS (storage_factory), INTENT(INOUT) :: this
    TYPE (json_value), POINTER, INTENT(IN) :: svalue
    TYPE (json_core) :: json
    DOUBLE PRECISION :: thearea, theelev
    TYPE (simple_storage), POINTER :: s
    CHARACTER (LEN=256) :: fld, msg
    LOGICAL :: found
    INTEGER :: ierr

    ierr = 0

    NULLIFY(sptr%p)

    CALL json%initialize()
    IF (json%failed()) THEN
       WRITE(msg, *) 'storage_factory_generate_simple: cannot initialize json'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
       RETURN
    END IF

    fld = "Area"
    CALL json%get(svalue, fld, thearea, found)
    IF (json%failed()) THEN
       WRITE(msg, *) 'storage_factory_generate_simple: JSON error looking for ', TRIM(fld)
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    ELSE IF (.NOT. found) THEN
       WRITE(msg, *) 'storage_factory_generate_simple: required value "', TRIM(fld), '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    END IF

    fld = "BottomElevation"
    CALL json%get(svalue, fld, theelev, found)
    IF (json%failed()) THEN
       WRITE(msg, *) 'storage_factory_generate_simple: JSON error looking for ', TRIM(fld)
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    ELSE IF (.NOT. found) THEN
       WRITE(msg, *) 'storage_factory_generate_simple: required value "', TRIM(fld), '" not found'
       CALL error_message(msg, fatal=.FALSE.)
       ierr = ierr + 1
    END IF

    IF (ierr .EQ. 0) THEN
       ALLOCATE(s)
       s = simple_storage(thearea, theelev)
       sptr%p => s
       NULLIFY(s)
    END IF

    CALL json%destroy()
    
  END FUNCTION storage_factory_generate_simple

  
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
          sptr = this%generate_simple(svalue)
       CASE DEFAULT
          WRITE(msg, *) 'storage_factory_generate: unknown type:"', TRIM(tword), '"'
          CALL error_message(msg, fatal=.FALSE.)
          ierr = ierr + 1
       END SELECT
    END IF

    CALL json%destroy()

  END FUNCTION storage_factory_generate

END MODULE storage_factory_module
