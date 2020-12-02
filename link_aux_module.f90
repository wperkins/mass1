  ! ----------------------------------------------------------------
  ! file: link_aux_module.f90
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Battelle Memorial Institute
  ! Pacific Northwest Laboratory
  ! ----------------------------------------------------------------
  ! ----------------------------------------------------------------
  ! Created September 10, 2020 by  William Perkins 
  ! Last Change: 2020-12-02 12:59:13 d3g096
  ! ----------------------------------------------------------------
! ----------------------------------------------------------------
! MODULE link_aux_module
! ----------------------------------------------------------------
MODULE link_aux_module

  USE utility
  USE json_module
  USE json_parameters
  USE json_string_utilities

  IMPLICIT NONE

  TYPE link_auxiliary
     CHARACTER (LEN=1024) :: name
     LOGICAL :: loaded
     TYPE (json_file) :: json
   CONTAINS
     PROCEDURE :: load => link_auxiliary_load
     PROCEDURE :: get => link_auxiliary_get
     PROCEDURE :: destroy => link_auxiliary_destroy
  END type link_auxiliary

  INTERFACE link_auxiliary
     MODULE PROCEDURE new_link_auxiliary
  END INTERFACE link_auxiliary

CONTAINS

  ! ----------------------------------------------------------------
  !  FUNCTION new_link_auxiliary
  ! ----------------------------------------------------------------
  FUNCTION new_link_auxiliary() RESULT (aux)

    IMPLICIT NONE
    TYPE (link_auxiliary) :: aux

    aux%name = ""
    aux%loaded = .FALSE.

  END FUNCTION new_link_auxiliary

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_auxiliary_load
  ! ----------------------------------------------------------------
  SUBROUTINE link_auxiliary_load(this, name)

    IMPLICIT NONE
    CLASS (link_auxiliary), INTENT(INOUT) :: this
    CHARACTER (LEN=*), INTENT(IN) :: name

    IF (this%loaded) THEN
       this%name = ""
       this%loaded = .FALSE.
       CALL this%json%destroy()
    END IF

    this%name = name
    CALL this%json%initialize()
    IF (this%json%failed()) THEN
       CALL this%json%print_error_message(utility_error_iounit)
       CALL error_message("JSON parser error, cannot continue", fatal=.TRUE.)
    END IF

    CALL this%json%load_file(this%name)
    IF (this%json%failed()) THEN
       CALL this%json%print_error_message(utility_error_iounit)
       CALL error_message("JSON parser error, cannot continue", fatal=.TRUE.)
    END IF

    this%loaded = .TRUE.

  END SUBROUTINE link_auxiliary_load



  ! ----------------------------------------------------------------
  !  FUNCTION link_auxiliary_get
  ! ----------------------------------------------------------------
  FUNCTION link_auxiliary_get(this, linkid) RESULT(p)

    IMPLICIT NONE
    TYPE (json_value), POINTER :: p
    CLASS (link_auxiliary), INTENT(INOUT) :: this
    INTEGER, INTENT(IN) :: linkid
    CHARACTER(LEN=1024) :: buf, msg
    LOGICAL :: found

    IF (.NOT. this%loaded) THEN
       CALL error_message("JSON get without loaded file", fatal=.TRUE.)
    END IF

    CALL integer_to_string(linkid, int_fmt, buf)

    CALL this%json%get(buf, p, found)
    IF (this%json%failed()) THEN
       CALL this%json%print_error_message(utility_error_iounit)
       NULLIFY(p)
    ELSE IF (.NOT. found) THEN
       WRITE(msg, *) TRIM(this%name), ": error: cannot find data for link ", linkid
       CALL error_message(msg)
       NULLIFY(p)
    END IF
    
  END FUNCTION link_auxiliary_get

  ! ----------------------------------------------------------------
  ! SUBROUTINE link_auxiliary_destroy
  ! ----------------------------------------------------------------
  SUBROUTINE link_auxiliary_destroy(this)

    IMPLICIT NONE
    CLASS (link_auxiliary), INTENT(INOUT) :: this

    CALL this%json%destroy()
    this%name = ""
    this%loaded = .FALSE.

  END SUBROUTINE link_auxiliary_destroy

END MODULE link_aux_module
  
