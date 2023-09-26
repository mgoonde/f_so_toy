module m_tools

  use iso_c_binding
  implicit none
  public

  interface
     FUNCTION th_malloc(size) BIND(C, name='malloc')
       IMPORT :: c_ptr, c_size_t
       IMPLICIT NONE
       INTEGER(c_size_t), VALUE :: size
       TYPE(c_ptr) :: th_malloc
     END FUNCTION th_malloc

     SUBROUTINE th_free(ptr) BIND(C, name='free')
       IMPORT :: c_ptr
       IMPLICIT NONE
       TYPE(c_ptr), VALUE :: ptr
     END SUBROUTINE th_free

     FUNCTION c_strlen(str) BIND(C, name='strlen')
       IMPORT :: c_ptr, c_size_t
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: str
       INTEGER(c_size_t) :: c_strlen
     END FUNCTION c_strlen
  end interface

contains

  ! ----------------------------------------------------------------------
  ! copy fortran string to zero terminated c string
  ! ----------------------------------------------------------------------
  FUNCTION f2c_string(f_string) RESULT(ptr)
    use iso_c_binding
    CHARACTER(LEN=*), INTENT(IN)           :: f_string
    CHARACTER(LEN=1, KIND=c_char), POINTER :: c_string(:)
    TYPE(c_ptr) :: ptr
    INTEGER(c_size_t) :: i, n

    n = LEN_TRIM(f_string)
    ptr = th_malloc(n+1)
    CALL C_F_POINTER(ptr, c_string, [n+1])
    DO i=1, n
       c_string(i) = f_string(i:i)
    END DO
    c_string(n+1) = c_null_char
  END FUNCTION f2c_string


  ! copy null-terminated C string to fortran string
  FUNCTION c2f_string(ptr) RESULT(f_string)
    use iso_c_binding
    TYPE(c_ptr), INTENT(IN) :: ptr
    CHARACTER(LEN=:), ALLOCATABLE :: f_string
    CHARACTER(LEN=1, KIND=c_char), DIMENSION(:), POINTER :: c_string
    INTEGER :: n

    IF (.NOT. C_ASSOCIATED(ptr)) THEN
      f_string = ' '
    ELSE
      n = INT(c_strlen(ptr), KIND=KIND(n))
      CALL C_F_POINTER(ptr, c_string, [n+1])
      f_string = array2string(c_string, n)
    END IF
  END FUNCTION c2f_string

  ! Copy a known-length or null-terminated array of C characters into a string
  FUNCTION array2string(array, length) RESULT(string)
    use iso_c_binding
    CHARACTER(LEN=1, KIND=c_char), DIMENSION(:) :: array
! NOTE: giving "length" the VALUE attribute reveals a bug in gfortran 12.2.1
! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=107441
    INTEGER, INTENT(IN), OPTIONAL :: length
    CHARACTER(LEN=:), ALLOCATABLE :: string
    INTEGER :: n, i

    IF (PRESENT(length)) THEN
      n = length
    ELSE
      n = 1
      DO WHILE (n < SIZE(array) .AND. array(n+1) /= c_null_char)
        n = n + 1
      END DO
    END IF
    ALLOCATE(CHARACTER(LEN=n) :: string)
    DO i = 1, n
      string(i:i) = array(i)
    END DO
  END FUNCTION array2string


end module m_tools
