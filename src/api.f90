
!! This is the C-bound API. It is compiled together with
!! the src code, so it can use anything from there.
!! It behaves as C-code when compiled.

!! return c_ptr to constructor of t_thing
function api_thing_open()bind(C)
  use iso_c_binding
  use themodule_h, only: t_thing
  implicit none

  type( c_ptr ) :: api_thing_open
  type( t_thing ), pointer :: thing

  write(*,*) "THIS IS API_THING_OPEN"
  !! point to constructor
  thing => t_thing()

  !! get c_loc of constructor
  api_thing_open = c_loc(thing)
end function api_thing_open


subroutine api_thing_close( handle )bind(C)
  use iso_c_binding
  use themodule_h, only: t_thing
  implicit none

  type( c_ptr ), value :: handle

  type( t_thing ), pointer :: thing

  call c_f_pointer( handle, thing )

  !! this doesn't actually call the routine specified as FINAL (?)
  deallocate( thing )
  if( associated(thing)) nullify( thing )

end subroutine api_thing_close



!! call a routine which is a member of t_thing
subroutine api_print_thing( handle )bind(C)
  use iso_c_binding
  use themodule_h, only: t_thing
  implicit none

  type( c_ptr ), value :: handle

  type( t_thing ), pointer :: thing

  call c_f_pointer( handle, thing )
  call thing% print()

end subroutine api_print_thing


!! receive C-data, forward it to F, with just converting the 'name'
subroutine api_set_thing( handle, cname, csize, cval )bind(C)
  use iso_c_binding
  use themodule_h, only: t_thing
  implicit none
  INTERFACE
     FUNCTION c2f_string(ptr) RESULT(f_string)
       use iso_c_binding
       TYPE(c_ptr), INTENT(IN) :: ptr
       CHARACTER(LEN=:), ALLOCATABLE :: f_string
     END FUNCTION c2f_string
  END INTERFACE

  type( c_ptr ), value :: handle
  type( c_ptr ), value :: cname
  type( c_ptr )  :: csize !! csize and cval are passed to another F routine,
  type( c_ptr )  :: cval  !! so the "value" attribute is not here (is this the real reason?)

  type( t_thing ), pointer :: thing
  character(:), allocatable :: fname

  call c_f_pointer( handle, thing )

  fname = c2f_string( cname )

  call thing% set_thing( fname, csize, cval )

  deallocate( fname )
end subroutine api_set_thing


!! call the function to return encoded datatype of 'name'
function api_get_datatype( handle, cname) result(dtype) bind(C)
  use iso_c_binding
  use themodule_h, only: t_thing
  implicit none
  INTERFACE
     FUNCTION c2f_string(ptr) RESULT(f_string)
       use iso_c_binding
       TYPE(c_ptr), INTENT(IN) :: ptr
       CHARACTER(LEN=:), ALLOCATABLE :: f_string
     END FUNCTION c2f_string
  END INTERFACE

  type( c_ptr ), value :: handle
  type( c_ptr ), value :: cname
  integer( c_int ) :: dtype

  type( t_thing ), pointer :: thing
  character(:), allocatable :: fname

  fname = c2f_string( cname )
  call c_f_pointer( handle, thing )

  dtype = thing% get_datatype( fname )
end function api_get_datatype

!! call the function to return size of 'name' as array encoded into c_ptr
function api_get_datasize( handle, cname ) result( c_sizeptr ) bind(C)
  use iso_c_binding
  use themodule_h, only: t_thing
  implicit none
  INTERFACE
     FUNCTION c2f_string(ptr) RESULT(f_string)
       use iso_c_binding
       TYPE(c_ptr), INTENT(IN) :: ptr
       CHARACTER(LEN=:), ALLOCATABLE :: f_string
     END FUNCTION c2f_string
  END INTERFACE

  type( c_ptr ), value :: handle
  type( c_ptr ), value :: cname
  type( c_ptr ) :: c_sizeptr

  type( t_thing ), pointer :: thing
  character(:), allocatable :: fname

  fname = c2f_string( cname )
  call c_f_pointer( handle, thing )

  c_sizeptr = thing% get_datasize( fname )
end function api_get_datasize

!! function which returns c_ptr to data var of 'name'
function api_get_thing( handle, cname ) result( c_dataptr ) bind(C)
  use iso_c_binding
  use themodule_h, only: t_thing
  implicit none
  INTERFACE
     FUNCTION c2f_string(ptr) RESULT(f_string)
       use iso_c_binding
       TYPE(c_ptr), INTENT(IN) :: ptr
       CHARACTER(LEN=:), ALLOCATABLE :: f_string
     END FUNCTION c2f_string
  END INTERFACE

  type( c_ptr ), value :: handle
  type( c_ptr ), value :: cname
  type( c_ptr ) :: c_dataptr

  type( t_thing ), pointer :: thing
  character(:), allocatable :: fname
  integer( c_int ) :: dtype

  fname = c2f_string( cname )
  call c_f_pointer( handle, thing )

  c_dataptr = thing% get_thing( fname )

end function api_get_thing


!! send commands to module. The commandis decoded to F here, and
!! based on its content, different actions can be taken
function api_command( handle, ccmd ) result( ierr )bind(C)
  use iso_c_binding
  use themodule_h, only: t_thing
  implicit none
  INTERFACE
     FUNCTION c2f_string(ptr) RESULT(f_string)
       use iso_c_binding
       TYPE(c_ptr), INTENT(IN) :: ptr
       CHARACTER(LEN=:), ALLOCATABLE :: f_string
     END FUNCTION c2f_string
  END INTERFACE

  type( c_ptr ), value :: handle
  type( c_ptr ), value :: ccmd
  integer( c_int ) :: ierr

  type( t_thing ), pointer :: thing
  character(:), allocatable :: fcmd
  integer :: err

  ierr = 0_c_int

  call c_f_pointer( handle, thing )

  !! convert to F string
  fcmd = c2f_string( ccmd )

  !! do something with command:
  ! call thing% mammal% command( fcmd, err )
  write(*,*) "api received command:",fcmd

  select case( fcmd )

  case( "lone_routine" )
     call thing% call_lone()

  ! case( "self-destruct" )
  !    call thing% implode()

  ! case( "make_sandwich" )
  !    call thing% ...

  case default
     !! unknown command, return error code.
     !! Would be nice to have a collection of error codes and corresponding messages
     ierr = -1_c_int
  end select



end function api_command



FUNCTION c2f_string(ptr) RESULT(f_string)
  use iso_c_binding

  INTERFACE
     !! standard c function
     FUNCTION c_strlen(str) BIND(C, name='strlen')
       IMPORT :: c_ptr, c_size_t
       IMPLICIT NONE
       TYPE(c_ptr), INTENT(IN), VALUE :: str
       INTEGER(c_size_t) :: c_strlen
     END FUNCTION c_strlen
  END INTERFACE

  TYPE(c_ptr), INTENT(IN) :: ptr
  CHARACTER(LEN=:), ALLOCATABLE :: f_string
  CHARACTER(LEN=1, KIND=c_char), DIMENSION(:), POINTER :: c_string
  INTEGER :: n, i

  IF (.NOT. C_ASSOCIATED(ptr)) THEN
     f_string = ' '
  ELSE
     n = INT(c_strlen(ptr), KIND=KIND(n))
     CALL C_F_POINTER(ptr, c_string, [n+1])
     allocate( CHARACTER(LEN=n)::f_string)
     do i = 1, n
        f_string(i:i) = c_string(i)
     end do
  END IF
END FUNCTION c2f_string
