module thing_interface

  !! Fortran interface to the src/api.f90

  use iso_c_binding

  implicit none
  private
  public :: thing, ASSIGNMENT(=)


  !! values that encode datatype, they need to be equal to values in src/thing_datatype.f90
  integer( c_int ), parameter :: &
       THING_DATA_UNKNOWN = 0, &
       THING_DATA_INT     = 1, &
       THING_DATA_INT_1D  = 2, &
       THING_DATA_INT_2D  = 3, &
       THING_DATA_REAL    = 4, &
       THING_DATA_REAL_1D = 5, &
       THING_DATA_REAL_2D = 6, &
       THING_DATA_STR     = 7


  type thing
     type( c_ptr ) :: handle
   contains
     procedure :: close => th_close
     procedure :: print => th_print

     procedure, private :: &
          th_set_int, th_set_real, &
          th_set_int1d, th_set_real1d, &
          th_set_int2d, th_set_real2d, &
          th_set_str
     generic :: set => &
          th_set_int, th_set_real, &
          th_set_int1d, th_set_real1d, &
          th_set_int2d, th_set_real2d, &
          th_set_str

     procedure :: get => th_get_thing
     procedure :: command => th_command
  end type thing

  !! overload type with th_open
  interface thing
     module procedure th_open
  end interface thing


  !! derived type to hold data when extracting
  TYPE :: thing_data
     INTEGER(c_int) :: datatype = -1_c_int
     CLASS(thing), POINTER, PRIVATE :: thing_instance => NULL()

     INTEGER(c_int), POINTER :: integer => NULL()
     INTEGER(c_int), DIMENSION(:), POINTER :: integer_vec => NULL()
     INTEGER(c_int), DIMENSION(:,:), POINTER :: integer_mat => NULL()
     ! REAL(c_double), POINTER :: scalar => NULL()
     REAL(c_float), POINTER :: scalar => NULL()
     REAL(c_float), DIMENSION(:), POINTER :: scalar_vec => NULL()
     REAL(c_float), DIMENSION(:,:), POINTER :: scalar_mat => NULL()
     CHARACTER(LEN=:), ALLOCATABLE :: string
  END TYPE thing_data
  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE &
          assign_int_to_thing_data, &
          assign_real_to_thing_data, &
          assign_intvec_to_thing_data, &
          assign_realvec_to_thing_data, &
          assign_intmat_to_thing_data, &
          ! assign_int3d_to_thing_data, &
          assign_realmat_to_thing_data, &
          assign_string_to_thing_data
          ! assign_stringvec_to_thing_data
  END INTERFACE ASSIGNMENT(=)



  !! some C functions
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


  !! interfaces to src/api.f90
  interface

     function api_thing_open()bind(C)
       import :: c_ptr
       type( c_ptr ) :: api_thing_open
     end function api_thing_open

     subroutine api_thing_close( handle )bind(C)
       import :: c_ptr
       type( c_ptr ), value :: handle
     end subroutine api_thing_close

     subroutine api_print_thing( handle )bind(C)
       import :: c_ptr
       type( c_ptr ), value :: handle
     end subroutine api_print_thing

     subroutine api_set_thing( handle, cname, csize, cval )bind(C)
       import :: c_ptr
       type( c_ptr ), value :: handle
       type( c_ptr ), value :: cname
       type( c_ptr ) :: csize
       type( c_ptr ) :: cval
     end subroutine api_set_thing

     function api_get_datatype( handle, cname ) result( dtype ) bind(C)
       import :: c_ptr, c_int
       type( c_ptr ), value :: handle
       type( c_ptr ), value :: cname
       integer( c_int ) :: dtype
     end function api_get_datatype

     function api_get_datasize( handle, cname ) result( c_sizeptr ) bind(C)
       import :: c_ptr
       type( c_ptr ), value :: handle
       type( c_ptr ), value :: cname
       type( c_ptr ) :: c_sizeptr
     end function api_get_datasize

     function api_get_thing( handle, cname ) result( c_dataptr ) bind(C)
       import :: c_ptr, c_int
       type( c_ptr ), value :: handle
       type( c_ptr ), value :: cname
       type( c_ptr ) :: c_dataptr
     end function api_get_thing

     function api_command( handle, ccmd ) result(ierr) bind(C)
       import :: c_ptr, c_int
       type( c_ptr ), value :: handle
       type( c_ptr ), value :: ccmd
       integer( c_int ) :: ierr
     end function api_command

  end interface

contains


  !!=============================
  !! wrapper functions to call api
  !!=============================

  !! initialize the .so :: honestly i don't know how this
  !! magic works, but this below is identical to dlopen(), and
  !! it gives direct access to all routines in the .so without
  !! having to dlsym() every time.
  type( thing ) function th_open()
    implicit none

    !! handle becomes pointer to constructor of the module in F.
    th_open% handle = api_thing_open()

  end function th_open


  !! call a routine from the thing module, via call to API
  subroutine th_close( self )
    class( thing ) :: self
    call api_thing_close( self% handle )
  end subroutine th_close



  !! call a routine from the thing module, via call to API
  subroutine th_print( self )
    class( thing ) :: self
    call api_print_thing( self% handle )
  end subroutine th_print



  !! send a single command (string)
  function th_command( self, cmd ) result(err)
    class( thing ) :: self
    character(*), intent(in) :: cmd
    integer :: err

    type( c_ptr ) :: ccmd
    integer( c_int ) :: cerr

    !! convert string to C
    ccmd = f2c_string( cmd )

    !! call api
    cerr = api_command( self% handle, ccmd )
    err = int( cerr )

    call th_free( ccmd )
  end function th_command
  !! a routine with a command and value in arguments would be effectively
  !! identical to the set_* routines, just calling different API routine


  !!===============================================
  !! subroutines to set each type of data:
  !! They follow the same principle, for each data type,
  !! 1. encode the fortran string containing name of the variable into C
  !! 2. encode the size of data into a 2D-array, and into C-pointer
  !! 3. encode the value to be sent into C-pointer
  !! 4. call the API routine with C-style data
  !!===============================================
  !! integer
  subroutine th_set_int( self, name, val )
    class( thing ) :: self
    character(*), intent(in) :: name
    integer, intent(in) :: val

    type( c_ptr ) :: cname, cval, csize
    integer, pointer :: val_ptr
    integer, dimension(:), pointer :: size_ptr

    !! convert fortran string to c
    cname = f2c_string( name )

    !! encode size into C-pointer
    allocate( size_ptr(1:2), source=1)
    csize = c_loc( size_ptr(1) )
    ! write(*,*) "size ptr in th_set_int",size_ptr

    !! encode value into C-pointer
    allocate( val_ptr, source=int(val) )
    cval = c_loc(val_ptr)

    !! call the api with C-style data
    call api_set_thing( self% handle, cname, csize, cval)

    call th_free( cname )
    call th_free( cval )
    call th_free(csize)
  end subroutine th_set_int

  !! real
  subroutine th_set_real( self, name, val )
    class( thing ) :: self
    character(*), intent(in) :: name
    real, intent(in) :: val

    type( c_ptr ) :: cname, cval, csize
    real, pointer :: val_ptr
    integer, dimension(:), pointer :: size_ptr

    cname = f2c_string( name )

    allocate( size_ptr(1:2), source=1)
    csize = c_loc(size_ptr(1))

    allocate( val_ptr, source=real(val) )
    cval = c_loc(val_ptr)

    call api_set_thing( self% handle, cname, csize, cval)

    call th_free( cname )
    call th_free( cval )
    call th_free(csize)
  end subroutine th_set_real

  !! integer, dimension(:,:)
  subroutine th_set_int1d( self, name, val )
    class( thing ) :: self
    character(*), intent(in) :: name
    integer, dimension(:), intent(in) :: val

    integer :: dim1
    type( c_ptr ) :: cname, cval, csize
    integer, dimension(:), pointer :: val_ptr
    integer, dimension(:), pointer :: size_ptr

    dim1 = size(val,1)

    cname = f2c_string( name )

    !! encode size into 2D list with values: ( dim1, 1 )
    allocate( size_ptr(2) )
    size_ptr(1) = dim1; size_ptr(2) = 1
    csize = c_loc( size_ptr(1) )

    !! encode actual values
    allocate( val_ptr, source = int(val) )
    cval = c_loc( val_ptr(1) )

    call api_set_thing( self% handle, cname, csize, cval )

    call th_free( cname )
    call th_free( csize )
    call th_free( cval )

  end subroutine th_set_int1d

  !! real, dimension(:)
  subroutine th_set_real1d( self, name, val )
    class( thing ) :: self
    character(*), intent(in) :: name
    real, dimension(:), intent(in) :: val

    integer :: dim1
    type( c_ptr ) :: cname, cval, csize
    real, dimension(:), pointer :: val_ptr
    integer, dimension(:), pointer :: size_ptr

    dim1 = size(val,1)

    cname = f2c_string( name )

    !! encode size into 2D list with values: ( dim1, 1 )
    allocate( size_ptr(2) )
    size_ptr(1) = dim1; size_ptr(2) = 1
    csize = c_loc( size_ptr(1) )

    !! encode actual values
    allocate( val_ptr, source = real(val) )
    cval = c_loc( val_ptr(1) )

    call api_set_thing( self% handle, cname, csize, cval )

    call th_free( cname )
    call th_free( cval )
    call th_free( csize )

  end subroutine th_set_real1d

  !! integer, dimension(:,:)
  subroutine th_set_int2d( self, name, val )
    class( thing ) :: self
    character(*), intent(in) :: name
    integer, dimension(:,:), intent(in) :: val

    integer :: dim1, dim2
    type( c_ptr ) :: cname, cval, csize
    integer, dimension(:,:), pointer :: val_ptr
    integer, dimension(:), pointer :: size_ptr

    dim1 = size(val,1)
    dim2 = size(val,2)

    cname = f2c_string( name )

    !! encode size into 2D list with values: ( dim1, dim2 )
    allocate( size_ptr(2) )
    size_ptr(1) = dim1; size_ptr(2) = dim2
    csize = c_loc( size_ptr(1) )

    !! encode actual values
    allocate( val_ptr, source = int(val) )
    cval = c_loc( val_ptr(1,1) )

    call api_set_thing( self% handle, cname, csize, cval )

    call th_free( cname )
    call th_free( cval )
    call th_free( csize )

  end subroutine th_set_int2d

  !! real, dimension(:,:)
  subroutine th_set_real2d( self, name, val )
    class( thing ) :: self
    character(*), intent(in) :: name
    real, dimension(:,:), intent(in) :: val

    integer :: dim1, dim2
    type( c_ptr ) :: cname, cval, csize
    real, dimension(:,:), pointer :: val_ptr
    integer, dimension(:), pointer :: size_ptr

    dim1 = size(val,1)
    dim2 = size(val,2)

    cname = f2c_string( name )

    !! encode size into 2D list with values: ( dim1, dim2 )
    allocate( size_ptr(2) )
    size_ptr(1) = dim1; size_ptr(2) = dim2
    csize = c_loc( size_ptr(1) )

    !! encode actual values
    allocate( val_ptr, source = real(val) )
    cval = c_loc( val_ptr(1,1) )

    call api_set_thing( self% handle, cname, csize, cval )

    call th_free( cname )
    call th_free( cval )
    call th_free( csize )

  end subroutine th_set_real2d

  !! character(*)
  subroutine th_set_str( self, name, val )
    class( thing ) :: self
    character(*), intent(in) :: name
    character(*), intent(in) :: val

    integer :: dim1, dim2
    type( c_ptr ) :: cname, cval, csize
    integer, dimension(:), pointer :: size_ptr

    dim1 = len_trim(val)
    dim2 = 1

    cname = f2c_string( name )

    allocate( size_ptr(2) )
    size_ptr(1) = dim1; size_ptr(2) = dim2
    csize = c_loc( size_ptr(1) )

    cval = f2c_string( val )

    call api_set_thing( self% handle, cname, csize, cval )

    call th_free( cname )
    call th_free( csize )
    call th_free( cval )
  end subroutine th_set_str

  !!===============================================



  !! function to extract data from 'thing' via API:
  !! 'name' is the variable name we want to extract.
  !! The data returned from api is a C-pointer, for which we
  !! need to know the type, shape/size, so we can cast it into
  !! the correct Fortran data. So the steps here are:
  !! 1. get the type of data, by calling api with name of the variable we want
  !! 2. get the C-pointer to data, and C-pointer to size array
  !! 3. according to datatype, correctly convert C-pointer to F-pointer,
  !!    and record the F-data in a derived type. This derived type has overloaded
  !!    assignment operator, so the correct data will be extracted when called.
  function th_get_thing( self, name ) result(recv_data)
    !! the result is written into derived type which has
    !! an overloaded assignment
    class( thing ) :: self
    character(*), intent(in) :: name
    type( thing_data ) :: recv_data

    type( c_ptr ) :: cname
    type( c_ptr ) :: c_sizeptr, c_dataptr
    integer( c_int ) :: dtype
    integer, dimension(:), pointer :: f_sizeptr

    real, pointer :: pp

    cname = f2c_string( name )

    !! get type
    dtype = api_get_datatype( self% handle, cname )
    recv_data% datatype = dtype

    !! get data
    c_dataptr = api_get_thing( self% handle, cname )

    !! get size
    c_sizeptr = api_get_datasize( self% handle, cname )
    call c_f_pointer( c_sizeptr, f_sizeptr, [2] )

    !! cast the C-pointer to correct F-pointer inside the derived type
    select case( dtype )
    case( THING_DATA_INT )
       call c_f_pointer( c_dataptr, recv_data% integer )
    case( THING_DATA_INT_1D )
       call c_f_pointer( c_dataptr, recv_data% integer_vec, [f_sizeptr(1)] )
    case( THING_DATA_INT_2D )
       call c_f_pointer( c_dataptr, recv_data% integer_mat, [f_sizeptr(1), f_sizeptr(2)] )
    case( THING_DATA_REAL )
       call c_f_pointer( c_dataptr, recv_data% scalar )
    case( THING_DATA_REAL_1D )
       call c_f_pointer( c_dataptr, recv_data% scalar_vec, [f_sizeptr(1)] )
    case( THING_DATA_REAL_2D )
       call c_f_pointer( c_dataptr, recv_data% scalar_mat, [f_sizeptr(1), f_sizeptr(2)] )
    case( THING_DATA_STR )
       recv_data% string = c2f_string( c_dataptr )
    case( THING_DATA_UNKNOWN )
       write(*,*) "UKNOWN dtype in th_get_thing:",name
    case default
       write(*,*) "dtype has unsupported value in th_get_thing:",dtype, name
    end select

    ! call th_free( c_dataptr )
    call th_free( cname )
    call th_free( c_sizeptr )

  end function th_get_thing





  !! overloaded assignment for setting thing_data to proper output,
  !! after the get_thing function.
  !! Attention, the conversion between precisions happens here.
  SUBROUTINE assign_int_to_thing_data(lhs, rhs)
    INTEGER, INTENT(OUT), POINTER :: lhs
    CLASS(thing_data), INTENT(IN) :: rhs

    IF (rhs% datatype == THING_DATA_INT) THEN
       ! lhs => rhs% integer
       allocate( lhs, source=int(rhs% integer))
    ELSE
       CALL assignment_error(rhs, 'integer')
    END IF
  END SUBROUTINE assign_int_to_thing_data
  SUBROUTINE assign_real_to_thing_data(lhs, rhs)
    REAL, INTENT(OUT), POINTER :: lhs
    CLASS(thing_data), INTENT(IN) :: rhs

    IF (rhs% datatype == THING_DATA_REAL) THEN
       ! lhs => rhs% scalar
       allocate( lhs, source=real(rhs% scalar))
    ELSE
       CALL assignment_error(rhs, 'real')
    END IF
  END SUBROUTINE assign_real_to_thing_data
  SUBROUTINE assign_intvec_to_thing_data(lhs, rhs)
    INTEGER, DIMENSION(:), INTENT(OUT), POINTER :: lhs
    CLASS(thing_data), INTENT(IN) :: rhs

    IF (rhs% datatype == THING_DATA_INT_1D) THEN
       ! lhs => rhs% integer_vec
       allocate( lhs, source=int(rhs% integer_vec))
    ELSE
       CALL assignment_error(rhs, 'intger 1d')
    END IF
  END SUBROUTINE assign_intvec_to_thing_data
  SUBROUTINE assign_realvec_to_thing_data(lhs, rhs)
    REAL, DIMENSION(:), INTENT(OUT), POINTER :: lhs
    CLASS(thing_data), INTENT(IN) :: rhs

    IF (rhs% datatype == THING_DATA_REAL_1D) THEN
       ! lhs => rhs% scalar_vec
       allocate( lhs, source=real( rhs% scalar_vec))
    ELSE
       CALL assignment_error(rhs, 'real 1d')
    END IF
  END SUBROUTINE assign_realvec_to_thing_data
  SUBROUTINE assign_intmat_to_thing_data(lhs, rhs)
    INTEGER, DIMENSION(:,:), INTENT(OUT), POINTER :: lhs
    CLASS(thing_data), INTENT(IN) :: rhs

    IF (rhs% datatype == THING_DATA_INT_2D) THEN
       ! lhs => rhs% integer_mat
       allocate( lhs, source=int(rhs% integer_mat))
    ELSE
       CALL assignment_error(rhs, 'integer 2d')
    END IF
  END SUBROUTINE assign_intmat_to_thing_data
  ! SUBROUTINE assign_int3d_to_thing_data(lhs, rhs)
  !   INTEGER, DIMENSION(:,:,:), INTENT(OUT), POINTER :: lhs
  !   CLASS(thing_data), INTENT(IN) :: rhs

  !   IF (rhs% datatype == THING_DATA_INT_3D) THEN
  !      ! lhs => rhs% integer_3d
  !      allocate( lhs, source=int(rhs% integer_3d))
  !   ELSE
  !      CALL assignment_error(rhs, 'integer 3d')
  !   END IF
  ! END SUBROUTINE assign_int3d_to_thing_data
  SUBROUTINE assign_realmat_to_thing_data(lhs, rhs)
    REAL, DIMENSION(:,:), INTENT(OUT), POINTER :: lhs
    CLASS(thing_data), INTENT(IN) :: rhs

    IF (rhs% datatype == THING_DATA_REAL_2D) THEN
       ! lhs => rhs% scalar_mat
       allocate( lhs, source=real(rhs% scalar_mat))
    ELSE
       CALL assignment_error(rhs, 'real 2d')
    END IF
  END SUBROUTINE assign_realmat_to_thing_data
  SUBROUTINE assign_string_to_thing_data(lhs, rhs)
    ! CHARACTER(*), INTENT(OUT) :: lhs
    CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: lhs
    CLASS(thing_data), INTENT(IN) :: rhs

    IF (rhs% datatype == THING_DATA_STR) THEN
       ! allocate( character(len=rhs% n)::lhs)
       allocate( lhs, source=rhs% string)
       ! lhs = rhs% string
    ELSE
       CALL assignment_error(rhs, 'string')
    END IF
  END SUBROUTINE assign_string_to_thing_data
  ! SUBROUTINE assign_stringvec_to_thing_data(lhs, rhs)
  !   CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: lhs(:)
  !   CLASS(thing_data), INTENT(IN) :: rhs
  !   type( c_ptr ) :: cname, cmsg

  !   IF (rhs% datatype == THING_DATA_STRING_1D) THEN
  !      allocate( character(len=rhs% n)::lhs(1:rhs% m))
  !      lhs = rhs% string_vec
  !   ELSE
  !      CALL assignment_error(rhs, 'string 1d')
  !   END IF
  ! END SUBROUTINE assign_stringvec_to_thing_data

  SUBROUTINE assignment_error( rhs, lhs_typ )
    CLASS(thing_data), INTENT(IN) :: rhs
    CHARACTER(len=*), INTENT(IN) :: lhs_typ

    type( c_ptr ) :: cname, cmsg
    character(len=256) :: rhs_typ

    ! cmsg = f2c_string( msg )
    cname = f2c_string( "assignment_to_thing_data, while trying to assign types" )

    select case( rhs% datatype )
    case( THING_DATA_INT )
       rhs_typ = "integer"
    case( THING_DATA_REAL )
       rhs_typ = "real"
    case( THING_DATA_INT_1D )
       rhs_typ = "integer"
    case( THING_DATA_REAL_1D )
       rhs_typ = "real 1d"
    case( THING_DATA_INT_2D )
       rhs_typ = "integer 2d"
    case( THING_DATA_REAL_2D )
       rhs_typ = "real 2d"
    ! case( THING_DATA_STRING )
    !    rhs_typ = "string"
    ! case( THING_DATA_STRING_1D )
    !    rhs_typ = "string 1d"
    case default
       rhs_typ = "unknown"
    end select

    cmsg = f2c_string( trim(rhs_typ)//" to "//lhs_typ )
    ! call thing_error( rhs% thing_instance% handle, cname, cmsg )
  END SUBROUTINE assignment_error


  ! ----------------------------------------------------------------------
  ! copy fortran string to zero terminated c string, and return
  ! a pointer to it.
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

  ! decode a C-pointer containing a string, and
  ! copy the null-terminated C string to fortran string
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


end module thing_interface

