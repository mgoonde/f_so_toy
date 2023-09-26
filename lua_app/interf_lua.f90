module interf_lua
  use iso_c_binding
  use :: lua

  implicit none


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


  type(c_ptr), save :: handle


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

  interface

     function api_thing_open()bind(C)
       import :: c_ptr
       type( c_ptr ) :: api_thing_open
     end function api_thing_open

     subroutine api_thing_close( handle )bind(C)
       import :: c_ptr
       type( c_ptr ), value :: handle
     end subroutine api_thing_close

     function api_command( handle, ccmd ) result(ierr) bind(C)
       import :: c_ptr, c_int
       type( c_ptr ), value :: handle
       type( c_ptr ), value :: ccmd
       integer( c_int ) :: ierr
     end function api_command

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

  end interface


  !! useful helper function when debugging
  ! interface
  !    subroutine print_stack(lua)bind(C,name="print_stack")
  !      import :: c_ptr
  !      type( c_ptr ), intent(in), value :: lua
  !    end subroutine print_stack
  ! end interface

contains

  !! this subroutine constructs the interface object to lua,
  !! it needs to exist, and needs to register all functions
  !! from this module.
  subroutine luaopen_interf_lua(lua)bind(c)
    type( c_ptr ), intent(in), value :: lua

    call lua_register( lua, "m_open", c_funloc(thing_open) )
    call lua_register( lua, "m_close", c_funloc(thing_close) )
    call lua_register( lua, "m_command", c_funloc(thing_command) )
    call lua_register( lua, "m_set", c_funloc(thing_set) )
    call lua_register( lua, "m_get", c_funloc(thing_get) )
    call lua_register( lua, "m_print", c_funloc(thing_print) )
  end subroutine luaopen_interf_lua

  !! all functions visible from lua have the same prototype which is:
  !!
  !! >>> function funcname( lua ) result( nret ) bind(c)
  !! >>>   type( c_ptr ), value, intent(in) :: lua
  !! >>>   integer( c_int ) :: nret
  !! >>>
  !! >>>   ... do things
  !! >>>   ... set nret
  !! >>>
  !! >>> end function funcname
  !!
  !! To call this function from lua, first need to register it in luaopen_* subroutine,
  !! then call from lua like:
  !!
  !!     require("interf_lua")
  !!     m = funcname( arg1, arg2, arg3, ... )    # the number of arguments is not pre-defined
  !!
  !! The communication of data from lua to C/fortran happens through a
  !! "virtual stack", which works like this:
  !! when you call 'm=funcname( arg1, arg2 )' from lua, the variables arg1 and arg2 are put
  !! on the stack. Then in the C/fortran implementation of 'funcname', we have to read this
  !! data from the virtual stack.
  !! In order to send data back to lua, we have to put the data on the 'virtual stack', and tell
  !! the result 'nret' how many variables we are returning.
  !! One array, no matter the size or dimension, counts as one variable.
  !!

  function thing_open( lua ) result(nret)bind(C)
    !! call from lua like:
    !!
    !!    m_open()
    !!
    type( c_ptr ), value, intent(in) :: lua
    integer( c_int ) :: nret


    handle = api_thing_open()
    nret = 0

  end function thing_open

  function thing_close( lua ) result(nret)bind(C)
    !! call from lua like:
    !!
    !!    m_close()
    !!

    type( c_ptr ), value, intent(in) :: lua
    integer( c_int ) :: nret

    call api_thing_close( handle )
    nret = 0

  end function thing_close

  function thing_command( lua ) result(nret)bind(C)
    !! call from lua like:
    !!
    !!    m_command( "a string command" )
    !!

    type( c_ptr ), value, intent(in) :: lua
    integer( c_int ) :: nret

    character(len=:), allocatable :: cmd
    type( c_ptr ) :: ccmd, il
    integer( c_int ) :: ierr

    !! get single string from lua stack
    cmd = lua_tostring(lua, -1)
    !! remove value from stack
    call lua_pop(lua, 1)

    !! convert string to c
    ccmd = f2c_string(cmd)

    !! send command
    ierr = api_command(handle, ccmd)

    !! push interger value of ierr to stack
    call lua_pushinteger(lua, int(ierr, lua_integer) )
    !! return ierr: is one result
    nret = 1

  end function thing_command


  function thing_print( lua ) result(nret)bind(C)
    !! call from lua like:
    !!
    !!    m_print()
    !!

    type( c_ptr ), value, intent(in) :: lua
    integer( c_int ) :: nret

    !! takes no arguments
    call api_print_thing( handle )

  end function thing_print


  function thing_set( lua ) result(nret)bind(C)
    !! call from lua like: m_set( val, size, name), e.g.:
    !!
    !!    -- ## define 3x4 real array:
    !!    a = { {1.1, 1.2, 1.3}, {2.1, 2.2, 2.3}, {3.1, 3.2, 3.3}, {4.1, 4.2, 4.3} }
    !!    -- ## call set
    !!    m_set( a, 3, 4, "real2d" )
    !!
    !!    -- ## define 2x1 int array:
    !!    b = { 7, 93 }
    !!    -- ## call set
    !!    m_set( b, 2, "int1d" )
    !!

    type( c_ptr ), value, intent(in) :: lua
    integer( c_int ) :: nret

    type( c_ptr ) :: cname, cval, csize
    character(len=:), allocatable :: name, strg
    integer( c_int ) :: dtype, c, r
    integer( c_int ) :: intgr
    integer( c_int ), allocatable :: int1d(:), int2d(:,:)
    integer( c_int ), pointer :: i_ptr
    integer( c_int ), dimension(:), pointer :: i1d_ptr
    integer( c_int ), dimension(:,:), pointer :: i2d_ptr
    real( c_float ), allocatable :: real1d(:), real2d(:,:)
    real( c_float ), pointer :: r_ptr
    real( c_float ), dimension(:), pointer :: r1d_ptr
    real( c_float ), dimension(:,:), pointer :: r2d_ptr
    real( lua_number ) :: dreal
    integer(c_int), dimension(:), pointer :: size_ptr

    !! takes two arguments: name, val
    !! write them in opposite order: val, name

    !! take last arg from stack: name
    name = lua_tostring(lua, -1)
    call lua_pop(lua, 1)
    !! convert name to c
    cname = f2c_string( name )

    !! get datatype of name
    dtype = api_get_datatype( handle, cname )

    !! allocate size
    allocate( size_ptr(1:2), source = 1)

    select case( dtype )

    case( THING_DATA_STR )
       !! get string
       strg = lua_tostring(lua, -1)
       call lua_pop(lua, 1)
       !! set size
       size_ptr(1) = len_trim(strg)
       csize = c_loc( size_ptr(1) )
       !! get c val
       cval = f2c_string(strg)
       call api_set_thing( handle, cname, csize, cval )
       call th_free( cname)
       call th_free( csize )
       call th_free( cval )

    case( THING_DATA_INT )
       !! get integer from stack
       intgr = lua_tonumber(lua, -1)
       call lua_pop(lua, 1)
       csize = c_loc( size_ptr(1) )
       !! set val ptr
       allocate( i_ptr, source=int(intgr) )
       cval = c_loc(i_ptr)
       !! send to api
       call api_set_thing( handle, cname, csize, cval )
       call th_free( cname )
       call th_free( csize )
       call th_free( cval )

    case( THING_DATA_REAL )
       !! get real from stack
       dreal = lua_tonumber(lua, -1)
       call lua_pop(lua, 1)
       csize = c_loc( size_ptr(1) )
       !! set val ptr
       allocate( r_ptr, source=real(dreal, c_float) )
       cval = c_loc(r_ptr)
       call api_set_thing( handle, cname, csize, cval )
       call th_free( cname )
       call th_free( csize )
       call th_free( cval )

    case( THING_DATA_INT_1D )
       ! call print_stack(lua)
       !! get c dimension from stack
       c = lua_tonumber(lua, -1)
       call lua_pop(lua, 1)
       !! get 1D int from stack
       allocate( int1d(1:c), source=0 )
       call receive_1D_arr_int( lua, c, int1d)
       call lua_pop(lua, 1)
       !! size
       size_ptr(1) = c
       csize = c_loc( size_ptr(1) )
       allocate( i1d_ptr, source = int(int1d) )
       cval = c_loc( i1d_ptr(1) )
       !! send to api
       call api_set_thing( handle, cname, csize, cval )
       call th_free( cname )
       call th_free( csize )
       call th_free( cval )

    case( THING_DATA_INT_2D )
       !! get r dimension from stack
       r = lua_tonumber(lua, -1)
       call lua_pop(lua, 1)
       !! get c dimension from stack
       c = lua_tonumber(lua, -1)
       call lua_pop(lua, 1)
       !! get 1D int from stack
       allocate( int2d(1:c,1:r), source=0 )
       call receive_2D_arr_int( lua, c, r, int2d)
       call lua_pop(lua, 1)
       !! size
       size_ptr(1) = c
       size_ptr(2) = r
       csize = c_loc( size_ptr(1) )
       allocate( i2d_ptr, source = int(int2d) )
       cval = c_loc( i2d_ptr(1,1) )
       !! send to api
       call api_set_thing( handle, cname, csize, cval )
       call th_free( cname )
       call th_free( csize )
       call th_free( cval )

    case( THING_DATA_REAL_1D )
       ! call print_stack(lua)
       !! get c dimension from stack
       c = lua_tonumber(lua, -1)
       call lua_pop(lua, 1)
       !! get 1D int from stack
       allocate( real1d(1:c), source=0.0_c_float )
       call receive_1D_arr( lua, c, real1d)
       call lua_pop(lua, 1)
       !! size
       size_ptr(1) = c
       csize = c_loc( size_ptr(1) )
       allocate( r1d_ptr, source = real(real1d, c_float) )
       cval = c_loc( r1d_ptr(1) )
       !! send to api
       call api_set_thing( handle, cname, csize, cval )
       call th_free( cname )
       call th_free( csize )
       call th_free( cval )

    case( THING_DATA_REAL_2D )
       !! get r dimension from stack
       r = lua_tonumber(lua, -1)
       call lua_pop(lua, 1)
       !! get c dimension from stack
       c = lua_tonumber(lua, -1)
       call lua_pop(lua, 1)
       !! get 1D int from stack
       allocate( real2d(1:c,1:r), source=0.0_c_float )
       call receive_2D_arr( lua, c, r, real2d)
       call lua_pop(lua, 1)
       !! size
       size_ptr(1) = c
       size_ptr(2) = r
       csize = c_loc( size_ptr(1) )
       allocate( r2d_ptr, source = real(real2d, c_float) )
       cval = c_loc( r2d_ptr(1,1) )
       !! send to api
       call api_set_thing( handle, cname, csize, cval )
       call th_free( cname )
       call th_free( csize )
       call th_free( cval )


    end select

  end function thing_set




  function thing_get( lua ) result(nret)bind(C)
    !! call from lua like:
    !!
    !!  b = m_get( name )
    !!

    type( c_ptr ), value, intent(in) :: lua
    integer( c_int ) :: nret

    type( c_ptr ) :: cname, csize, cdata, cc
    integer( c_int ) :: dtype
    character(len=:), allocatable :: name, fstr
    integer, dimension(:), pointer :: fsize

    integer(c_int), pointer :: i_ptr
    real( c_float ), pointer :: r_ptr
    integer(c_int), pointer :: i1d_ptr(:), i2d_ptr(:,:)
    real( c_float ), pointer :: r1d_ptr(:), r2d_ptr(:,:)

    name = lua_tostring(lua, -1)
    call lua_pop(lua, 1)
    !! convert name to c
    cname = f2c_string( name )

    !! get datatype of name
    dtype = api_get_datatype( handle, cname )

    !! get datasize
    csize = api_get_datasize( handle, cname )
    call c_f_pointer( csize, fsize, [2] )

    !! get data
    cdata = api_get_thing( handle, cname )

    !! cast data into correct type and shape
    select case( dtype )
    case( THING_DATA_INT )
       call c_f_pointer( cdata, i_ptr )
       !! put this value to stack
       call lua_pushinteger( lua, int( i_ptr, lua_integer) )

    case( THING_DATA_INT_1D )
       call c_f_pointer( cdata, i1d_ptr, [fsize(1)] )
       call send_1D_arr_int( lua, fsize(1), i1d_ptr )

    case( THING_DATA_INT_2D )
       call c_f_pointer( cdata, i2d_ptr, [fsize(1), fsize(2)] )
       call send_2D_arr_int( lua, fsize(1), fsize(2), i2d_ptr )

    case( THING_DATA_REAL )
       call c_f_pointer( cdata, r_ptr )
       call lua_pushnumber( lua, real(r_ptr, lua_number) )

    case( THING_DATA_REAL_1D )
       call c_f_pointer( cdata, r1d_ptr, [fsize(1)] )
       call send_1D_arr(lua, fsize(1), r1d_ptr )

    case( THING_DATA_REAL_2D )
       call c_f_pointer( cdata, r2d_ptr, [fsize(1), fsize(2)] )
       call send_2D_arr(lua, fsize(1), fsize(2), r2d_ptr )

    case( THING_DATA_STR )
       fstr = c2f_string( cdata )
       cc = lua_pushstring(lua, fstr )

    case( THING_DATA_UNKNOWN )
       write(*,*) ">> ERROR: Unknown data type in name:",trim(name)
       call lua_pushinteger(lua, int(-99, lua_integer))
    end select

    !! always return only one thing to the stack
    nret = 1

  end function thing_get



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




    !! local functions
    subroutine receive_1D_arr_int( lua, c, arr )
      type( c_ptr ), intent(in), value :: lua
      integer( c_int ), intent(in) :: c
      integer, dimension(c), intent(inout) :: arr

      integer :: i
      real( lua_number ) :: m
      integer( c_int ) :: p

      do i = 1, c
         p = lua_rawgeti( lua, -1, int(i, lua_integer) )
         m = lua_tonumber(lua, -1)
         arr(i) = int(m)
         call lua_pop(lua, 1)
      end do

      !do i = 1, c
      !   write(*,*) i, arr(i)
      !end do

    end subroutine receive_1D_arr_int

    subroutine receive_1D_arr( lua, c, arr )
      type( c_ptr ), intent(in), value :: lua
      integer( c_int ), intent(in) :: c
      real( c_float ), dimension(c), intent(inout) :: arr

      integer :: i
      real( lua_number ) :: m
      integer( c_int ) :: p

      do i = 1, c
         p = lua_rawgeti( lua, -1, int(i, lua_integer) )
         m = lua_tonumber(lua, -1)
         arr(i) = real(m, c_double )
         call lua_pop(lua, 1)
      end do

      !do i = 1, c
      !   write(*,*) i, arr(i)
      !end do

    end subroutine receive_1D_arr

    subroutine receive_2D_arr( lua, c, r, arr )
      type( c_ptr ), intent(in), value :: lua
      integer( c_int ), intent(in) :: c,r
      real( c_float ), dimension(c,r), intent(inout) :: arr

      integer :: i, j
      real( lua_number ) :: m
      integer( c_int ) :: p

      do i = 1, r
         p = lua_rawgeti( lua, -1, int(i, lua_integer) )
         do j = 1, c
            p = lua_rawgeti( lua, -1, int(j, lua_integer) )
            m = lua_tonumber(lua, -1)
            arr(j,i) = real(m, c_double )
            call lua_pop(lua, 1)
         end do
         call lua_pop(lua, 1)
      end do

      ! do i = 1, r
      !   write(*,*) i, arr(:,i)
      ! end do

      ! call print_stack(lua)
      ! write(*,*) "top", lua_gettop(lua)
    end subroutine receive_2D_arr


    subroutine receive_2D_arr_int( lua, c, r, arr )
      type( c_ptr ), intent(in), value :: lua
      integer( c_int ), intent(in) :: c,r
      integer, dimension(c,r), intent(inout) :: arr

      integer :: i, j
      real( lua_number ) :: m
      integer( c_int ) :: p

      do i = 1, r
         p = lua_rawgeti( lua, -1, int(i, lua_integer) )
         do j = 1, c
            p = lua_rawgeti( lua, -1, int(j, lua_integer) )
            m = lua_tonumber(lua, -1)
            arr(j,i) = int(m)
            call lua_pop(lua, 1)
         end do
         call lua_pop(lua, 1)
      end do

      ! do i = 1, r
      !   write(*,*) i, arr(:,i)
      ! end do

      ! call print_stack(lua)
      ! write(*,*) "top", lua_gettop(lua)
    end subroutine receive_2D_arr_int

    subroutine send_1D_arr_int(lua, c, arr)
      type( c_ptr ), intent(in), value :: lua
      integer( c_int ), intent(in) :: c
      integer( c_int ), dimension(c), intent(in) :: arr
      integer(c_int) :: nret

      integer :: i

      call lua_newtable(lua)

      do i = 1, c
         call lua_pushinteger(lua, int(arr(i), lua_integer) )
         call lua_rawseti(lua, -2, int(i, lua_integer) )
      end do

      nret = 1
    end subroutine send_1D_arr_int

    subroutine send_1D_arr(lua, c, arr)
      type( c_ptr ), intent(in), value :: lua
      integer( c_int ), intent(in) :: c
      real( c_float ), dimension(c), intent(in) :: arr
      integer(c_int) :: nret

      integer :: i

      call lua_newtable(lua)

      do i = 1, c
         call lua_pushnumber(lua, real(arr(i), lua_number) )
         call lua_rawseti(lua, -2, int(i, lua_integer) )
      end do

      nret = 1
    end subroutine send_1D_arr

    subroutine send_2D_arr_int(lua, c, r, arr)
      type( c_ptr ), intent(in), value :: lua
      integer( c_int ), intent(in) :: r, c
      integer( c_int ), dimension(c,r), intent(in) :: arr
      integer(c_int) :: nret

      integer :: i, j

      call lua_newtable(lua)

      do i = 1, r
         call lua_newtable(lua)
         do j = 1, c
            call lua_pushinteger(lua, int(arr(j,i), lua_integer) )
            call lua_rawseti(lua, -2, int(j, lua_integer) )
         end do
         call lua_rawseti(lua, -2, int( i, lua_integer) )
      end do

      nret = 1
    end subroutine send_2D_arr_int

    subroutine send_2D_arr(lua, c, r, arr)
      type( c_ptr ), intent(in), value :: lua
      integer( c_int ), intent(in) :: r, c
      real( c_float ), dimension(c,r), intent(in) :: arr
      integer(c_int) :: nret

      integer :: i, j

      call lua_newtable(lua)

      do i = 1, r
         call lua_newtable(lua)
         do j = 1, c
            call lua_pushnumber(lua, real(arr(j,i), lua_number) )
            call lua_rawseti(lua, -2, int(j, lua_integer) )
         end do
         call lua_rawseti(lua, -2, int( i, lua_integer) )
      end do

      nret = 1
    end subroutine send_2D_arr



end module interf_lua
