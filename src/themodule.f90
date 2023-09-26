submodule( themodule_h )themodule_routines

  implicit none

contains

  module function thing_constructor()result(this)

    type( t_thing ), pointer :: this

    ! write(*,*) "in thing_constructor"

    allocate( t_thing :: this )

    this% i = 0
    this% r = 0.0

    ! write(*,*) "thing_constructor complete"

  end function thing_constructor


  module subroutine thing_destructor( self )
    type( t_thing ), intent(inout) :: self

    write(*,*) "in thing_destructor"
    self% r = 0.0
    if( allocated( self% int1d) ) deallocate( self% int1d )
    if( allocated( self% int2d) ) deallocate( self% int2d )
    if( allocated( self% real1d) ) deallocate( self% real1d )
    if( allocated( self% real2d) ) deallocate( self% real2d )
  end subroutine thing_destructor



  module subroutine print( self )
    !! member routine of t_thing
    implicit none
    class( t_thing ), intent(inout) :: self

    integer :: i


    write(*,*) repeat('=',40)
    write(*,*) "PRINT THING FROM F:"


    write(*,*) "i = ", self% i

    if( allocated( self% int1d) ) then
       write(*,'(a,x,*(i3,2x))') "int1d = ", self% int1d
    end if

    if( allocated( self% int2d)) then
       do i = 1, size( self% int2d, 2)
          write(*,'(a,x,i2,":",5x,*(i3,2x))') "int2d(:,i)",i, self% int2d(:,i)
       end do
    endif


    write(*,*) "r = ", self% r

    if( allocated( self% real1d)) then
       write(*,'(a,x,*(f9.5))') "real1d = ", self% real1d
    end if

    if( allocated( self% real2d)) then
       do i = 1, size( self% real2d, 2)
          write(*,'(a,x,i2,":",5x,*(f9.5,2x))') "real2d(:,i)", i, self% real2d(:,i)
       end do
    end if

    if( allocated( self% string)) then
       write(*,*) "string = ",self% string
    end if

    write(*,*) repeat('=',40)


    ! write(*,*) "exiting thing_print"

  end subroutine print


  module subroutine call_lone( self )
    !! call the lonely routine with data from module
    class( t_thing ), intent(inout) :: self
    integer :: m, n
    m = size( self% real2d, 1)
    n = size( self% real2d, 2)

    if( .not. allocated( self% real2d)) then
       write(*,*) "cannot call lone_routine, real2d data is not present!"
       return
    end if

    call lone_routine( m, n, self% real2d )
  end subroutine call_lone


  module function get_datatype( self, name ) result( dtype )
    !! return the encoded datatype value
    use iso_c_binding, only: c_int
    use thing_datatype
    implicit none
    class( t_thing ), intent(inout) :: self
    character(*),     intent(in) :: name

    integer( c_int ) :: dtype

    !! based on name of variable, set value to dtype
    select case( name )
    case( 'i' );     dtype = THING_DATA_INT
    case( 'int1d' ); dtype = THING_DATA_INT_1D
    case( 'int2d' ); dtype = THING_DATA_INT_2D
    case( 'r' );      dtype = THING_DATA_REAL
    case( 'real1d' ); dtype = THING_DATA_REAL_1D
    case( 'real2d' ); dtype = THING_DATA_REAL_2D
    case( 'string' ); dtype = THING_DATA_STR


    case default
       dtype = THING_DATA_UNKNOWN

    end select

  end function get_datatype

  module function get_datasize( self, name ) result( c_sizeptr )
    !! encode the size of wanted data into c_ptr of 2 dimensions
    use iso_c_binding, only: c_ptr, c_loc
    implicit none
    class( t_thing ), intent(inout) :: self
    character(*), intent(in) :: name
    type( c_ptr ) :: c_sizeptr

    integer, dimension(:), pointer :: f_sizeptr
    integer :: dim1, dim2

    dim1 = 1; dim2 = 1
    allocate( f_sizeptr(1:2) )

    select case( name )
    case( 'i', 'r' )
    case( 'int1d' ); dim1 = size( self% int1d, 1)
    case( 'int2d' ); dim1 = size( self% int2d, 1); dim2 = size(self% int2d, 2)
    case( 'real1d' ); dim1 = size( self% real1d, 1)
    case( 'real2d' ); dim1 = size( self% real2d, 1); dim2 = size(self% real2d, 2)
    ! case( 'string' ); dim1=len_trim( self% string ); dim2=1
    case( 'string' ); dim1=1; dim2=1

    case default
       write(*,*) "UKNOWN NAME IN get_datasize:",name
    end select

    f_sizeptr(1) = dim1
    f_sizeptr(2) = dim2
    !! If we need more dimensional arrays, add values here 'dim3', 'dim4' etc.
    !! In that case don't forget to increase the shape of c_sizeptr everywhere!

    c_sizeptr = c_loc( f_sizeptr(1) )

  end function get_datasize


  module function get_thing( self, name ) result( c_dataptr )
    !! encode wanted data into c_ptr, change the type to C-compatible (single prec)
    use iso_c_binding
    use m_tools
    implicit none
    class( t_thing ), intent(inout) :: self
    character(*),     intent(in) :: name
    type( c_ptr ) :: c_dataptr

    integer( c_int ),                 pointer :: int_ptr
    integer( c_int ), dimension(:),   pointer :: int1d_ptr
    integer( c_int ), dimension(:,:), pointer :: int2d_ptr
    real( c_float ),                 pointer :: real_ptr
    real( c_float ), dimension(:),   pointer :: real1d_ptr
    real( c_float ), dimension(:,:), pointer :: real2d_ptr

    integer :: m, n

    select case( name )
    case( 'i' )
       nullify( int_ptr )
       allocate( int_ptr )
       int_ptr = int( self% i, c_int )
       c_dataptr = c_loc( int_ptr )
    case( 'int1d' )
       m = size( self% int1d, 1)
       nullify( int1d_ptr )
       allocate( int1d_ptr(1:m) )
       int1d_ptr = int( self% int1d, c_int )
       c_dataptr = c_loc( int1d_ptr(1) )
    case( 'int2d' )
       m = size( self% int2d, 1)
       n = size( self% int2d, 2)
       nullify( int2d_ptr )
       allocate( int2d_ptr(1:m, 1:n) )
       int2d_ptr = int( self% int2d, c_int )
       c_dataptr = c_loc( int2d_ptr(1,1) )
    case( 'r' )
       nullify(real_ptr )
       allocate( real_ptr )
       real_ptr = real( self% r, c_float )
       c_dataptr = c_loc( real_ptr )
    case( 'real1d' )
       m = size( self% real1d, 1 )
       nullify( real1d_ptr )
       allocate( real1d_ptr(1:m) )
       real1d_ptr = real( self% real1d, c_float )
       c_dataptr = c_loc( real1d_ptr(1) )
    case( 'real2d' )
       m = size( self% real2d, 1 )
       n = size( self% real2d, 2 )
       nullify( real2d_ptr )
       allocate( real2d_ptr(1:m, 1:n) )
       real2d_ptr = real( self% real2d, c_float )
       c_dataptr = c_loc( real2d_ptr(1,1) )
    case( 'string' )
       c_dataptr = f2c_string( self% string )
    case default
       write(*,*) "UNKNOWN name in get_thing:",name

    end select


  end function get_thing




  module subroutine set_thing( self, name, c_sizeptr, c_dataptr )
    use iso_c_binding
    use thing_datatype
    use m_tools
    implicit none

    class( t_thing ) :: self
    character(*) :: name
    ! integer,          intent(in) :: dtype
    type( c_ptr ) :: c_sizeptr
    type( c_ptr ) :: c_dataptr

    integer( c_int ) :: dtype
    integer,                 pointer :: int_ptr
    integer, dimension(:),   pointer :: int1d_ptr
    integer, dimension(:,:), pointer :: int2d_ptr
    real,                 pointer :: real_ptr
    real, dimension(:),   pointer :: real1d_ptr
    real, dimension(:,:), pointer :: real2d_ptr
    character(len=:), allocatable :: fstring

    integer, dimension(:), pointer :: f_sizeptr
    integer :: dim1, dim2

    ! write(*,*) "name:",name
    dtype = get_datatype( self, name )

    ! write(*,*) "dtype",dtype

    !! decode the size
    call c_f_pointer( c_sizeptr, f_sizeptr, [2] )
    dim1 = f_sizeptr(1); dim2 = f_sizeptr(2)

    ! write(*,*) "dim:",dim1, dim2
    ! write(*,*) "f_sizeptr",f_sizeptr

    !! cast the c pointer to f, based on type
    select case( dtype )
    case( THING_DATA_INT )
       nullify( int_ptr )
       call c_f_pointer( c_dataptr, int_ptr )

    case( THING_DATA_INT_1D )
       nullify( int1d_ptr )
       call c_f_pointer( c_dataptr, int1d_ptr, [dim1] )

    case( THING_DATA_INT_2D )
       nullify( int2d_ptr )
       call c_f_pointer( c_dataptr, int2d_ptr, [dim1, dim2] )

    case( THING_DATA_REAL )
       nullify( real_ptr )
       call c_f_pointer( c_dataptr, real_ptr )

    case( THING_DATA_REAL_1D )
       nullify( real1d_ptr )
       call c_f_pointer( c_dataptr, real1d_ptr, [dim1] )

    case( THING_DATA_REAL_2D )
       nullify( real2d_ptr )
       call c_f_pointer( c_dataptr, real2d_ptr, [dim1, dim2] )

    case( THING_DATA_STR )
       fstring = c2f_string( c_dataptr )

    case( THING_DATA_UNKNOWN )
       write(*,*) "UNKNOWN DATA TYPE FOR NAME:", name

    case default
       write(*,*) "Invalid dtype value:", dtype

    end select

    !! assign the value to self, based on name
    select case( name )
    case( "i" )
       self% i = int_ptr

    case( "int1d" )
       if( allocated(self% int1d) ) deallocate(self% int1d)
       allocate( self% int1d, source = int1d_ptr )

    case( "int2d" )
       if( allocated(self% int2d) ) deallocate( self% int2d)
       allocate( self% int2d, source = int2d_ptr )

    case( "r" )
       self% r = real(real_ptr)

    case( "real1d" )
       if( allocated(self% real1d) ) deallocate(self% real1d)
       allocate( self% real1d, source = real(real1d_ptr) )

    case( "real2d" )
       if( allocated(self% real2d) ) deallocate( self% real2d)
       allocate( self% real2d, source = real(real2d_ptr) )

    case( "string" )
       if( allocated( self% string) ) deallocate( self% string)
       allocate( self% string, source=fstring)

    case default
       write(*,*) "Unknown name in set_thing:", name

    end select

  end subroutine set_thing

end submodule themodule_routines
