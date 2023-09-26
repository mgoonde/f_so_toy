module themodule_h

  implicit none

  public :: t_thing

  private

  type :: t_thing
     !! holds these variables
     integer :: i
     integer, allocatable :: int1d(:), int2d(:,:)
     real :: r
     real, allocatable :: real1d(:), real2d(:,:)
     character(:), allocatable :: string

     !! pointers to other objects
     ! type( t_mammal ), pointer :: mammal
     ! type( t_reptile ), pointer :: reptile

   contains
     ! procedure :: init => thing_init
     procedure :: print
     procedure :: call_lone
     procedure :: get_datatype
     procedure :: get_datasize
     procedure :: get_thing
     procedure :: set_thing
     final :: thing_destructor
  end type t_thing

  !! overload the type with constructor
  interface t_thing
     module procedure :: thing_constructor
  end interface t_thing

  interface
     module function thing_constructor()result(this)
       type( t_thing ), pointer :: this
     end function thing_constructor

     module subroutine thing_destructor( self )
       type( t_thing ), intent(inout) :: self
     end subroutine thing_destructor


     module subroutine call_lone( self )
       class( t_thing ), intent(inout) :: self
     end subroutine call_lone

     module subroutine print( self )
       class( t_thing ), intent(inout) :: self
     end subroutine print

     module function get_datatype( self, name ) result( dtype )
       use iso_c_binding, only: c_int
       class( t_thing ), intent(inout) :: self
       character(*),     intent(in) :: name
       integer( c_int ) :: dtype
     end function get_datatype

     module function get_datasize( self, name ) result( c_sizeptr )
       use iso_c_binding, only: c_ptr, c_loc
       class( t_thing ), intent(inout) :: self
       character(*), intent(in) :: name
       type( c_ptr ) :: c_sizeptr
     end function get_datasize

     module function get_thing( self, name ) result( c_dataptr )
       use iso_c_binding, only: c_ptr, c_int
       implicit none
       class( t_thing ), intent(inout) :: self
       character(*),     intent(in) :: name
       integer( c_int ) :: dtype
       type( c_ptr ) :: c_sizeptr
       type( c_ptr ) :: c_dataptr
     end function get_thing

     module subroutine set_thing( self, name, c_sizeptr, c_dataptr )
     use iso_c_binding, only: c_ptr, c_int
       class( t_thing ) :: self
       character(*)    :: name
       type( c_ptr )   :: c_sizeptr
       type( c_ptr )   :: c_dataptr
     end subroutine set_thing

  end interface

contains

end module themodule_h


