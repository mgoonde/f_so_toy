module m_type

  use iso_c_binding
  implicit none

  private
  public :: mytype

  !! derived type with bind(C)
  type, bind(C) :: mytype
     real( c_float ) :: a
     integer( c_int ) :: m, n
     real( c_float ), dimension(3,3) :: r
     character(c_char) :: cstr(200)
     !! C-bound derived types cannot have allocatable arrays
     !! C-bound derived type cannot have a 'contains' part in the definition;
     !! put the routines under the 'contains' of the whole module
  end type mytype

contains


  subroutine simple_proc( mt ) bind(C)
    type( mytype ), intent(inout) :: mt
    integer :: i

    write(*,*) mt% a
    write(*,*) mt% cstr

  end subroutine simple_proc


end module m_type
