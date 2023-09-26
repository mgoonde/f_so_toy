program main

  use thing_interface
  ! use iso_c_binding
  use iso_c_binding

  implicit none

  type( thing ) :: me
  integer, dimension(5) :: v
  real, allocatable :: r2d(:,:)
  integer :: i

  !! the variables which will be extracted from module need to be declared as pointer
  !! The kind() of these vars needs to be consistent to output vars when overloading the
  !! assignments in the thing_interface.f90
  real, pointer :: p
  integer, dimension(:), pointer :: vv
  real, dimension(:,:), pointer :: rp
  real, dimension(:), pointer :: r1
  character(:), allocatable :: ss


  !! initialize
  me = thing()

  call me% set("string","this is a string HERE")
  ! call me% print()

  call me% set("i",9)
  ! call me% print()

  call me% set( "r", 5.0)
  ! call me% print()

  v = (/1,2,3,3,9/)
  call me% set( "int1d", v )
  ! call me% print()

  call me% set("real1d", [1.2, 2.2, 3.2])

  allocate( r2d(1:3,1:2) )
  r2d(:,1) = (/ 11.1, 22.1, 33.1 /)
  r2d(:,2) = (/ 11.2, 22.2, 33.2 /)
  call me% set( "real2d", r2d)


  v = (/ 3,4,5,2,6 /)
  call me% set("int1d", v)


  call me% print()

  p = me% get( "r" )
  write(*,*) "got real",p

  vv = me% get( "int1d" )
  write(*,*) "got int1d",vv

  ss = me% get("string")
  write(*,*) "got string:",ss

  r1 = me% get( "real1d" )
  write(*,*) "got real1d:", r1(:)

  rp = me% get( "real2d" )
  write(*,*) "got real2d:"
  do i = 1, size(rp,2)
     write(*,*) rp(:,i)
  end do
  i = me% command("this is a command from fortran_application!")

  !! deallocate
  call me% close()

  deallocate(p)
  deallocate( r2d )
  deallocate( vv )
end program main
