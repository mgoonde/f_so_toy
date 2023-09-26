subroutine lone_routine( m, n, r2d )

  !! a standalone routine
  implicit none

  integer, intent(in) :: m, n
  real, dimension(m, n), intent(inout) :: r2d

  write(*,*) "this is inside the lonely routine"
  r2d(1,1) = 0.123
end subroutine lone_routine
