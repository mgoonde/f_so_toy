module thing_datatype

  use iso_c_binding, only: c_int
  implicit none
  public

  integer( c_int ), parameter :: &
       THING_DATA_UNKNOWN = 0, &
       THING_DATA_INT     = 1, &
       THING_DATA_INT_1D  = 2, &
       THING_DATA_INT_2D  = 3, &
       THING_DATA_REAL    = 4, &
       THING_DATA_REAL_1D = 5, &
       THING_DATA_REAL_2D = 6, &
       THING_DATA_STR     = 7

end module thing_datatype
