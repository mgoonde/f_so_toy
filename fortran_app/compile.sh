gfortran -c thing_interface.f90
#gfortran -c main.f90
gfortran -o main.x main.f90 thing_interface.o ../lib/libthemodule.so
