#gcc -c printstack.c
gfortran -c lua.f90
#gfortran -shared -fPIC -o interf_lua.so interf_lua.f90 lua.o printstack.o -llua5.3 ../lib/libthemodule.so 
gfortran -shared -fPIC -o interf_lua.so interf_lua.f90 lua.o -llua5.3 ../lib/libthemodule.so 

