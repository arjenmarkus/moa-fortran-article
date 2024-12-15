gcc      -c getresources_f.c -march=native -mtune=native
gfortran -o copy_and_extend_v2 copy_and_extend_v2.f90 getresources_f.o -fbacktrace -mtune=native
