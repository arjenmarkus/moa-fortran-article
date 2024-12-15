gcc      -c getresources_f.c
gfortran -c view_general_v6.f90
gfortran -c moa_view_ndim_flat_v15.f90
gfortran -o copy_and_extend_view copy_and_extend_view.f90 view_general_v6.o moa_view_ndim_flat_v15.o getresources_f.o -fbacktrace -mtune=native
