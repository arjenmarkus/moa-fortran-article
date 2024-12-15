gcc      -c getresources_f.c -march=native -mtune=native -O2
gfortran -c cmdparse.f90 -march=native -mtune=native -O2
gfortran -c view_general_v6.f90 -march=native -mtune=native -O2
gfortran -c moa_view_ndim_flat_v15.f90 -march=native -mtune=native -O2
gfortran -o test_moa_measure_pnt moa_measure_v2.f90 moa_view_ndim_flat_v15.o view_general_v6.o cmdparse.o getresources_f.o -march=native -mtune=native -O2
