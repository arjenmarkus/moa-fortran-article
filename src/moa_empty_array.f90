! moa_empty_array.f90 --
!     Empty arrays à la MoA?
!
program moa_empty_array
    implicit none

    integer, dimension(:,:,:), allocatable :: array

    allocate( array(0,0,3) )

    write(*,*) 'Size: ', size(array)
    write(*,*) 'Shape:', shape(array)

end program moa_empty_array
