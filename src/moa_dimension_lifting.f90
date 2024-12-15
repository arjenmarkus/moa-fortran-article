! moa_dimension_lifting.f90 --
!     A sort of dimension lifting à la MoA?
!
program moa_dimension_lifting
    implicit none

    integer, pointer                  :: p(:,:), q(:,:,:)
    integer, dimension(200), target   :: array
    integer, dimension(20,10), target :: array2d

    p(1:10,1:20) => array
    q(1:8,1:5,1:5) => array

    write(*,*) 'Shape array:     ', shape(array)
    write(*,*) 'Shape pointer p: ', shape(p)
    write(*,*) 'Shape pointer q: ', shape(q)

    q(1:2,1:5,1:5) => array
    write(*,*) 'Shape pointer q: ', shape(q)

    q(1:8,1:5,1:5) => array2d
    write(*,*) 'Shape pointer q: ', shape(q)

end program moa_dimension_lifting
