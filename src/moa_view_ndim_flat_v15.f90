!===============================================================================
! Module for generic views on arrays and scalars
! Last edited: Jan 10, 2022 (AM)
!
! TODO:
! - Use PCT
!
! Note:
! This is a reimplementaion, relying on a flat data structure instead of
! a recursive structure
!
! Note 2:
! Profiling has shown that the hotspot is in getting the first dimension of the
! stored arrays (in the moa_basic_view component). To alleviate that store
! the first dimension within the moa_view_type itself.
!
! Note 3:
! This version has no range checks or other sanity checks. Thus the run-time
! is hopefully more comparable to the plain implementation via arrays.
!
! Note 4:
! This version caches the index of the section that was used last in an attempt
! to speed up the process of finding the right section and returning the pointer
!
! Note 5:
! Use findloc() to find the right part instead of an explicit loop
!
! Note 6:
! Use binary search instead of findloc().
!
! Note 7:
! Instead of findindex() I assume the chunks are all the same size. This makes it
! possible to calculate directly which chunk to pick.
! Also drop the use of get_corrected_index() - we know it should be 1.
!
! Note 8:
! The change described in note 7 had very little or no effect. I realised that
! I may be looking at the wrong part of the code: this version replaces the
! call to elem_of() by a direct pointer assignment.
!
! Note 9:
! The changes with note 8 made a difference: 10% faster. So, what if I chisel
! away a few more things?
!===============================================================================
MODULE moa_view_types
    USE iso_c_binding
    USE moa_basic_view_types

    IMPLICIT NONE

    PRIVATE
    PUBLIC :: moa_view_type, operator(//), size, shape, rank

    !
    ! Type for defining "views" on arrays - to allow for a#b#c: POINTERs to other views
    !
    TYPE :: moa_view_type
        INTEGER                           :: cached = 1
        INTEGER, ALLOCATABLE              :: prev_elem(:)
        TYPE(moa_basic_view), ALLOCATABLE :: array(:)
    CONTAINS
         generic           :: elem        => elem_single, elem_ndim
         PROCEDURE         :: elem_single => get_elem_single
         PROCEDURE         :: elem_ndim   => get_elem_ndim
    END TYPE moa_view_type

    INTERFACE operator(//)
        MODULE PROCEDURE catenate_array_array, catenate_array_view, catenate_view_array, catenate_view_view
    END INTERFACE

    INTERFACE size
        MODULE PROCEDURE size_view
    END INTERFACE

    INTERFACE shape
        MODULE PROCEDURE shape_view
    END INTERFACE

    INTERFACE rank
        MODULE PROCEDURE rank_view
    END INTERFACE
CONTAINS
FUNCTION catenate_array_array( array1, array2 ) result(new_view)
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array1
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array2
    CLASS(moa_view_type),  ALLOCATABLE         :: new_view

    CALL check_shapes( SHAPE(array1), SHAPE(array2) )

    ALLOCATE( new_view )
    ALLOCATE( new_view%array(2) )

    new_view%prev_elem = [0, SIZE(array1,1), SIZE(array1,1) + SIZE(array2,1)]

    CALL point_to_any_rank( new_view%array(1), array1 )
    CALL point_to_any_rank( new_view%array(2), array2 )
END FUNCTION catenate_array_array

FUNCTION catenate_view_array( view1, array2 ) result(new_view)
    CLASS(moa_view_type), TARGET, INTENT(IN)   :: view1
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array2
    CLASS(moa_view_type),  ALLOCATABLE         :: new_view

    INTEGER                                    :: new_size

    CALL check_shapes( SHAPE(view1), SHAPE(array2) )

    new_size = SIZE(view1%array) + 1
    ALLOCATE( new_view )
    ALLOCATE( new_view%array(new_size) )

    new_view%prev_elem = [view1%prev_elem, view1%prev_elem(new_size) + SIZE(array2,1)]

    new_view%array(1:new_size-1) = view1%array
    CALL point_to_any_rank( new_view%array(new_size), array2 )

END FUNCTION catenate_view_array

FUNCTION catenate_array_view( array1, view2 ) result(new_view)
    INTEGER, DIMENSION(..), TARGET, INTENT(IN) :: array1
    CLASS(moa_view_type), TARGET, INTENT(IN)   :: view2
    CLASS(moa_view_type),  ALLOCATABLE         :: new_view

    INTEGER                                    :: new_size

    CALL check_shapes( SHAPE(array1), SHAPE(view2) )

    new_size = SIZE(view2%array) + 1
    ALLOCATE( new_view )
    ALLOCATE( new_view%array(new_size) )

    new_view%prev_elem = [0, SIZE(array1,1) + view2%prev_elem]

    CALL point_to_any_rank( new_view%array(1), array1 )
    new_view%array(2:new_size) = view2%array

END FUNCTION catenate_array_view

FUNCTION catenate_view_view( view1, view2 ) result(new_view)
    CLASS(moa_view_type), TARGET, INTENT(IN) :: view1
    CLASS(moa_view_type), TARGET, INTENT(IN) :: view2
    CLASS(moa_view_type),  ALLOCATABLE       :: new_view

    INTEGER                                  :: new_size
    INTEGER                                  :: last

    CALL check_shapes( SHAPE(view1), SHAPE(view2) )

    new_size = SIZE(view1%array) + SIZE(view2%array)
    ALLOCATE( new_view )
    ALLOCATE( new_view%array(new_size) )

    last = SIZE(view1%array) + 1
    new_view%prev_elem = [view1%prev_elem, view1%prev_elem(last) + view2%prev_elem]

    new_view%array(1:SIZE(view1%array))  = view1%array
    new_view%array(SIZE(view1%array)+1:) = view2%array

END FUNCTION catenate_view_view

! NOTE: further simplification is possible!!

FUNCTION get_elem_ndim(view, idx) result(elem)
    CLASS(moa_view_type), INTENT(INOUT) :: view
    INTEGER, DIMENSION(:), INTENT(IN)   :: idx
    INTEGER, POINTER                    :: elem

    logical                             :: found

    INTEGER, DIMENSION(SIZE(idx))       :: idxr
    INTEGER                             :: sz
    INTEGER                             :: firstidx
    INTEGER                             :: chunk
    INTEGER                             :: cnt

    !!found = .FALSE.

    !!idxr = idx

    !
    ! Check if the new index is in the same section as the previously accessed index
    !
    IF ( view%prev_elem(view%cached) < idx(1) .and. idx(1) <= view%prev_elem(view%cached+1) ) then
        !!found           =  .TRUE.
        !!firstidx        =  1 !!get_corrected_index(idxr, view%array(view%cached))
        !!idxr(firstidx)  =  idxr(firstidx) - view%prev_elem(view%cached)
        !!elem            => elem_of( view%array(view%cached), idxr(firstidx:))
        !!elem            => view%array(view%cached)%array(idxr(1))
        elem            => view%array(view%cached)%array(idx(1) - view%prev_elem(view%cached) )
    ELSE
        !CALL findindex( view%prev_elem, idx(1), chunk, cnt )
        chunk           = ( idx(1) + view%prev_elem(2) - 1 ) / view%prev_elem(2)
        !!found           =  .TRUE.
        !!firstidx        =  1 !!get_corrected_index(idxr, view%array(chunk))
        !!idxr(firstidx)  =  idxr(firstidx) - view%prev_elem(chunk)
        !!elem            => elem_of( view%array(chunk), idxr(firstidx:))
        !!elem            => view%array(view%cached)%array(idxr(1))
        elem            => view%array(chunk)%array(idx(1) - view%prev_elem(chunk) )

        view%cached     =  chunk
    ENDIF
END FUNCTION get_elem_ndim

FUNCTION get_corrected_index( idx, array ) result(firstidx)
    INTEGER, DIMENSION(:), INTENT(IN), TARGET :: idx
    CLASS(moa_basic_view), INTENT(IN)         :: array

    INTEGER                                   :: firstidx

    IF ( SIZE(idx) /= SIZE(array%shp) ) THEN
        IF ( SIZE(idx) == RANK(array) + 1 .and. idx(1) == 1 ) THEN
            firstidx = 2
        ELSE
            ERROR STOP
        ENDIF
    ELSE
        firstidx = 1
    ENDIF

END FUNCTION get_corrected_index

FUNCTION get_elem_single(view, i) result(elem)
    CLASS(moa_view_type), INTENT(INOUT) :: view
    INTEGER, INTENT(IN)                 :: i
    INTEGER, POINTER                    :: elem

    INTEGER, DIMENSION(1)               :: inew

    inew =  i
    elem => get_elem_ndim( view, inew )
END FUNCTION

SUBROUTINE findindex( lastvalue, idx, intv, cnt )
    INTEGER, INTENT(IN)  :: lastvalue(0:)
    INTEGER, INTENT(IN)  :: idx
    INTEGER, INTENT(OUT) :: intv
    INTEGER, INTENT(OUT) :: cnt

    INTEGER              :: first, last, middle

    cnt  = 0
    intv = 0

    first = 1
    last  = ubound(lastvalue,1)
    middle = (first + last) / 2

    !write(*,*) 'First, last:', first, last, ' -- idx = ', idx

    DO WHILE ( first <= last )
        cnt = cnt + 1
        !write(*,*) 'middle:', middle
        IF ( lastvalue(middle) < idx ) THEN
            first = middle + 1
        ELSEIF (lastvalue(middle-1) < idx .and. lastvalue(middle) >= idx ) THEN
            EXIT
        ELSE
            last = middle - 1
        ENDIF

        middle = (first + last) / 2
    ENDDO

    intv = middle
END SUBROUTINE findindex




INTEGER FUNCTION size_view( view )
    CLASS(moa_view_type), INTENT(IN) :: view

    size_view = product(SHAPE(view))
END FUNCTION size_view

FUNCTION shape_view( view )
    CLASS(moa_view_type), INTENT(IN)    :: view

    INTEGER, DIMENSION(:),  ALLOCATABLE :: shape_view
    INTEGER, DIMENSION(:),  ALLOCATABLE :: shp
    INTEGER                             :: i
    INTEGER                             :: rnk

    rnk = RANK(view)
    ALLOCATE( shape_view(rnk) )
    shape_view = 0

    IF ( .NOT. ALLOCATED(view%array) ) THEN
        RETURN
    ENDIF

    shape_view = view%array(1)%shp
    !write(*,*) '>> shape:', shape_view, '|', view%array(1)%shp, '|', view%array(1)%array
    DO i = 2,SIZE(view%array)
        shp           = view%array(i)%shp
        shape_view(1) = shape_view(1) + shp(1)
        !write(*,*) '>> shape:', i, shape_view
    ENDDO

END FUNCTION shape_view

recursive INTEGER FUNCTION rank_view( view )
    TYPE(moa_view_type), INTENT(IN) :: view

    IF ( ALLOCATED(view%array) ) THEN
        IF ( SIZE(view%array) > 0 ) THEN
            rank_view = SIZE(view%array(1)%shp)
        ELSE
            rank_view = 0
        ENDIF
    ELSE
        rank_view = 0
    ENDIF

END FUNCTION rank_view

SUBROUTINE check_shapes( shp1, shp2 )
    INTEGER, DIMENSION(:), INTENT(IN) :: shp1
    INTEGER, DIMENSION(:), INTENT(IN) :: shp2

    INTEGER                           :: i, offset1, offset2, first, last

    IF ( abs( SIZE(shp1) - SIZE(shp2) ) > 1 ) THEN
        !!WRITE(*,*) 'Incorrect combination of arrays - ranks differ more than 1'
        error stop
    ENDIF

    offset1 = 0
    offset2 = 0
    first   = 2
    last    = max( SIZE(shp1), SIZE(shp2) )

    IF ( SIZE(shp1) > SIZE(shp2) ) THEN
        offset2 = -1
    ENDIF

    IF ( SIZE(shp1) < SIZE(shp2) ) THEN
        offset1 = -1
    ENDIF

    do i = first,last
        IF ( shp1(i+offset1) /= shp2(i+offset2) ) THEN
            !!WRITE(*,*) 'Incorrect combination of arrays - shapes differ after first DIMENSION'
            error stop
        ENDIF
    enddo
END SUBROUTINE check_shapes

END MODULE moa_view_types
