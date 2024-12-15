! copy_and_extend.f90 --
!     Measure teh performance of extending (and thereby copying) arrays
!
MODULE monitoring
    USE iso_c_binding
    USE iso_fortran_env

    interface
        subroutine getresources( timeused, counts ) bind(C)
            import :: c_double, c_long
            real(kind=c_double), intent(out)  :: timeused(*)
            integer(kind=c_long), intent(out) :: counts(*)
        end subroutine getresources
    end interface

    REAL(KIND=c_double), dimension(2)   :: timeused
    INTEGER(KIND=c_long), dimension(14) :: counts

    INTEGER                             :: time_first, time_last, time_zero, time_rate
    REAL                                :: cpu_first, cpu_last

    REAL(KIND=c_double), dimension(2)   :: timeused_new
    INTEGER(KIND=c_long), dimension(14) :: counts_new

    INTEGER                            :: lurep = OUTPUT_UNIT, lucsv, luoverall

CONTAINS

! write_csv_header
!     Write the header for the CSV file
!
! Arguments:
!     None
!
SUBROUTINE write_csv_header( extra )
    LOGICAL, INTENT(IN) :: extra

    IF ( EXTRA ) THEN
        WRITE( luoverall, '(100(a,'',''))') 'Chunk sizes', 'Number chunks', &
            'Wall clock', 'CPU time', 'User time', 'System time', 'Maximum memory', 'Maximum text', 'Unshared data', &
            'Stack space', 'Minor fault', 'Major fault', 'Swaps', 'Inblock',  'Outblock', 'Messages sent', 'Message received', &
            'Signals', 'Voluntary switch', 'Involuntary switch'
    ELSE
        WRITE( lucsv, '(100(a,'',''))') &
            'Wall clock', 'CPU time', 'User time', 'System time', 'Maximum memory', 'Maximum text', 'Unshared data', &
            'Stack space', 'Minor fault', 'Major fault', 'Swaps', 'Inblock',  'Outblock', 'Messages sent', 'Message received', &
            'Signals', 'Voluntary switch', 'Involuntary switch'
    ENDIF

END SUBROUTINE write_csv_header

! start_timer
!     Initialise the time, so that we can keep track of how much wall clock/system time is spent
!
! Arguments:
!     None
!
SUBROUTINE start_timer

    CALL system_clock( count = time_first, count_rate = time_rate )
    CALL cpu_time( cpu_first )

    CALL getresources( timeused, counts )

END SUBROUTINE start_timer

! stop_timer
!     Measure the elapsed time and report it
!
! Arguments:
!     None
!
SUBROUTINE stop_timer

    CALL getresources( timeused_new, counts_new )

    CALL system_clock( count = time_last )
    CALL cpu_time( cpu_last )

    WRITE( lurep, '(a,g12.6)' ) 'Wall clock (s): ', (time_last - time_first) / real(time_rate)
    WRITE( lurep, '(a,g12.6)' ) 'CPU time (s):   ',  cpu_last  - cpu_first

    WRITE( lucsv, '(100(g0.6,'',''))' ) &
        (time_last - time_first) / real(time_rate), cpu_last  - cpu_first, &
        (timeused_new - timeused), counts_new(1), (counts_new(2:) - counts(2:))

END SUBROUTINE stop_timer

SUBROUTINE write_csv_overall( chunk_size, number_chunks )
    INTEGER, INTENT(IN) :: chunk_size, number_chunks

    LOGICAL             :: exist

    INQUIRE( FIlE = 'moa_overall.csv', EXiST = exist )
    IF ( .NOT. EXIST ) THEN
        OPEN( NEWUNIT = luoverall, FILE = 'moa_overall.csv' )
        CALL WRITE_CSV_HEADER( .true. )
    ELSE
        OPEN( NEWUNIT = luoverall, FILE = 'moa_overall.csv', POSITION = 'APPEND' )
    ENDIF

    WRITE( luoverall, '(100(g0.6,'',''))' ) chunk_size, number_chunks, &
        (time_last-time_zero) / real(time_rate), cpu_last, timeused_new, counts_new

END SUBROUTINE write_csv_overall

END MODULE monitoring


PROGRAM copy_and_extend
    USE moa_view_types
    USE monitoring

    IMPLICIT none

    INTEGER, ALLOCATABLE :: basis(:)
    INTEGER, POINTER     :: chunk(:)
    TYPE(moa_view_type)  :: array

    INTEGER              :: i, repeat, chunk_size, chk, appends, total, repeats
!   INTEGER              :: factor(0:5) = [ 1000, 300, 100, 30, 1, 1]
    INTEGER              :: factor(0:5) = [ 1000, 1000, 1000, 1000, 1000, 1000]


    OPEN( NEWUNIT = lurep, FILE = 'copy_and_extend_view.out' )
    OPEN( NEWUNIT = lucsv, FILE = 'copy_and_extend_view.csv' )
    CALL write_csv_header( .false. )

    DO chk = 0,4 !5
        chunk_size = 16**chk
        !appends    = 10**6 / chunk_size
        appends    = 100
        repeats    = 100 * factor(chk)
        ALLOCATE( basis(chunk_size) )
        basis = 0

        WRITE( *    , '(a,i0)' ) 'Chunk size:        ', chunk_size
        WRITE( lurep, '(a,i0)' ) 'Chunk size:        ', chunk_size
        WRITE( lurep, '(a,i0)' ) 'Number of appends: ', appends
        WRITE( lurep, '(a,i0)' ) 'Repeated:          ', repeats

        CALL start_timer

        DO repeat = 1,repeats
            ALLOCATE( chunk(chunk_size) )
            chunk = 1
            array = basis // chunk
            DO i = 2,appends
                NULLIFY( chunk )
                ALLOCATE( chunk(chunk_size) )
                chunk = 2
                array = array // chunk
            ENDDO
            total = array%elem(1) + array%elem(size(array))
            WRITE( 10, * ) total ! Dummy to fool the compiler

            CALL free( array )
        ENDDO

        CALL stop_timer

        DEALLOCATE( basis )
        NULLIFY( chunk )
    ENDDO

    WRITE(*,*) 'Done'

CONTAINS

! free --
!     Free the memory associated with the view "array"
!
SUBROUTINE free( array )
    TYPE(moa_view_type), INTENT(INOUT) :: array

    INTEGER                            :: i

    ! Leave the first element!!!!
    DO i = 2,SIZE(array%array)
        DEALLOCATE( array%array(i)%array )
    ENDDO

    DEALLOCATE( array%array )
END SUBROUTINE free

END PROGRAM copy_and_extend
