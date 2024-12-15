dd. 14 december 2024, Arjen Markus

Experiments with the concepts of Mathematics of Arrays
======================================================

Overview of the files:
----------------------
Input:
    array1.inp           Input file for the program that measures the
                         performance (test_moa_measure): use plain
                         Fortran arrays

    view1.inp            Ditto, this time use MoA views

Note: in our performance experiments we have a few dozen such files to
examine the performance as function of total array size. Also with
different sizes of the chunks.


Auxiliary source files:
    cmdparse.f90         Simple command parser

    getresources_f.c     Wrapper for the getrusagfe() system routine to
                         get the resource usage

Batch files/shell scripts
    mkmeasure.bat        Basic version for building test_moa_measure program

    mkmeasure_new_pnt_v3.bat
                         Version that builds the extended test program
                         using the optimised versions

Note: the batch files are so simple that they function as shell scripts
as well.


Source files MoA:
    copy_and_extend_v2.f90
    copy_and_extend_view.f90
                         Copy and extend arrays - using plain arrays
                         and MoA views

    moa_cat.f90          First illustration of the cat operation,
                         multidimensional. Structure based on a binary tree
                         (following the theory)

    moa_dimension_lifting.f90
                         Illustration of "dimension lifting"

    moa_empty_array.f90  Illustration that an empty array has a definite
                         shape

    moa_measure.f90      First version of the measurement program

    moa_measure_v2.f90   Extended version (includes getrusage())

    moa_view_ndim_flat.f90
                         First revised version of the moa_view_type,
                         using a flat array instead of a binary tree
                         (this was much easier to get right)

    moa_view_ndim_flat_v15.f90
                         Optimised version, concentrates on
                         concatenating one-dimensional arrays
                         efficiently

    test_moa_view_ndim.f90
                         Test program to show that the concatenation
                         works in several dimensions (including scalar)
.
    view_general.f90     First version of the underlying type that
                         provdes access to the array elements via
                         a "general-dimension" pointer

    view_general_v6.f90  Improved version of this moa_basic_view
                         type

