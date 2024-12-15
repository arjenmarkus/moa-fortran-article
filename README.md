# moa-fortran-article
Implementation of Mathematics-of-Arrays operations in Fortran, part of the article on the subject

The article ["Implementing Mathematics of Arrays in Modern Fortran: Efficiency and Efficacy"](https://www.mdpi.com/2674-113X/3/4/26)
published in [Software](https://www.mdpi.com/journal/software) describes how operations on arrays as
defined in Mathematics of Arrays can be implemented in Fortran. The article includes the
results of several experiments to understand the effectiveness and efficiency. This
repository contains the source code and the result files for these experiments.

The directory `src` contains the sources of the programs that were developed, As a number of iterations
were involved, I have put in the most relevant versions. It also includes several demonstrations of
MoA operations that can be implemented directly in Fortran.

The directory `results` contains the output files as were obtained with the experiments described
in the article:

 * `experiment-catenation`: extending arrays and copying the contents
 * `experiment-access`: performance of accessing individual elements of the arrays (both plain Fortran arrays and MoA views)
   As there are many files, I have provided this experiment via zip and tgz files.

