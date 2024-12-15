/* getresources_f.c --
       Provide simple interface to getrusage() for Fortran programs
*/

#include <stdlib.h>
#include <stdio.h>
// #include <sys/resource.h>

// getresources:
//     Return the resource useage
//
// Arguments:
//     timeused           Array of two doubles: time spent on user instructions and time spent in OS code
//     counts             Array of counts (14 numbers)

void getresources( double *timeused, long *counts ) {
/*
    struct rusage rsc;

    getrusage( RUSAGE_SELF, &rsc );

    timeused[0] = (double) rsc.ru_utime.tv_sec + ((double) rsc.ru_utime.tv_usec) / 1.0e06; // Time spent executing user instructions.
    timeused[1] = (double) rsc.ru_stime.tv_sec + ((double) rsc.ru_stime.tv_usec) / 1.0e06; // Time spent in operating system code on behalf of processes.

    counts[0]   = rsc.ru_maxrss;       // The maximum resident set size used, in kilobytes. That is, the maximum number of kilobytes of physical memory that processes used simultaneously.
    counts[1]   = rsc.ru_ixrss;        // An integral value expressed in kilobytes times ticks of execution, which indicates the amount of memory used by text that was shared with other processes.
    counts[2]   = rsc.ru_idrss;        // An integral value expressed the same way, which is the amount of unshared memory used for data.
    counts[3]   = rsc.ru_isrss;        // An integral value expressed the same way, which is the amount of unshared memory used for stack space.
    counts[4]   = rsc.ru_minflt;       // The number of page faults which were serviced without requiring any I/O.
    counts[5]   = rsc.ru_majflt;       // The number of page faults which were serviced by doing I/O.
    counts[6]   = rsc.ru_nswap;        // The number of times processes was swapped entirely out of main memory.
    counts[7]   = rsc.ru_inblock;      // The number of times the file system had to read from the disk on behalf of processes.
    counts[8]   = rsc.ru_oublock;      // The number of times the file system had to write to the disk on behalf of processes.
    counts[9]   = rsc.ru_msgsnd;       // Number of IPC messages sent.
    counts[10]  = rsc.ru_msgrcv;       // Number of IPC messages received.
    counts[11]  = rsc.ru_nsignals;     // Number of signals received.
    counts[12]  = rsc.ru_nvcsw;        // The number of times processes voluntarily invoked a context switch (usually to wait for some service).
    counts[13]  = rsc.ru_nivcsw;       // The number of times an involuntary context switch took place (because a time slice expired, or another process of higher priority was scheduled).
*/

}
