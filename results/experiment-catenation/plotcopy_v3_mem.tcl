# plotcopy.tcl --
#     Plot the timers for copy actions
#
package require Plotchart
lappend auto_path d:/tcl-programs/pdf4tcl-head
package require pdf4tcl

pack [canvas .c -width 700 -height 600]

set p [::Plotchart::createLogXLogYPlot .c {0.1 1.0e5 10.0} {1.0e3 1.5e6 10.0}]

$p dataconfig array -type symbol -colour blue    -symbol plus
$p dataconfig view  -type symbol -colour red     -symbol cross
$p dataconfig intel -type symbol -colour magenta -symbol up

$p legendconfig -spacing 15
$p legend array "Plain arrays (gfortran)"
$p legend intel "Plain arrays (intel)"
$p legend view  "View type"

console show

foreach file {copy_and_extend_v2.csv copy_and_extend_view.csv copy_and_extend_intel.csv} \
        type {array                  view                     intel} {
    set infile [open $file]
    gets $infile line

    set chunk 1

    while { [gets $infile line] >= 0 } {

        set maxmemory [lindex [split $line ,] 4]
        $p plot $type $chunk $maxmemory

        puts "$chunk -- $maxmemory"

        set chunk [expr {$chunk * 16}]
    }
}

after 1000 {
    set pdf1 [::pdf4tcl::new %AUTO% -paper {16c 12c}]
    $pdf1 canvas .c
    $pdf1 write -file copy_extend_combined_v3_mem_three.pdf
}
