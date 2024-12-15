# plotcopy.tcl --
#     Plot the timers for copy actions
#
package require Plotchart
lappend auto_path d:/tcl-programs/pdf4tcl-head
package require pdf4tcl

pack [canvas .c -width 700 -height 600]

set p [::Plotchart::createLogXLogYPlot .c {0.1 1.0e7 10.0} {1.0 1.0e7 10.0}]

$p dataconfig array -type symbol -colour blue -symbol plus
$p dataconfig view  -type symbol -colour red  -symbol cross

$p legendconfig -spacing 15
$p legend array "Plain arrays"
$p legend view  "View type"


set outfile [open "regression.csv" w]

foreach file {copy_and_extend_v2.out copy_and_extend_view.out} \
        type {array                  view                    } {
    set infile [open $file]
    while { [gets $infile line] >= 0 } {
        if { [string first "Chunk size" $line] >= 0 } {
            set chunk [lindex $line end]
        }
        if { [string first "Repeated" $line] >= 0 } {
            set factor [expr {100000.0 / [lindex $line end]}]
        }
        if { [string first "Wall clock" $line] >= 0 } {
            set time [expr {$factor * [lindex $line end]}]

            $p plot $type $chunk $time

            puts $outfile "[expr {log($chunk)}],[expr {log($time)}]"
        }
    }
}

after 1000 {
    set pdf1 [::pdf4tcl::new %AUTO% -paper {16c 12c}]
    $pdf1 canvas .c
    $pdf1 write -file copy_extend_combined_v3.pdf
}
