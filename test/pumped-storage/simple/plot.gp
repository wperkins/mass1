
set term post enh eps color dashed "Helvetica" 18
set output 'plot.eps'

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

set size 1.0, 1.5
set multiplot layout 2, 1 scale 1.0, 1.5

set tmargin 0
set bmargin 1
set lmargin 8
set rmargin 2
set key right noreverse
set ylabel 'Discharge, m^{3}/s'
set format y '%4.1f'

set size 1.0, 0.5
set origin 0.0, 1.0
set xzeroaxis
set yrange [-2:2]
set xtics mirror format ""
set key title 'Pumped Storage'

plot 'tsPump.out' using 1:4 notitle with lines ls 1

set origin 0.0, 0.0
set size 1.0, 1.0
set yrange [1:5]
set xtics mirror
set bmargin 3
set xtics mirror format "%m%d\n%H:%M"
set key title "Channel"
plot 'tsInlet.out' using 1:4 title "Upstream" w lines ls 1, \
     'tsOutlet.out' using 1:4 title "Downstream" w lines ls 3

unset multiplot

