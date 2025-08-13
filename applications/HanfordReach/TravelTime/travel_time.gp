
set term post enh eps color dashed "Helvetica" 18

set xlabel "Discharge, kcfs"
set xtics nomirror
xmin = 0
xmax = 350
set xrange [xmin:xmax]

x2min=28.316847*xmin
x2max=28.316847*xmax
set x2label "Discharge, m^{3}/s"
set x2tics nomirror
set x2range [x2min:x2max]

set ylabel "Travel Time from PRD, hr"
# set yrange [0:*]

plot "travel_time.dat" using 1:2 title "300 Area (RM 344.3)" with linespoints ls 1, \
     '' using 1:3 title "Clover Island, PAQW (RM 328.5)" with linespoints ls 3

