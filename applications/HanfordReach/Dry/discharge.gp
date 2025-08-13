set term post eps enh color solid "Helvetica" 18

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S'

set format x "%d%b\n%Y"
set xrange ["05-01-2000 00:00:00":"10-01-2000 00:00:00"]

set ylabel "Discharge, kcfs"

set arrow 1 from first "06-01-2000 00:00:00", graph 0.0 to first "06-01-2000 00:00:00", graph 1.0 nohead lt 0
set arrow 2 from first "09-15-2000 00:00:00", graph 0.0 to first "09-15-2000 00:00:00", graph 1.0 nohead lt 0
set arrow 3 from first "06-01-2000 00:00:00", graph 0.5 to first "09-15-2000 00:00:00", graph 0.5 heads lt 7
set label 1 "Simulation Period" at graph 0.5, graph 0.55 center

set key top center

plot "BCFiles/PRD-Qtotal.dat" using 1:($3/1000) title "Priest Rapids" with lines ls 1, \
     "ts1194.out" using 1:($4/1000) title "Simulated @ 100-F Area" with lines ls 3
