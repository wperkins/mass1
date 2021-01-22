# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2021-01-22 09:46:47 d3g096
# -------------------------------------------------------------

set terminal postscript eps color solid "Helvetica,18"

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"
set format x "%m-%d\n%H:%M"
set xrange [ "01-01-1997 12:00:00" : * ]

qfld = 4
tfld = 9 
set ylabel "Discharge, cfs"
set y2label "Concentration"
set ytics nomirror
set y2tics nomirror
set yrange [0:*]

plot 'ts175.out' using 1:qfld axes x1y1 title "Upstream Discharge" with lines lt 1 lc 1, \
     'ts175.out' using 1:tfld axes x1y2 title "Upstream Concentration" with lines lt 3 lc 1, \
     'ts21.out' using 1:qfld axes x1y1 title "Downstream Discharge" with lines lt 1 lc 3, \
     'ts21.out' using 1:tfld axes x1y2 title "Downstream Concentration" with lines ls 3 lc 3


