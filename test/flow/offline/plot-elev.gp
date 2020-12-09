# -------------------------------------------------------------
# file: plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 22, 1999 by William A. Perkins
# Last Change: 2020-12-09 07:47:21 d3g096
# -------------------------------------------------------------

set terminal postscript eps color solid "Helvetica,18"

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"
set format x "%m-%d\n%H:%M"
set xrange [ "01-01-1997 12:00:00" : * ]

fld = 3
set ylabel "WSE, ft"

plot 'ts11.out' using 1:fld title "Inflow" with lines ls 1, \
     'ts175.out' using 1:fld title "Upstream" with lines ls 3, \
     'ts21.out' using 1:fld title "Downstream" with lines ls 4, \
     'ts275.out' using 1:fld title "Outlet" with lines ls 5
