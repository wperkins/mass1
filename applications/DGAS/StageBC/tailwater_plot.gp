# -------------------------------------------------------------
# file: plots.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September 10, 1999 by William A. Perkins
# Last Change: 2017-03-08 07:20:21 d3g096
# -------------------------------------------------------------

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set format x "%m-%d\n%Y"
set xrange ['02-15-@YEAR@ 00:00:00' : '10-15-@YEAR@ 00:00:00']

set ylabel "Discharge, cfs"
set format y "%.1f"
set auto y

set timestamp

set title '@NAME@ Dam'

plot '@FILE@' using 1:4 title 'Simulated' with lines ls 1, \
     '../BCFiles/project/@CODE@-Qtotal.dat' using 1:3 title 'Observed' with lines ls 3


