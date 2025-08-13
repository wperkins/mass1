# -------------------------------------------------------------
# file: border-plot.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 31, 2002 by William A. Perkins
# Last Change: 2017-05-18 09:47:26 d3g096
# -------------------------------------------------------------
set term postscript eps enhanced color solid "Helvetica" 16

set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'

set pointsize 0.5
set nokey

set title 'Columbia River Discharge at International Boundary'


set ylabel 'Discharge, kcfs'
set yrange [0:350]

set format x "%d%b\n%Y"
set grid

set output 'border-hourly-1.eps'
set xrange ['01-01-1971 00:00:00' : '01-01-1976 00:00:00']
plot 'Border-Discharge.dat' using 1:($3/1000) with lines ls 1

set output 'border-hourly-2.eps'
set xrange ['01-01-1976 00:00:00' : '01-01-1981 00:00:00']
plot 'Border-Discharge.dat' using 1:($3/1000) with lines ls 1

set output 'border-hourly-3.eps'
set xrange ['01-01-1981 00:00:00' : '01-01-1986 00:00:00']
plot 'Border-Discharge.dat' using 1:($3/1000) with lines ls 1

set output 'border-hourly-4.eps'
set xrange ['01-01-1986 00:00:00' : '01-01-1991 00:00:00']
plot 'Border-Discharge.dat' using 1:($3/1000) with lines ls 1

set output 'border-hourly-5.eps'
set xrange ['01-01-1991 00:00:00' : '01-01-1996 00:00:00']
plot 'Border-Discharge.dat' using 1:($3/1000) with lines ls 1

set output 'border-hourly-6.eps'
set xrange ['01-01-1996 00:00:00' : '01-01-2001 00:00:00']
plot 'Border-Discharge.dat' using 1:($3/1000) with lines ls 1

set output 'border-hourly-7.eps'
set xrange ['01-01-2001 00:00:00' : '01-01-2006 00:00:00']
plot 'Border-Discharge.dat' using 1:($3/1000) with lines ls 1

set output 'border-hourly-8.eps'
set xrange ['01-01-2006 00:00:00' : '01-01-2011 00:00:00']
plot 'Border-Discharge.dat' using 1:($3/1000) with lines ls 1

set output 'border-hourly-9.eps'
set xrange ['01-01-2011 00:00:00' : '01-01-2017 00:00:00']
plot 'Border-Discharge.dat' using 1:($3/1000) with lines ls 1

set title 'Columbia River Daily Discharge at International Boundary'

set output 'border-daily-1.eps'
set xrange ['01-01-1971 00:00:00' : '01-01-1976 00:00:00']
plot 'Border-Discharge-daily.dat' using 1:($3/1000) with lines ls 1

set output 'border-daily-2.eps'
set xrange ['01-01-1976 00:00:00' : '01-01-1981 00:00:00']
plot 'Border-Discharge-daily.dat' using 1:($3/1000) with lines ls 1

set output 'border-daily-3.eps'
set xrange ['01-01-1981 00:00:00' : '01-01-1986 00:00:00']
plot 'Border-Discharge-daily.dat' using 1:($3/1000) with lines ls 1

set output 'border-daily-4.eps'
set xrange ['01-01-1986 00:00:00' : '01-01-1991 00:00:00']
plot 'Border-Discharge-daily.dat' using 1:($3/1000) with lines ls 1

set output 'border-daily-5.eps'
set xrange ['01-01-1991 00:00:00' : '01-01-1996 00:00:00']
plot 'Border-Discharge-daily.dat' using 1:($3/1000) with lines ls 1

set output 'border-daily-6.eps'
set xrange ['01-01-1996 00:00:00' : '01-01-2001 00:00:00']
plot 'Border-Discharge-daily.dat' using 1:($3/1000) with lines ls 1

set output 'border-daily-7.eps'
set xrange ['01-01-2001 00:00:00' : '01-01-2006 00:00:00']
plot 'Border-Discharge-daily.dat' using 1:($3/1000) with lines ls 1

set output 'border-daily-8.eps'
set xrange ['01-01-2006 00:00:00' : '01-01-2011 00:00:00']
plot 'Border-Discharge-daily.dat' using 1:($3/1000) with lines ls 1

set output 'border-daily-9.eps'
set xrange ['01-01-2011 00:00:00' : '01-01-2017 00:00:00']
plot 'Border-Discharge-daily.dat' using 1:($3/1000) with lines ls 1

