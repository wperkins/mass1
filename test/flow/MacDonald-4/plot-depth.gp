# -------------------------------------------------------------
# file: plot-elev.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January  2, 2004 by William A. Perkins
# Last Change: 2017-01-16 08:37:44 d3g096
# -------------------------------------------------------------
# $Id$

set terminal postscript eps color solid "Helvetica,18"
set auto
set ytics nomirror
set y2tics nomirror
set xlabel "Longitudinal Distance, m"
set ylabel "Depth, m"
set y2label "Froude Number"
set format y "%.1f"
set format y2 "%.1f"
set pointsize 0.5
set key below
set xrange [0:1000]
set yrange [0:*]
set y2range [0:*]
set key font ",16"

plot "<head -46 profile1.out" using ($4*0.3048):($8*0.3048) axes x1y1 title "Initial Conditions" with linespoints lt 1, \
     "<tail -46 profile1.out" using ($4*0.3048):($8*0.3048) axes x1y1 title "Steady State" with linespoints lt 7, \
     "solution.dat" using ($1*0.3048):($2*0.3048) axes x1y1 title "Analytic Solution" with lines lt 3, \
     "<tail -46 profile1.out" using ($4*0.3048):($17) axes x1y2 title "Froude Number" with linespoints lt 4

     
