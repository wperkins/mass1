# -------------------------------------------------------------
# file: stage_discharge.gp
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July 23, 2012 by William A. Perkins
# Last Change: 2018-02-13 14:08:53 d3g096
# -------------------------------------------------------------

set terminal postscript eps enh color solid "Helvetica" 16

set grid

set xtics nomirror
set x2tics nomirror
set xlabel "Discharge, m^{3}/s"
set x2label "Discharge, kcfs"
set format x "%.0f"
set format x2 "%.0f"
set xrange [0:14158.423]
set x2range [0:500]
set xrange [0:4500]
set x2range [0:158.916]
set xrange [0:16000]
set x2range [0:565.03467]

set ytics nomirror
set y2tics nomirror
set ylabel "Stage above 120 kcfs, m"
set y2label "Stage above 120 kcfs, ft"
set format y "%.1f"
set format y2 "%.0f"

set dataf sep ','
set key notitle
set key left

set auto y
set auto y2

set output "/dev/null"
plot "stage_discharge_632.5.dat" using ($4/1000.0):($3-404.12) axes x2y2 title 'Rkm 632.5' with lines lt 1, \
     "stage_discharge_593.0.dat" using ($4/1000.0):($3-370.25) axes x2y2 title 'Rkm 593.0' with lines lt 3, \
     "stage_discharge_590.0.dat" using ($4/1000.0):($3-366.32) axes x2y2 title 'Rkm 590.0' with lines lt 4, \
     "stage_discharge_583.5.dat" using ($4/1000.0):($3-359.44) axes x2y2 title 'Rkm 583.5' with lines lt 2

set output
set y2range [GPVAL_Y2_MIN : GPVAL_Y2_MAX]
set yrange [GPVAL_Y2_MIN*0.3048 : GPVAL_Y2_MAX*0.3048]
replot



