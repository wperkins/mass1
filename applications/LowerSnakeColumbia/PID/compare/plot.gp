set terminal unknown

set xdata time
set timefmt "%m-%d-%Y %H:%M:%S"

set title "@NAME@"

set ylabel "@DATA@"
set format y "%.1f"
set format x "%b%d\n%Y"

plot "@SIMFILE@" using 1:3 

set terminal postscript eps color solid "Helvetica" 16
set output "@OUTPS@"

set xrange [GPVAL_DATA_X_MIN:GPVAL_DATA_X_MAX]

plot '@SIMFILE@' using 1:@FLD@ title "Simulated" with lines lt 1, \
     '@OBSFILE@' using 1:3 title "Observed" with lines lt 3

