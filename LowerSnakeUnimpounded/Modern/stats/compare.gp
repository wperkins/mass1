if (GPVAL_VERSION >= 5.0) { set colors classic }
set term postscript eps color dashed "Helvetica" 16
set format y "%.1f"
set nogrid
set key left
set key title "@TITLE@"
set xtics mirror norotate  ("Jan 1" 1, "Feb 1" 32, "Mar 1" 60, "Apr 1" 91, "May 1" 121, "Jun 1" 152, "Jul 1" 182, "Aug 1" 212, "Sep 1" 244, "Oct 1" 274, "Nov 1" 305, "Dec 1" 335, "Dec 31" 365)
set xlabel "Day of Year" 
set xrange [ 1.0000 : 365.000 ] noreverse nowriteback
set ylabel "Median Daily Maximum Temperature,  C" 
set yrange [0:27.5]

plot '@IMPOUNDED@/stats/@LOC@-temp.median' using 1:($5) title 'Current Conditions' with lines lt 1 lc 1, \
     '@LOC@-temp.median' using 1:($5) title 'Unimpounded' with lines ls 7 lc 3
#    EOF
