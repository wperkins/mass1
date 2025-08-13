
set term post eps enh color solid "Helvetica" 18

set xlabel "Columbia River Mile"
set ylabel "Elevation, ft"

set xrange [340:400]
set key left width -6
set key title "Hanford Reach Goes Dry!"


plot "<head -n 486 profile1.out" using 4:13 title "Thalweg" with lines lt 7, \
     "profile1.out" using 4:5 title "Water Surface (every 14 days)" with lines lt 1