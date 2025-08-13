#! /bin/sh
# -------------------------------------------------------------
# file: latflow_plot.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September 13, 1999 by William A. Perkins
# Last Change: 2017-03-24 13:12:07 d3g096
# -------------------------------------------------------------

years="1994 1996 1997"
# years="1994 1995"
first="02-15"
last="10-15"

exec 3< code-files.txt

while read -u3 code start end link tsfile ; do
    case $code in
        BON) name='Bonneville' ;;
        TDA) name='The Dalles' ;;
        JDA) name='John Day' ;;
        MCN) name='McNary' ;;
        IHR) name='Ice Harbor' ;;
        LMN) name='Lower Monumental' ;;
        LGS) name='Little Goose' ;;
        LWG) name='Lower Granite' ;;
    esac
    for y in $years; do
        cat <<EOF | gnuplot
set size 0.7,1.4
set term postscript eps color dashed "Helvetica" 14
set output '$code-Lateral-$y.eps'
set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'
set lmargin 7
set xrange ['$first-$y 00:00:00' : '$last-$y 00:00:00']
set format x "%m-%d\n%Y"
set nokey

set multiplot
set origin 0.0,0.7
set size 0.7,0.7
set lmargin 10
set format y '%.0f'
set ylabel '$name Dam Discharge, cfs'

plot '../BCFiles/project/$code-Qtotal.dat' using 1:3 with lines ls 3

set origin 0.0, 0.0
set size 0.7, 0.7
set lmargin 10
set format y '%.0f'
set ylabel 'Flow "Correction for $name Pool", cfs'

plot '$code-Lateral-$y.dat' using 1:(\$3*($end-$start)*5280) with lines ls 3

set nomultiplot
EOF
    done
done
