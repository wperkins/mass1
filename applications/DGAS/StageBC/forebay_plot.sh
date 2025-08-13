#! /bin/sh
# -------------------------------------------------------------
# file: forebay_plot.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created September 10, 1999 by William A. Perkins
# Last Change: 2017-03-01 11:30:02 d3g096
# -------------------------------------------------------------

years="1994 1995 1996 1997"

if [ $# -gt 0 ]; then
    years="$*"
fi

exec 3< code-files.txt

while read -u3 code start end link tsfile; do
    case $code in
        BON) name='Bonneville' ;;
        TDA) name='The Dalles' ;;
        JDA) name='John Day' ;;
        MCN) name='McNary' ;;
        IHR) name='Ice Harbor' ;;
        LMN) name='Lower Monumental' ;;
        LGS) name='Little Goose' ;;
        LWG) name='Lower Granite' ;;
        CHJ) name='Chief Joseph' ;;
        WEL) name='Wells' ;;
        RIS) name='Rock Island' ;;
        RRH) name='Rocky Reach' ;;
        WAN) name='Wanapum' ;;
        PRD) name='Priest Rapids' ;;
    esac
    for year in $years; do
        cat <<EOF | gnuplot
set term postscript eps color dashed "Helvetica" 14
set output '$code-forebay-$year.eps'
set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'
set format x "%m-%d\n%Y"
set xrange ['02-15-$year 00:00:00' : '10-15-$year 00:00:00']
set ylabel "Discharge, cfs"
set format y "%.1f"
set auto y
set timestamp
set title '$name Dam'
plot '../BCFiles/project/$code-Qtotal.dat' using 1:3 title 'Observed' with lines ls 3, \
     '$tsfile' using 1:4 title 'Simulated' with lines ls 1
     
EOF

    done
done
