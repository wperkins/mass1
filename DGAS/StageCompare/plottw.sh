#! /bin/sh
# -------------------------------------------------------------
# file: plottw.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October  6, 1999 by William A. Perkins
# Last Change: 2017-03-08 08:03:07 d3g096
# -------------------------------------------------------------

damtw="\
    LWG     101 \
    LGS     121 \
    LMN     181 \
    IHR     201 \
    PRD     11  \
    MCN     251 \
    JDA     311 \
    TDA     351 \
    BON     451 \
"

damfb="\
    LWG     865 \
    LGS     1085 \
    LMN     1637 \
    IHR     1875 \
    MCN     2345 \
    JDA     298 \
    TDA     3328 \
    BON     4317 \
"

years="1994 1996 1997"
obsdir="../BCFiles/project"



echo 'set terminal postscript landscape color dashed "Helvetica" 14'

for year in $years; do

    for var in TWE; do

        case $var in
            TWE)
                ylabel="Tailwater Elevation, feet"
                set $damtw
                ;;
            FBE)
                ylabel="Forebay Elevation, feet"
                set $damfb
                ;;
        esac

        while [ $# -gt 0 ]; do
            code=$1
            num=$2
            
            observed="$obsdir/$code-$var.dat"
            simulated="ts$num.out.$year"

            case $code in
                DWR) title="Dworshak" ;;
                LWG) title="Lower Granite" ;;
                LGS) title="Little Goose" ;;
                LMN) title="Lower Monumental" ;;
                IHR) title="Ice Harbor" ;;
                PRD) title="Priest Rapids" ;;
                MCN) title="McNary" ;;
                JDA) title="John Day" ;;
                TDA) title="The Dalles" ;;
                BON) title="Bonneville" ;;
                PRD) title="Priest Rapids" ;;
                WAN) title="Wanapum" ;;
                RIS) title="Rock Island" ;;
                RRH) title="Rocky Reach" ;;
                WEL) title="Wells" ;;
                CHJ) title="Chief Joseph" ;;
                GCL) title="Grand Coulee" ;;
            esac

            cat <<EOF
set xdata time
set timefmt '%m-%d-%Y %H:%M:%S'
set format x "%m-%d\n%Y"
set xrange ['05-15-$year 00:00:00':'07-15-$year 00:00:00']
set format y '%.1f'
set ylabel '$ylabel'
set title '$title Dam'
set timestamp
plot '$observed' using 1:3 title "Observed" with lines ls 1, \
    '$simulated' using 1:3 title "Simulated" with lines ls 3
EOF

            shift 2
        done
    done
done
