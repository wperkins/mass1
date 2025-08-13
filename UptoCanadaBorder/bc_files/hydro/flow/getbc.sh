#!/bin/sh
# -------------------------------------------------------------
# file: getbc.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created May 24, 2018 by William A. Perkins
# Last Change: 2019-02-08 10:05:18 d3g096
# -------------------------------------------------------------

dbdir=${DO9DBDIR-/home/d3g096/do9/database}
massbc="$dbdir/massbc.pl"
perlinc="-I$dbdir"

set -xue

start="01/01/1998"
end="01/01/2020"

dams="GCL CHJ WEL RRH RIS PRD WAN IHR"
dams="GCL CHJ WEL RRH RIS WAN IHR"

for d in $dams; do
    srcflag="-w"
    perl $perlinc $massbc $srcflag -Q -O '-30 minutes' -o "${d}_flow.dat" $d $start $end
done

# Gaged tributaries w/ daily data (start one day early)

start="12/31/1997"

gages=" \
    Chel            12452500 \
    Col_IB_daily    12399500 \
    Colville        12409000 \
    Crab            12472600 \
    Entiat          12452800 \
    GCL_feeder      12435500 \
    Kettl           12404500 \
    Meth            12449950 \
    Okan            12447200 \
    Spok            12433000 \
    Wen             12462500 \
    Yak             12510500 \
"

set $gages
while [ $# -gt 0 ]; do
    gage=$2
    name=$1
    out="${name}_flow.dat"

    case $name in
        GCL_feeder)
            scale="-1.0"
            ;;
        *)
            scale="1.0"
    esac
    perl $perlinc $massbc -g -Q -x "$scale" -o "$out" $gage $start $end
    shift 2;
done

# Gaged tributaries w/ hourly data

gages=" \
    Col_IB          12399500 \
"

set $gages
while [ $# -gt 0 ]; do
    gage=$2
    name=$1
    out="${name}_flow.dat"

    perl $perlinc $massbc -g -h -Q -o "$out" $gage $start $end
    shift 2;
done
