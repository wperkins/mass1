#! /bin/sh
# -------------------------------------------------------------
# file: getbc.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March  1, 2001 by William A. Perkins
# Last Change: 2017-10-10 11:42:52 d3g096
# -------------------------------------------------------------

dbdir=${DO9DBDIR-/home/d3g096/do9/database}
massbc="$dbdir/massbc.pl"
perlinc="-I$dbdir"

set -xe

if [ -z "$PGHOST" ]; then
    PGHOST=localhost
    export PGHOST
fi

if [ -z "$PGDATABASE" ]; then
    PGDATABASE=columbia
    export PGDATABASE
fi

dams="BON TDA JDA MCN PRD IHR LMN LGS LWG"
start="01/01/1998"
end="01/01/2018"

                                # dam tailwater stage

for d in $dams; do
    perl $perlinc $massbc -T -0 -o "$d-TWE.dat" $d $start $end
done

# get hourly only stage at Clover Island

perl $perlinc $massbc -g -h -0 -S -o "USGS-12514500.dat" 12514500 $start $end
gsed -i.orig -e '/\(15\|30\|45\):00/d' USGS-12514500.dat


# Get stage for the NOAA gages in the estuary

gages=" \
    9439040    Astoria \
    9440083    Vancouver \
    9440422    Longview \
    9440569    Skamokawa \
"
set $gages
while [ $# -gt 0 ]; do
    gage=$1
    name=$2
    out="$name-Stage.dat"
    perl $perlinc $massbc -g -h -S -0 -o "$out" $gage $start $end
    shift 2;
done
