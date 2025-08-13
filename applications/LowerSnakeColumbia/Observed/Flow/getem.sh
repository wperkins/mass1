#! /bin/sh
# -------------------------------------------------------------
# file: get_trib_flow.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created March 19, 2001 by William A. Perkins
# Last Change: 2019-01-10 14:52:52 d3g096
# -------------------------------------------------------------
set -x

program=`basename $0`

dbdir=${DO9DBDIR-/home/d3g096/do9/database}
massbc="$dbdir/massbc.pl"
perlinc="-I$dbdir"

# if [ -z "$PGHOST" ]; then
#     PGHOST=leechong
#     export PGHOST
# fi

if [ -z "$PGDATABASE" ]; then
    PGDATABASE=columbia
    export PGDATABASE
fi

start="01/01/1998"
end="01/01/2020"

# if [ -z "$1" -o -z "$2" ]; then
#     echo usage: $program start end
#     exit 3;
# fi

# -------------------------------------------------------------
# Tributary and Boundary Flow where we have 1977 data
# -------------------------------------------------------------

gages=" \
    14246900    Beaver \
"
set $gages
while [ $# -gt 0 ]; do
    gage=$1
    name=$2
    out="$name-Flow.dat"
    perl $perlinc $massbc -g -h -Q -0 -o "$out" $gage $start $end
    shift 2;
done

