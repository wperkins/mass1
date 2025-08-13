#! /bin/sh
# -------------------------------------------------------------
# file: getbc.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created February 14, 2001 by William A. Perkins
# Last Change: 2019-01-10 12:18:09 d3g096
# -------------------------------------------------------------
dbdir=${DO9DBDIR-/home/d3g096/do9/database}
massbc="$dbdir/massbc.pl"
perlinc="-I$dbdir"

set -xe

#if [ -z "$PGHOST" ]; then
#    PGHOST=localhost
#    export PGHOST
#fi

if [ -z "$PGDATABASE" ]; then
    PGDATABASE=columbia
    export PGDATABASE
fi

start='01/01/1998'
end='01/01/2026'

                                  # gaged tributary inflows (hourly)

list=" \
    Anatone     13334300 \
    Orofino-Hourly     13340000 \
"

set $list

while [ $# -gt 0 ]; do
    trib=$1
    gage=$2
    perl $perlinc $massbc -g -h -Q -0 -o "$trib-Flow.dat" $gage $start $end
    shift 2
done

                                # gaged tributary inflows (daily)

sh get_trib_flow.sh $start $end

                                # guessed flows for ungaged inflows

# sh get_trib_const.sh 
