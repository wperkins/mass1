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
# Last Change: 2025-06-09 13:12:26 d3g096
# -------------------------------------------------------------

dbdir=${DO9DBDIR-/home/d3g096/do9/database}
massbc="$dbdir/massbc.pl"
perlinc="-I$dbdir"

set -xe

# if [ -z "$PGHOST" ]; then
#     PGHOST=localhost
#     export PGHOST
# fi

if [ -z "$PGDATABASE" ]; then
    PGDATABASE=columbia
    export PGDATABASE
fi


dams="DWR LWG LGS LMN IHR PRD MCN JDA TDA BON"
start="01/01/1998"
end="01/01/2026"

                                # total flows at dams

for d in $dams; do
    srcflag="-w"
    perl $perlinc $massbc $srcflag -Q -O '-30 minutes' -o "$d-Qtotal.dat" $d $start $end
done

#sh plots.sh | gnuplot
