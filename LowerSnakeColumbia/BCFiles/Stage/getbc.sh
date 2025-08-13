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
# Last Change: 2025-06-10 14:21:39 d3g096
# -------------------------------------------------------------

set -x

dbdir=${DO9DBDIR-/home/d3g096/do9/database}
massbc="$dbdir/massbc.pl"
perlinc="-I$dbdir"

# if [ -z "$PGHOST" ]; then
#     PGHOST=localhost
#     export PGHOST
# fi

if [ -z "$PGDATABASE" ]; then
    PGDATABASE=columbia
    export PGDATABASE
fi

dams="BON TDA JDA MCN IHR LMN LGS LWG"
dams=""
start="01/01/1998"
end="01/01/2026"

                                # dam forebay stage

for d in $dams; do
    perl $perlinc $massbc -F -w -0 -o "$d-FBE.dat" $d $start $end
done

                                # tidal stage

perl $perlinc  $massbc -g -h -S -0 -o Tidal-Stage.dat 9440581 $start $end
