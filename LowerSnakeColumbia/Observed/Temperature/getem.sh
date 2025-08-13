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
# Last Change: 2018-02-23 12:58:05 d3g096
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

start="01/01/1998"
end="01/01/2019"

# Dam tailrace TDG monitors
monitors="\
    PEKI \
    LEWI \
    LGNW \
    LGSW \
    LMNW \
    IDSW \
    PAQW \
    MCPW \
    JHAW \
    TDDO \
    CCIW \
    WRNO \
    CWMW \
"

for m in $monitors; do
    perl $perlinc $massbc -q -w -T -0 -o "$m-Temp.dat" "$m" $start $end
done

# Get temperature from USGS gages
perl $perlinc $massbc -g -h -T -0 -o Peck-Temp.dat 13341050 "$start" "$end"
perl $perlinc $massbc -g -h -T -0 -o Spalding-Temp.dat 13342500 "$start" "$end"

    
