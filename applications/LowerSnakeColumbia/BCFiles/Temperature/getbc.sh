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
# Last Change: 2019-01-10 10:50:14 d3g096
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
end="01/01/2020"

# Dworshak tailrace temperature
perl $perlinc $massbc -q -w -T -0 -o "DWQI-Temp.dat" DWQI $start $end

# Clearwater River @ Orofino
perl $perlinc $massbc -g -T -O "12 hour" -o Orofino-Daily-Temperature.dat 13340000 "$start" "$end"

# NF Clearwater River @ Canyon Ranger Station
perl $perlinc $massbc -g -T -O "12 hour" -o NFClearwater-Daily-Temperature.dat 13340600 "$start" "$end"


# Snake River @ Anatone temperature
# The actual BC file is ANQW-Temperature.dat which has many hand
# edits; use these to fill the gaps:
perl $perlinc $massbc -w -q -T -0 -o "ANQW-Monitor-Temperature.dat" ANQW  "$start" "$end"
perl $perlinc $massbc -g -h -T -0 -o "Anatone-Temperature.dat" 13334300 "$start" "$end"
perl $perlinc $massbc -g -T -O "12 hour" -o "Anatone-Daily-Temperature.dat" 13334300 "$start" "$end"

# Priest Rapids tailrace temperature, use part of the Hanford Reach BC
perl $perlinc $massbc -q -w -T -0 -o "PRXW-Temp.dat" PRXW $start $end

# The Priest Rapids tailrace monitor seems to have been out of action
# for a large part of 2017.  The gap is filled with that from the
# forebay monitor.
# perl $perlinc $massbc -q -w -T -0 -o "PRD-Forebay-Temp.dat" PRD 06/29/2017 11/15/2017

# Willamette
perl $perlinc $massbc -g -T -O "12 hour" -o Willamette-Daily-Temperature.dat 14211720 "$start" "$end"
