#! /bin/sh
# -------------------------------------------------------------
# file: compare.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created November  9, 2001 by William A. Perkins
# Last Change: 2018-03-01 13:26:02 d3g096
# -------------------------------------------------------------

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
R="R --no-save --slave"
# R="R --no-save"

mass1bc="mass1bc"
# mass1bc="true"
mass1bcopts=" -f temp -0"

set -xue

# -------------------------------------------------------------
# dodaily
# -------------------------------------------------------------
dodaily() {
    all=$1
    out=$2
    sed -e "s/@IN@/$all/g" \
        -e "s/@OUT@/$out/g" dostat.R > tmp.R
    $R < tmp.R
    return 0
}


# -------------------------------------------------------------
# domedian
# -------------------------------------------------------------
domedian() {
    daily="$1"
    out="$2"
    sed -e "s/@IN@/$daily/g" \
        -e "s/@OUT@/$out/g" julian-median.R > tmp.R
    $R < tmp.R
    return 0
    
}

# -------------------------------------------------------------
# docfdplots
# -------------------------------------------------------------
docfdplots() {
    code="$1"
    lvl="$2"
    title="$3"
    sed -e "s/@LEVEL@/$lvl/g" \
        -e "s/@CODE@/$code/g" \
        -e "s/@TITLE@/$title/g" \
        cfd-plot.R > tmp.R
    $R < tmp.R
}

# -------------------------------------------------------------
# doexcursions
# -------------------------------------------------------------
doexcursions() {
    code="$1"
    lvl="$2"
    scn="$3"
    sed -e "s/@CODE@/$code/g" \
        -e "s/@SCENARIO@/$scn/g" \
        -e "s/@LEVEL@/$lvl/g" excursions.R > tmp.R
    $R < tmp.R
}




# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

exec < site-list.txt

while read code loc criteria title; do
    files=`ls ../2[01]??/ts${loc}.out | grep -v 2003 `
    $mass1bc $mass1bcopts $files > $code-temp.all
    dodaily $code-temp.all $code-temp.daily
    domedian $code-temp.daily $code-temp.median
    sed -e "s/@LOC@/$code/g" -e "s/@TITLE@/$title/g" strawbroom.gp | \
        gnuplot > $code-temp.strawbroom.eps
done
