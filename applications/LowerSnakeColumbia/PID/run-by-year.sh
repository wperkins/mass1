#!/bin/sh

model="${MASS1-mass1}"

set -xue

y=2007
yend=2025

while [ "$y" -le "$yend" ]; do
    y1=`expr "$y" - 1`

    if [ -f "hotstart-${y1}.dat" ]; then
        restart="hotstart-${y1}.dat"
    else
        restart="../StageBC/hotstart-${y}.dat"
    fi
    rm -f mass1.cfg
    sed -e "s,@RESTART@,$restart,g" \
        -e "s/@YEAR@/$y/g" \
        mass1-pid.cfg > mass1.cfg
    if [ $y -eq 2025 ]; then
        sed -i.orig -e "s/12-31/06-01/" mass1.cfg
    fi
    $model

    (cd compare; sh compare.sh)

    rm -rf "${y}.old"
    if [ -d "${y}" ]; then
        mv "${y}" "${y}.old"
    fi
    mkdir "${y}"
    mv *.out *.eps statistics.dat "${y}"
    
    y=`expr "$y" + 1`
done

