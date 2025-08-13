#!/bin/sh

model="${MASS1-mass1}"

set -xue

y=2003
yend=2017

while [ "$y" -le "$yend" ]; do
    y1=`expr "$y" - 1`

    if [ -f "hotstart-${y1}.dat" ]; then
        restart="hotstart-${y1}.dat"
    else
        restart="../PID/hotstart-${y1}.dat"
    fi
    rm -f mass1.cfg
    sed -e "s,@RESTART@,$restart,g" \
        -e "s/@YEAR@/$y/g" \
        mass1-temp.cfg > mass1.cfg
    $model

    sed -e "s/-2015/-$y/g" Temp.gp | gnuplot

    (cd compare; sh compare.sh $y)

    rm -rf "${y}.old"
    if [ -d "${y}" ]; then
        mv "${y}" "${y}.old"
    fi
    mkdir "${y}"
    mv *.out *.eps statistics.dat "${y}"

    (cd "${y}"; mogrify -format png -density 300 *.eps )
    
    y=`expr "$y" + 1`
done

(cd stats; sh stats.sh)
