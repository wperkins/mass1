#!/bin/sh

model=/files0/mass1/bin/mass1

set -xue

y=2003
yend=2017

while [ "$y" -le "$yend" ]; do
    y1=`expr "$y" - 1`

    if [ -f "hotstart-${y1}.dat" ]; then
        restart="hotstart-${y1}.dat"
    else
        restart="hotstart.dat"
    fi
    rm -f mass1.cfg
    sed -e "s,@RESTART@,$restart,g" \
        -e "s/@YEAR@/$y/g" \
        mass1-temp.cfg > mass1.cfg
    $model

    rm -rf "${y}.old"
    if [ -d "${y}" ]; then
        mv "${y}" "${y}.old"
    fi
    mkdir "${y}"
    mv *.out "${y}"

    y=`expr "$y" + 1`
done

