#!/bin/sh

model=/files0/mass1/bin/mass1

set -x

y=2005
yend=2018

while [ "$y" -le "$yend" ]; do
    y1=`expr "$y" - 1`

    if [ -f "hotstart-${y1}.dat" ]; then
        restart="hotstart-${y1}"
    else
        restart="../Rampdown.dams/restart"
    fi
    rm -f mass1.cfg
    sed -e "s,@RESTART@,$restart,g" \
        -e "s/@YEAR@/$y/g" \
        mass1-base.cfg > mass1.cfg
    $model

    sed -e "s/-2015/-$y/g" Disch.gp Stage-FBE.gp Temp.gp | gnuplot

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

