#!/bin/sh
# -------------------------------------------------------------
# file: runme.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  7, 2020 by  William Perkins 
# Last Change: 2020-07-07 12:32:19 d3g096
# -------------------------------------------------------------

set -xue

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------
mass1=/net/flophouse/files0/perksoft/linux64/mass1/bin/mass1_new

runit=no
runit=yes

qstep=20
qmin=40
qmax=300

q=$qmin

if [ "$runit" == "yes" ]; then
    while [ "$q" -le "$qmax" ]; do
        outd=`echo $q | awk '{printf("q%03d", $0)}'`
        if [ -d "$outd" ]; then
            if [ -d "${outd}.old" ]; then
                rm -rf "${outd}.old"
            fi
            mv "$outd" "${outd}.old"
        fi
        mkdir "$outd"

        sed -e "s/120/$q/g" discharge.dat.in > "$outd/discharge.dat"
        cp mass1.cfg.in "$outd/mass1.cfg"

        (cd $outd; $mass1 >& mass1.log )
        
        q=`expr "$q" + "$qstep"`
    done
fi


ttout="travel_time.dat"
if [ -f $ttout ]; then
    mv $ttout "${ttout}.old"
fi
cp /dev/null travel_time.dat

q=$qmin
while [ "$q" -le "$qmax" ]; do
    outd=`echo $q | awk '{printf("q%03d", $0)}'`
    tt300=`python extract-travel-time.py < $outd/ts1350.out | awk '{print $NF}' `
    ttpaqw=`python extract-travel-time.py < $outd/ts314.out | awk '{print $NF}' `
    echo $q $tt300 $ttpaqw >> $ttout
    
    q=`expr "$q" + "$qstep"`
done
