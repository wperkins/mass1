#! /bin/sh
# -------------------------------------------------------------
# file: run-by-year.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 13, 1999 by William A. Perkins
# Last Change: 2017-10-25 15:10:25 d3g096
# -------------------------------------------------------------

set -x
set -e

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------

model=${MASS1-/net/flophouse/files0/perksoft/linux64/mass1/bin/mass1}

startyear=1960
startdate='01-15'
# startyear=1994
# startdate='01-01'

endyear=1964
enddate='06-30'

dumptime='00:00:00'
timestep=0.5000000
transsteps=10
outstep=1			# hours

# -------------------------------------------------------------
# computed variables
# -------------------------------------------------------------
outtimestep=`echo 'scale=0;' $outstep/$timestep | bc `

# -------------------------------------------------------------
# do a warmup
# -------------------------------------------------------------
cp mass1-warmup.cfg mass1.cfg
$model > /dev/null
for i in ts[0-9]*[0-9].out status.out; do
    n=`expr "$i" : '\(.*\)\.out'`-warmup.out
    mv $i $n
done
rm output.out

# -------------------------------------------------------------
# loop through the years
# -------------------------------------------------------------
year=$startyear
while [ $year -le $endyear ]; do
    lastyear=`echo $year - 1 | bc`
    nextyear=`echo $year + 1 | bc`
    tag="${year}yr"
    lasttag="${lastyear}yr"
    if [ $year -eq 1960 ]; then
        date=$startdate
        hotstart='hotstart-warmup.dat'
    else
        date='01-01'
        hotstart="hotstart-$lasttag.dat"
    fi
    if [ $nextyear -gt $endyear ]; then
        nextdate=$enddate
    else
        nextdate='01-01'
    fi
    restart="hotstart-$tag.dat"
    sed \
        -e "s/@STARTDATE@/$date/" \
        -e "s/@STARTYEAR@/$year/" \
        -e "s/@ENDDATE@/$nextdate/" \
        -e "s/@ENDYEAR@/$nextyear/" \
        -e "s/@TIME@/$dumptime/" \
        -e "s/@HOTSTART@/$hotstart/" \
        -e "s/@RESTART@/$restart/" \
        -e "s/@TIMESTEP@/$timestep/" \
        -e "s/@TRANSSTEP@/$transsteps/" \
        -e "s/@OUTSTEP@/$outtimestep/" \
	mass1-pattern.cfg > mass1-${tag}.cfg

    cp mass1-${tag}.cfg mass1.cfg
    time ($model > /dev/null)
    # mv profile1.out profile1-${tag}.out
    # tail -n 342 profile1-${tag}.out > profile1-${tag}.last
    # mv profile2.out profile2-${tag}.out
    # tail -n 342 profile2-${tag}.out > profile2-${tag}.last
    for i in ts[0-9]*[0-9].out status.out; do
        n=`expr "$i" : '\(.*\)\.out'`-${tag}.out
        mv $i $n
    done
    rm -f output.out
    
    year=$nextyear
done

# ./dostats.sh
