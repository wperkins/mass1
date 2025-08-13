#! /bin/sh
# -------------------------------------------------------------
# file: runit-all.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 11, 1998 by William A. Perkins
# Last Change: Thu Sep 16 07:59:32 1999 by William A. Perkins <perk@mack.pnl.gov>
# -------------------------------------------------------------
set -x
set -e

                                # to trap floating point errors on SGI

TRAP_FPE='INVALID=ABORT(1);UNDERFL=ZERO;OVERFL=ABORT(1);INT_OVERFL=ABORT(1);DIVZERO=ABORT(1);DEBUG'
export TRAP_FPE

model='/projects/do9/src/perk/mass1/mass1_v084'
adjust="perl /home/perk/do9/database/flowadjust.pl"
years="1994 1996 1997"

for year in $years; do
                                # initial versions of link file and
                                # link bc file

    cp link_bc_files_start.dat link_bc_files.dat
    cp ../../BaseFiles/collink-1.prn link.dat

                                # make zeroed out lateral inflow files

    exec 3< code-files.txt

    rm -f lateral-inflow-files.dat

    count=0
    while read -u3 code start end link tsfile; do
        count=`expr $count + 1`
        name="$code-Lateral-$year.dat"
        echo " $count $name /" >> lateral-inflow-files.dat
        echo '# zero lateral inflow' > $name
        echo '01-01-1900 00:00:00 0.00 /' >> $name
        echo '01-01-2000 00:00:00 0.00 /' >> $name
    done

    exec 3<&-

                                # inital warmup period 

    rm -f mass1.cfg
    sed -e 's/@YEAR@/'$year'/' mass1-warmup-year.cfg > mass1.cfg
    rm -f *.out
    $model > /dev/null

                                # run the model and compute lateral
                                # inflow for each pool listed in the
                                # file

    exec 3< code-files.txt
    rm -f mass1.cfg
    sed -e 's/@YEAR@/'$year'/' mass1-run-year.cfg > mass1.cfg

    while read -u3 code start end link tsfile; do
        name="$code-Lateral-$year.dat"
        rm *.out
        $model > /dev/null
        $adjust -o $name $code $start $end $tsfile
        cp link.dat link-old.dat
        sed -e 's/^\( *'$link'  *[0-9][0-9]*  *[0-9][0-9]*  *[0-9][0-9]*  *\)3/\12/' link-old.dat > link.dat
        cp link_bc_files.dat link_bc_files_old.dat
        sed -e 's/'$code'-FBE/'$code'-Qtotal/' link_bc_files_old.dat > link_bc_files.dat
        sh forebay_plot.sh $year
    done
                                # we have to change the configuration
                                # file to make PRD a flow boundary


    sed -e '22s/^0/1/' \
        -e '50s/^1.0/0.25/' \
        -e '51s/1/4/' mass1.cfg > mass1-last-$year.cfg
    rm -f mass1.cfg
    cp mass1-last-$year.cfg mass1.cfg
    rm -f *.out
    $model > /dev/null
    sh forebay_plot.sh $year
    sh latflow_plot.sh $year
    rm -f mass1.cfg

done


