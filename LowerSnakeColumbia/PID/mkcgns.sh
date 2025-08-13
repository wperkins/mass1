#!/bin/sh
# -------------------------------------------------------------
# file: mkcgns.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created June  2, 2025 by Perkins
# Last Change: 2025-06-12 11:49:03 d3g096
# -------------------------------------------------------------


set -xue

PATH=/file1/HanfordIBM/ibm/bin:$PATH
export PATH

. $HOME/Projects/HanfordReachIBM/pycgnsenv/bin/activate

prof2cgns="$HOME/Projects/CorpsSpillEffects/src/mass1/scripts/profile2cgns.new.py"

y=2008
yend=2025

pstart=1440                      # approx. Mar 1
pend=7320                        # approx. Nov 1

while [ "$y" -le "$yend" ]; do
    y1=`expr "$y" - 1`

    if [ -d $y ]; then
        (cd $y; python $prof2cgns -s $pstart -e $pend profile1.out $y.cgns $y.list; \
            hdf2adf $y.cgns )
    fi
    
    y=`expr "$y" + 1`
done
