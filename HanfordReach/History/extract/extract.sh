#! /bin/sh
# -------------------------------------------------------------
# file: extract.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 31, 2012 by William A. Perkins
# Last Change: 2025-05-20 09:19:25 d3g096
# -------------------------------------------------------------

set -xeu

# Note: MASS1 from the master branch must be used. The format of the
# profile file changes in the dhsvm branch.

mass1dir="/home/d3g096/Projects/MASS1/src/mass1-redux"
origquadrants="../../Forecast/quadrm.txt"
extract="$mass1dir/scripts/profile_extract.py"

gawk -F'|' -f - "$origquadrants" <<EOF
NR == 1 { next; }
{ 
    quad=\$1
    segment=\$3
    out=sprintf("segment.%02d.txt", segment)
    print \$1, \$4 > out
}
EOF

for f in segment.??.txt; do
    out=`expr "$f" : '\(.*\)\.txt$' `
    out="$out.mass1.txt"
    python2.7 "$extract" \
        --column-names \
        --metric \
        --section \
        --temperature \
        --start '01/01/2021 00:00' \
	--end '12/31/2025 23:30' \
        --file "$f" ../profile1.out > "$out" 
done

