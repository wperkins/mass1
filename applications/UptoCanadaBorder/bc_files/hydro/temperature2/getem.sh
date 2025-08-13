#!/bin/sh

set -xue

start="01/01/2015"
end="01/01/2019"
massbc="perl /home/d3g096/do9/database/massbc.pl"

#$massbc -Q -O "-30 minute" -o GCL-Discharge.dat GCL "$start" "$end"
#$massbc -g -h -Q -0 -o Border-Discharge.dat 12399500 "$start" "$end"
#$massbc -g -h -Q -0 -o Kettle-Discharge.dat 12404900 "$start" "$end"
#$massbc -g -Q -O "12 hour" -o Kettle-Discharge-daily.dat 12404500 "$start" "$end"
#$massbc -g -h -Q -0 -o Colville-Discharge.dat 12409000 "$start" "$end"
#$massbc -g -Q -O "12 hour" -o Colville-Discharge-daily.dat 12409000 "$start" "$end"
#$massbc -g -h -Q -0 -o Sanpoil-Discharge.dat 12434590 "$start" "$end"
#$massbc -g -Q -O "12 hour" -o Spokane-Discharge.dat 12433000 "$start" "$end"

# Provisional data not available 
# This is actually the reverse: negative should be withdrawl via pump to Banks Lake.  
# $massbc -g -Q -O "12 hour" -o 12435500 BanksCanal-Discharge.dat 12435500 "$start" "$end"

# Shift elevation datum for Grand Coulee forebay stage
# $massbc -F -0 -o GCL-Elev-NGVD29.dat GCL "$start" "$end"
#awk -f - GCL-Elev-NGVD29.dat > GCL-Elev.dat <<EOF
#NR == 1 { print; next; }
#{ printf("%s %s %.3f / %.3f\n", \$1, \$2, \$3 + 2.502, \$3 + 0.0); }
#EOF

#$massbc -q -0 -o CIBW-Temperature.dat CIBW "$start" "$end"


python2.7 gen-temp.py --output Kettle-Temperature.dat  \
    --title "Generated Kettle River Temperature" \
    10.4 11.3 169 8.5 171 305

python2.7 gen-temp.py --output Colville-Temperature.dat  \
    --title "Generated Colville River Temperature" \
    10.3  9.8 169.5 3.4 145 304

python2.7 gen-temp.py --output Spokane-Temperature.dat  \
    --title "Generated Spokane River Temperature" \
    11.2  8.3  151.9  2  138  311

python2.7 gen-temp.py --output Okanagon-Temperature.dat  \
    --title "Generated Okanagon Temperature" \
    11.6 11.8 168.6 8 189 298

python2.7 gen-temp.py --output Methow-Temperature.dat  \
    --title "Generated Methow Temperature" \
    10.8  10.1 173.9 8.8 173 311

python2.7 gen-temp.py --output Chelan-Temperature.dat  \
    --title "Generated Chelan Temperature" \
    12.7 9 153.1 3.9 243 298

python2.7 gen-temp.py --output Entiat-Temperature.dat  \
    --title "Generated Entiat Temperature" \
    9.8 9.5 170.5 8.4 174 314

python2.7 gen-temp.py --output Wenatchee-Temperature.dat  \
    --title "Generated Wenatchee Temperature" \
    10.5 9.1 167.9 7.1 169 315

python2.7 gen-temp.py --output Crab-Temperature.dat  \
    --title "Generated Crab Temperature" \
    13.1  10.7 172.9 3.6 176 300

python2.7 gen-temp.py --output Snake-Temperature.dat  \
    --title "Generated Wind Temperature" \
    13  10  150.1  5  189  326

python2.7 gen-temp.py --output Yakima-Temperature.dat  \
    --title "Generated Yakima Temperature" \
    14.1  11.3  170.9  4.5  176  300
