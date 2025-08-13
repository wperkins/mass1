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


python2.7 gen-temp.py --output Anatone-Temperature.dat  \
    --title "Generated Anatone Temperature" \
    12.6 9.9 156.2 4.9 183 313

python2.7 gen-temp.py --output Clatskaine-Temperature.dat  \
    --title "Generated Clatskaine Temperature" \
    9.6  4.7 163 2.1 135 310

python2.7 gen-temp.py --output Cowlitz-Temperature.dat  \
    --title "Generated Cowlitz Temperature" \
    10.6   5.4 	158.8 	2.2  172  306
 
python2.7 gen-temp.py --output Deschutes-Temperature.dat  \
    --title "Generated Deschutes Temperature" \
    11.8 6.4 162.3 0 155 303

python2.7 gen-temp.py --output HoodRiver-Temperature.dat  \
    --title "Generated Hood River Temperature" \
    7.9 3.1 166.3 0 166 304

python2.7 gen-temp.py --output JohnDay-Temperature.dat  \
    --title "Generated John Day Temperature" \
    13.4 10.8 171.6 4.3 157 305

python2.7 gen-temp.py --output Klickitat-Temperature.dat  \
    --title "Generated Klickitat Temperature" \
     10.4  7.3  174.7  3.8  141  302

python2.7 gen-temp.py -C --output Lewis-Temperature.dat  \
    --title "Generated Lewis Temperature" \
     7.431  4.984  256.696  3.641  53.432  222.438
  
python2.7 gen-temp.py --output Orofino-Temperature.dat  \
    --title "Generated Orofino Temperature" \
    11.6 11.4 173.8 9.6 150 311

python2.7 gen-temp.py --output Palouse-Temperature.dat  \
    --title "Generated Palouse Temperature" \
    12.5 11.2 172.4 4.6 151 302

python2.7 gen-temp.py --output Sandy-Temperature.dat  \
    --title "Generated Sandy Temperature" \
    10.7 6.3 148.1 3.1 149 330

python2.7 gen-temp.py --output Tucannon-Temperature.dat  \
    --title "Generated Tucannon Temperature" \
    11.1  7.5 171.9 5.3 154 303

python2.7 gen-temp.py -C --output Umatilla-Temperature.dat  \
    --title "Generated Umatilla Temperature" \
    13.234 9.739 256.007 6.546 62.829 224.859

python2.7 gen-temp.py --output WallaWalla-Temperature.dat  \
    --title "Generated Walla Walla Temperature" \
    13.9 11 173.1 6 149 298

python2.7 gen-temp.py --output WhiteSalmon-Temperature.dat  \
    --title "Generated White Salmon Temperature" \
    7.9 3.1 166.3 0 166 304

python2.7 gen-temp.py --output Willamette-Temperature.dat  \
    --title "Generated Willamette Temperature" \
    13.9  8.5 164.1 4.6 160 308

python2.7 gen-temp.py --output Wind-Temperature.dat  \
    --title "Generated Wind Temperature" \
    8.6  5  163.5  3.5  143  317

python2.7 gen-temp.py --output Yakima-Temperature.dat  \
    --title "Generated Yakima Temperature" \
    14.1  11.3  170.9  4.5  176  300
