#!/bin/sh

set -xue

start="01/01/2016"
end="01/01/2017"
massbc="perl /home/d3g096/do9/database/massbc.pl"

# $massbc -F -0 -o GCL-Elev.dat GCL "$start" "$end"
$massbc -Q -O "-30 minute" -o GCL-Discharge.dat GCL "$start" "$end"
$massbc -g -h -Q -0 -o Border-Discharge.dat 12399500 "$start" "$end"
$massbc -g -h -Q -0 -o Kettle-Discharge.dat 12404900 "$start" "$end"
$massbc -g -Q -O "12 hour" -o Kettle-Discharge-daily.dat 12404500 "$start" "$end"
$massbc -g -h -Q -0 -o Colville-Discharge.dat 12409000 "$start" "$end"
$massbc -g -Q -O "12 hour" -o Colville-Discharge-daily.dat 12409000 "$start" "$end"
$massbc -g -h -Q -0 -o Sanpoil-Discharge.dat 12434590 "$start" "$end"
$massbc -g -Q -O "12 hour" -o Spokane-Discharge.dat 12433000 "$start" "$end"

# Provisional data not available 
# This is actually the reverse: negative should be withdrawl via pump to Banks Lake.  
# $massbc -g -Q -O "12 hour" -o 12435500 BanksCanal-Discharge.dat 12435500 "$start" "$end"
