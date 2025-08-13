#!/bin/sh

set -xue

start="12/31/1999"
end="01/01/2020"
massbc="perl /home/d3g096/do9/database/massbc.pl"

# Copy from LowerSnakeColumbia -- make sure they match
# $massbc -g -Q -O "-12 hour" -o Tucannon-Discharge.dat 13344500 "$start" "$end"
# $massbc -g -Q -O "-12 hour" -o Palouse-Discharge.dat 13351000 "$start" "$end"

# If we use Spaulding as the upstream Clearwater boundary:
$massbc -g -h -Q -0 -o Spalding-Discharge.dat 13342500 "$start" "$end"
$massbc -w -q -T -0 -o LEWI-Temperature.dat LEWI "$start" "$end"
$massbc -g -h -T -0 -o Spalding-Temperature.dat 13342500 "$start" "$end"
$massbc -g -T -O "12 hour" -o Spalding-Daily-Temperature.dat 13342500 "$start" "$end"

# If we use Peck as the upstream Clearwater boundary:
$massbc -g -h -Q -0 -o Peck-Discharge.dat 13341050 "$start" "$end"
$massbc -w -q -T -0 -o PEKI-Temperature.dat PEKI "$start" "$end"
$massbc -g -h -T -0 -o Peck-Temperature.dat 13341050 "$start" "$end"
$massbc -g  -T -O "12 hour" -o Peck-Daily-Temperature.dat 13341050 "$start" "$end"

# Copy from LowerSnakeColumbia -- make sure they match
# Anatone is the upstream Snake boundary:
# $massbc -g -h -Q -0 -o Anatone-Discharge.dat 13334300 "$start" "$end"
# $massbc -w -q -T -0 -o ANQW-Monitor-Temperature.dat ANQW  "$start" "$end"
# $massbc -g -h -T -0 -o Anatone-Temperature.dat 13334300 "$start" "$end"
# $massbc -g -T -O "12 hour" -o Anatone-Daily-Temperature.dat 13334300 "$start" "$end"

# Copy from LowerSnakeColumbia -- make sure they match
# python2.7 gen-temp.py --output Tucannon-Temperature.dat  \
#     --title "Generated Tucannon River Temperature" \
#     11.1 7.5 171.9 5.3 154 303

# python2.7 gen-temp.py --output Palouse-Temperature.dat  \
#     --title "Generated Palouse River Temperature" \
#     11.1 7.5 171.9 5.3 154 303


