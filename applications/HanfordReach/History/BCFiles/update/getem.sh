#!/bin/sh

set -xue

startdate="11/10/2019"
enddate="06/01/2025"

scriptdir="/home/d3g096/do9/database"

cmd="perl -I $scriptdir $scriptdir/massbc.pl"


$cmd -Q -w -o PRD-Flow.dat PRD "$startdate" "$enddate"
$cmd -q -T -w -0 -o PRXW-Temp.dat PRXW "$startdate" "$enddate"
$cmd -Q -w -o IHR-Flow.dat IHR "$startdate" "$enddate"
$cmd -q -T -w -0 -o IDSW-Temp.dat IDSW "$startdate" "$enddate"
$cmd -F -0 -w -o MCN-FBE.dat MCN "$startdate" "$enddate"
$cmd -g -Q -o Yakima-Flow.dat 12510500 "$startdate" "$enddate"
