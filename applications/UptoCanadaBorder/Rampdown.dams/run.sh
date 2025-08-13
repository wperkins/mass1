#! /bin/sh
# -------------------------------------------------------------
# file: run.sh
# 
# This generates hotstarts for several Grand Coulee forebay
# elevations.  Here are the GCL stages at the start of the calendar
# year:
# 
# 01-01-1998 00:00:00         1274.900 /(WMD-OPS   )
# 01-01-1999 01:00:00         1278.100 /(WMD-OPS   )
# 01-01-2000 01:00:00         1272.100 /(WMD-OPS   )
# 01-01-2001 00:00:00         1267.700 /(WMD-OPS   )
# 01-01-2002 00:00:00         1284.200 /(WMD-OPS   )
# 01-01-2003 00:00:00         1286.200 /(WMD-TDG   )
# 01-01-2004 00:00:00         1284.900 /(WMD-TDG   )
# 01-01-2005 00:00:00         1287.100 /(WMD-TDG   )
# 01-01-2006 00:00:00         1287.700 /(WMD-TDG   )
# 01-01-2007 00:00:00         1286.100 /(WMD-TDG   )
# 01-01-2008 00:00:00         1286.400 /(WMD-TDG   )
# 01-01-2009 00:00:00         1285.200 /(WMD-TDG   )
# 01-01-2010 00:00:00         1283.400 /(WMD-TDG   )
# 01-01-2011 00:00:00         1284.600 /(WMD-TDG   )
# 01-01-2012 00:00:00         1281.200 /(WMD-TDG   )
# 01-01-2013 00:00:00         1284.200 /(WMD-TDG   )
# 01-01-2014 00:00:00         1280.200 /(WMD-TDG   )
# 01-01-2015 00:00:00         1287.600 /(WMD-TDG   )
# 01-01-2016 00:00:00         1285.300 /(WMD-TDG   )
# 01-01-2017 00:00:00         1281.600 /(WMD-TDG   )
# 01-01-2018 00:00:00         1286.000 /(WMD-TDG   )
#
# When cold starting a simulation, the hotstart GCL stage needs to be
# very close to the boundary condition stage.  Choose a hotstart
# carefully depending on the year.
#
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created May 14, 2002 by William A. Perkins
# Last Change: 2018-11-16 08:22:50 d3g096
# -------------------------------------------------------------


model=/net/flophouse/files0/perksoft/linux64/mass1/bin/mass1
gclstage="1272 1274 1275 1278 1281 1286 1287"
#gclstage="1287"

for e in $gclstage; do
    sed -e "s/1272/$e/g" GC_elev_base.dat > GC_elev.dat
    $model # > /dev/null
    mv restart.dat restart.$e
    mv profile1.out profile1.$e.out
done
