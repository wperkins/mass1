#! /bin/sh
# -------------------------------------------------------------
# file: runit.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created December 11, 1998 by William A. Perkins
# Last Change: 2020-05-07 06:30:21 d3g096
# -------------------------------------------------------------
# $Id$

set -x
set -e

                                # to trap floating point errors on SGI

model=${MODEL-../../../build/mass1_new}


ln -sf mass1-warmup.cfg mass1.cfg

$model

ln -sf mass1-run.cfg mass1.cfg

$model

rm -f mass1.cfg

gnuplot plot.gp > plot.eps
gnuplot plot-temp.gp > plot-temp.eps