#!/usr/bin/gnuplot -persist
#
#    
#    	G N U P L O T
#    	Linux version 3.7
#    	patchlevel 1
#    	last modified Fri Oct 22 18:00:00 BST 1999
#    
#    	Copyright(C) 1986 - 1993, 1998, 1999
#    	Thomas Williams, Colin Kelley and many others
#    
#    	Type `help` to access the on-line reference manual
#    	The gnuplot FAQ is available from
#    	<http://www.ucc.ie/gnuplot/gnuplot-faq.html>
#    
#    	Send comments and requests for help to <info-gnuplot@dartmouth.edu>
#    	Send bugs, suggestions and mods to <bug-gnuplot@dartmouth.edu>
#    
# set terminal x11 
# set output
set term postscript eps color dashed "Helvetica" 16
set format y "%.1f"
set nogrid
set key left
set key title "@TITLE@"
set xtics mirror norotate  ("Jan 1" 1, "Feb 1" 32, "Mar 1" 60, "Apr 1" 91, "May 1" 121, "Jun 1" 152, "Jul 1" 182, "Aug 1" 212, "Sep 1" 244, "Oct 1" 274, "Nov 1" 305, "Dec 1" 335, "Dec 31" 365)
set xlabel "Day of Year" 
set xrange [ 1.0000 : 365.000 ] noreverse nowriteback
set ylabel "Temperature,  C" 
set yrange [0:27.5]
plot '<perl daily2julday.pl @LOC@-temp.daily' using 2:($5) title 'Daily Maximum' with lines lt 1 lc 7, \
     '@LOC@-temp.median' using 1:($5) title 'Median for Day' with points ls 7 lc 3
#    EOF
