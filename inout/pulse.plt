# gnuplot 4.4 - 4.6
set term postscript eps size 8.5cm,4.7cm clip enhanced color font 'Times-Roman' 16
# Note: font size in eps terminal is reduced by exactly half: 16 -> 8
set out "pulse.eps"

set style line 1 linetype 1 linecolor 0 linewidth "3pt"
set style line 2 linetype 1 linecolor 3 linewidth "3pt"
set style line 3 linetype 1 linecolor 1 linewidth "3pt"
set style line 4 linetype 1 linecolor 2 linewidth "3pt"
set style line 5 linetype 1 linecolor 4 linewidth "3pt"

set lmargin at screen 0.1 
set bmargin at screen 0.135
set tmargin at screen 0.95
set rmargin at screen 0.95
set notitle
set xtics offset 0,0.3 # move xtics a bit up (by 0.3 characters)
set ytics offset 0.5 # move y-axis tic labels half a character to the right
set mxtics 2 # make 1 minor tic between major tics on the x-axis
set mytics 2 # make 1 minor tic between major tics on the y-axics
set xlabel "time (ns)" offset 0,0.8       # move x-label up ...
set ylabel "amplitude (MHz)" offset 2.8,0 # ... and y-label to the right
set key inside horizontal right samplen 2
plot "pulse.dat" using 1:(sqrt($2*$2 + $3*3)) with lines linestyle 2 title "pulse"

