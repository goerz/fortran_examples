# gnuplot 4.4 - 4.6
set term postscript eps size 8.5cm,8cm clip enhanced color font 'Times-Roman' 16
# Note: font size in eps terminal is reduced by exactly half: 16 -> 8
set out "pulse_multi.eps"

set style line 1 linetype 1 linecolor 0 linewidth "3pt"
set style line 2 linetype 1 linecolor 3 linewidth "3pt"
set style line 3 linetype 1 linecolor 1 linewidth "3pt"
set style line 4 linetype 1 linecolor 2 linewidth "3pt"
set style line 5 linetype 1 linecolor 4 linewidth "3pt"

set multiplot
set notitle

set format x "" # suppress the x-axis tic labels for this subplot
set lmargin at screen 0.1 
set bmargin at screen 0.68
set tmargin at screen 0.98
set rmargin at screen 0.95
set xtics offset 0,0.3 # move xtics a bit up (by 0.3 characters)
set ytics offset 0.5 # move y-axis tic labels half a character to the right
set mxtics 2 # make 1 minor tic between major tics on the x-axis
set mytics 2 # make 1 minor tic between major tics on the y-axics
set key inside horizontal right samplen 2
set ylabel "amplitude (MHz)" offset 2.7,0 # ... and y-label to the right
plot "pulse.dat" using 1:(sqrt($2*$2 + $3*3)) with lines linestyle 1 title "abs(pulse)"

set format x "" # suppress the x-axis tic labels for this subplot
set lmargin at screen 0.1 
set bmargin at screen 0.38
set tmargin at screen 0.68
set rmargin at screen 0.95
set ylabel "amplitude (MHz)" offset 2.7,0 # ... and y-label to the right
plot "pulse.dat" using 1:2 with lines linestyle 2 title "Re(pulse)"

set format x "%g"
set lmargin at screen 0.1 
set bmargin at screen 0.08
set tmargin at screen 0.38
set rmargin at screen 0.95
set ylabel "amplitude (MHz)" offset 2.0,0 # ... and y-label to the right
set xlabel "time (ns)" offset 0,0.8       # move x-label up ...
plot "pulse.dat" using 1:3 with lines linestyle 3 title "Im(pulse)"

set nomultiplot
