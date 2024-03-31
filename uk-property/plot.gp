set datafile separator "\t"
set xdata time
set timefmt "%Y%m"
set format x "%Y-%m"
set terminal pngcairo size 800,600 enhanced font 'Verdana,8'
#set output 'plot.png'

set rmargin at screen 0.90

set yrange [0:800000]
set ytics nomirror
set y2tics
set y2range [0:800000]

set ylabel  "Price"
set y2label "Price"

set xtics rotate by -45

set palette defined (0 "gray", 1 "red")

unset colorbox

set grid lt 1 lw 1 lc rgb "#dddddd"

plot 'interpolated.tsv' using 1:2:3 with points palette pt 7 notitle, \
     '' using 1:2 smooth cspline with lines lc "black" dt 1 lw 1 title 'Price', \
     '' using 1:($3>0 ? $2 : 1/0):3 with points palette pt 7 notitle
