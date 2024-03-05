set datafile separator "\t"
set terminal pngcairo size 600,400 enhanced font 'Verdana,8'
set rmargin at screen 0.90
set grid lt 1 lw 1 lc rgb "#dddddd"
plot 'test.tsv' using 1:2 with points pt 7 lc rgb "gray" notitle
