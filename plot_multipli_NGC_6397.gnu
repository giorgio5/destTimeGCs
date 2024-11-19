set terminal png
set output 'multi_graph_NGC_6397.png'
set logscale y
set style data linespoints
set title "Time of destabilization in the NGC 6397 GC vs distance from center"
set xlabel "r (pc)"
set ylabel "time (year)"
plot "profile_NGC_6397_1.ris" using 1:2 title "M_{test} = 0.1", \
     "profile_NGC_6397_2.ris" using 1:2 title "M_{test} = 0.2", \
     "profile_NGC_6397_3.ris" using 1:2 title "M_{test} = 0.3", \
     "profile_NGC_6397_4.ris" using 1:2 title "M_{test} = 0.4", \
     "profile_NGC_6397_5.ris" using 1:2 title "M_{test} = 0.5", \
     "profile_NGC_6397_6.ris" using 1:2 title "M_{test} = 0.6", \
     "profile_NGC_6397_7.ris" using 1:2 title "M_{test} = 0.7", \
     
