set terminal png
set output 'multi_graph_NGC_7078.png'
set logscale y
set style data linespoints
set title "Time of destabilization in the NGC 7078 GC vs distance from center"
set xlabel "r (pc)"
set ylabel "time (year)"
plot "profile_NGC_7078_1.ris" using 1:2 title "M_{test} = 0.1", \
     "profile_NGC_7078_2.ris" using 1:2 title "M_{test} = 0.2", \
     "profile_NGC_7078_3.ris" using 1:2 title "M_{test} = 0.3", \
     "profile_NGC_7078_4.ris" using 1:2 title "M_{test} = 0.4", \
     "profile_NGC_7078_5.ris" using 1:2 title "M_{test} = 0.5", \
     "profile_NGC_7078_6.ris" using 1:2 title "M_{test} = 0.6", \
     "profile_NGC_7078_7.ris" using 1:2 title "M_{test} = 0.7", \
     
