set terminal png
set output 'multi_graph.png'
set logscale y
set style data linespoints

plot "profile_NGC_6397_1.ris" using 1:2 title "Flow 1", \
     "profile_NGC_6397_2.ris" using 1:2 title "Flow 2", \
     "profile_NGC_6397_3.ris" using 1:2 title "Flow 3", \
     "profile_NGC_6397_4.ris" using 1:2 title "Flow 4", \
     "profile_NGC_6397_5.ris" using 1:2 title "Flow 5", \
     "profile_NGC_6397_6.ris" using 1:2 title "Flow 6", \
     "profile_NGC_6397_7.ris" using 1:2 title "Flow 7", \
     
