test <- subset(b_corr_comp, decision == 1)

table(test$start_iter)
table(test$repls)


table(b_corr_comp$time)

# wenn 0.9 verwendet wird, steigt die Anzahl an Fehler etwas an, vor allem 
# nach den ersten Beobachtungen, weil es hier schneller zu einer Entscheidung 
# kommt als mit dem höheren Wert 

# wenn 0.99 verwendet wird, sinkt die Fehleranzahl. Die Anzahl an Fehlern die 
# in der ersten runde gleich gemacht werden sinkt, und dann treten eher fehler
# auf weil der Wert drum herum schwankt und dabei kann es auch sein, dass wenn 
# mehr als 10 beobvachtungen der wert wieder über den Threshold steigen würde