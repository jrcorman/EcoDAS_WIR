# Lake-res pair vs remaining lakes/reservoirs
# stats analysis

# Start with secchi disk comparison

sec.all$piece <- paste("remaining")
sec$piece <- paste("pair")
sec <- sec[,!(names(sec) %in% "PAIR_ID")]

sec.res <- rbind(sec.all[which(sec.all$type=="reservoir"),], sec[which(sec$type=="reservoir"),])
sec.lakes <- rbind(sec.all[which(sec.all$type=="lakes"),], sec[which(sec$type=="lakes"),])

t.test(sec.res$avg_all~sec.res$piece)
