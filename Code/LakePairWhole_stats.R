# Lake-res pair vs remaining lakes/reservoirs
# stats analysis

source("LakePairWhole.R")

# Graphing pair versus remaining
dgraph.piece <- function(df, xtitle, legend){
  sdata <- ddply(df, .(piece), summarize,
                 mean = mean(avg_all, na.rm=TRUE),
                 median = median(avg_all, na.rm=TRUE))
  p <- ggplot(df, aes(x=avg, fill=piece)) + 
    geom_density(alpha=.75, size=1) +
        geom_vline(data=sdata, aes(xintercept=mean,  colour=piece),
               linetype="solid", size=1) +
    geom_vline(data=sdata, aes(xintercept=median,  colour=piece),
               linetype="dashed", size=1) +
    theme_classic(14) +
    scale_fill_manual(values=c("#222222", "white")) +
    scale_color_manual(values=c("#222222", "#999999")) +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    xlab(xtitle)
  if(legend == "yes") a <-  p + theme(legend.position=c(0.85,0.85))
  if(legend == "no") a <- p + theme(legend.position="none")
  a
}


# Start with secchi disk comparison

sec.all$piece <- paste("remaining")
sec$piece <- paste("pair")
sec <- sec[,!(names(sec) %in% "PAIR_ID")]

sec.res <- rbind(sec.all[which(sec.all$type=="reservoir"),], sec[which(sec$type=="reservoir"),])
sec.lakes <- rbind(sec.all[which(sec.all$type=="lakes"),], sec[which(sec$type=="lakes"),])

cdata<-rbind(sec, sec.all)

t.test(sec.res$avg_all~sec.res$piece)
boxplot(sec.res$avg_all~sec.res$piece)
dgraph.piece(sec.res, "Secchi Depth (m)", "yes")

# Create graph with boxplot and density plots
# and report results of t-test
pair_whole <- function(data_pair, data_rem){
  data_pair$piece <- paste("pair")
  data_pair <- data_pair[,!(names(data_pair) %in% "PAIR_ID")]
  data_rem$piece <- paste("remaining")
  cdata <- rbind(data_pair, data_rem)
  stats <- function(x){
    print(t.test(x$avg ~ x$piece))
  }
  print(tapply(cdata, cdata$type, stats))
}

pair_whole(sec, sec.all)
