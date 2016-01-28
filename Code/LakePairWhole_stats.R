##########################################
# Are lakes and reservoirs in lake-pairs #
# representative of the NLA Dataset?     #
# Code by jrc, started 160127            #
##########################################

source("Code/LakePairWhole.R")

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

# Report results of t-test
library(plyr)

pair_whole <- function(data_pair, data_rem){
  data_pair$piece <- paste("pair")
  data_pair <- data_pair[,!(names(data_pair) %in% "PAIR_ID")]
  data_rem$piece <- paste("remaining")
  adata <- rbind(data_pair, data_rem)
  cdata <- na.omit(adata)
  cdata$id <- paste(cdata$type, cdata$piece, sep="_")
  d_ply(cdata, .(type), function(x)
    print(t.test(x$avg ~ x$piece, var.equal=FALSE)))
  count(cdata$id)
    }

pair_whole(sec, sec.all)
pair_whole(CA_all, CA_all.all)
pair_whole(peri, peri.all)
pair_whole(el, el.all)
pair_whole(depth, depth.all)
