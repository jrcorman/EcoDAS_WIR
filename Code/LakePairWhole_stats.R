##########################################
# Are lakes and reservoirs in lake-pairs #
# representative of the NLA Dataset?     #
# Code by jrc, started 160127            #
##########################################

source("Code/LakePairWhole.R")

# Consolidate paired and remaining datasets into one
data_prep <- function(data_pair, data_rem){
  data_pair$piece <- paste("pair")
  data_pair <- data_pair[,!(names(data_pair) %in% "PAIR_ID")]
  data_rem$piece <- paste("remaining")
  adata <- rbind(data_pair, data_rem)
  cdata <- na.omit(adata)
  cdata$id <- paste(cdata$type, cdata$piece, sep="_")
  return(cdata)
}

data_sec <- data_prep(sec, sec.all)
data_CA <- data_prep(CA_all, CA_all.all)
data_peri <- data_prep(peri, peri.all)
data_el <- data_prep(el, el.all)
data_depth <- data_prep(depth, depth.all)

# Report results of t-test
library(plyr)
pair_whole <- function(cdata){
  d_ply(cdata, .(type), function(x)
    print(t.test(x$avg ~ x$piece, var.equal=FALSE)))
  count(cdata$id)
    }

pair_whole(data_sec)
pair_whole(data_CA) #sig, both
pair_whole(data_peri) #sig, both
pair_whole(data_el) #sig, lakes
pair_whole(data_depth)

# Making graphs when results are significant
# Graphing pair versus remaining
dgraph.piece <- function(df, xtitle, legend){
 # sdata <- ddply(df, .(piece), summarize,
 #               mean = mean(avg_all, na.rm=TRUE),
#               median = median(avg_all, na.rm=TRUE))
  p <- ggplot(df, aes(x=avg, fill=piece)) + 
    geom_density(alpha=.75, size=1) +
  #  geom_vline(data=sdata, aes(xintercept=mean,  colour=piece),
  #             linetype="solid", size=1) +
  #  geom_vline(data=sdata, aes(xintercept=median,  colour=piece),
  #             linetype="dashed", size=1) +
    theme_classic(14) +
    scale_fill_manual(values=c("#222222", "white")) +
    scale_color_manual(values=c("#222222", "#999999")) +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    xlab(xtitle)
  if(legend == "yes") a <-  p + theme(legend.position=c(0.85,0.85)) + facet_wrap(~type, ncol=1)
  if(legend == "no") a <- p + theme(legend.position="none")+ facet_wrap(~type, nolc=1)
  a
}

# dgraph.piece(data_CA, "Catchment Area", "yes")
pr <- 0.9
data_CA$avg[data_CA$avg > quantile(data_CA$avg, prob=pr, na.rm=TRUE)] <- quantile(data_CA$avg, prob=pr, na.rm=TRUE)
dgraph.piece(data_CA, "Catchment Area", "yes")

pr <- 0.95
data_peri$avg[data_peri$avg > quantile(data_peri$avg, prob=pr, na.rm=TRUE)] <- quantile(data_peri$avg, prob=pr, na.rm=TRUE)
dgraph.piece(data_peri, "Perimeter", "yes")

dgraph.piece(data_el, "Elevation", "yes")
