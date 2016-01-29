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
data_rt <- data_prep(rt, rt.all)
data_ei <- data_prep(ei, ei.all)
data_LA <- data_prep(LA, LA.all)
data_tempbot <- data_prep(tdatab, tdatab.all)
data_strat <- data_prep(strat, strat.all)
data_tempsfc <- data_prep(temp_1m, temp_1m.all)

# Catchment area to lake area calculation
CA_LA1 <- CA_LA[, names(CA_LA) %in% c("id", "avg", "type")]
CA_LA.all1 <- CA_LA.all[, names(CA_LA.all) %in% c("ID", "avg", "type")]
names(CA_LA1) <- c("ID", "avg", "type")
data_CALA <- data_prep(CA_LA1, CA_LA.all1)

# Temperature info


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
pair_whole(data_rt) #sig, lakes
pair_whole(data_ei) # sig, both
pair_whole(data_CALA) #sig, res
pair_whole(data_LA) #sig, res
pair_whole(data_tempbot)
pair_whole(data_strat)
pair_whole(data_tempsfc) #sig, res

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

pr <- 0.99
data_rt$avg[data_rt$avg > quantile(data_rt$avg, prob=pr, na.rm=TRUE)] <- quantile(data_rt$avg, prob=pr, na.rm=TRUE)
dgraph.piece(data_rt, "Residence time", "yes")

dgraph.piece(data_ei, "E:I", "yes")

pr <- 0.90
data_CALA$avg[data_CALA$avg > quantile(data_CALA$avg, prob=pr, na.rm=TRUE)] <- quantile(data_CALA$avg, prob=pr, na.rm=TRUE)
dgraph.piece(data_CALA, "CA:LA", "yes")

pr <- 0.90
data_LA$avg[data_LA$avg > quantile(data_LA$avg, prob=pr, na.rm=TRUE)] <- quantile(data_LA$avg, prob=pr, na.rm=TRUE)
dgraph.piece(data_LA, "Lake Area", "yes")

dgraph.piece(data_tempsfc, "Surface Temp", "yes")
