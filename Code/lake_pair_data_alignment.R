#########################################
# Code for consolidating lake-pair info #
# generated by jrc                      #
# 18 Oct 2015                           #
#########################################

lp <- read.csv("Data/NLA_lakepairs_50km.csv")
names(lp)
# Columns 1 - 63 include reservoir info
# Columns 64 - 138 include lake info

# Add identifier for pairs
lp$pairs <- seq(1:66)

# Reservoirs of interest
res <- cbind(lp$pairs, as.character(lp$ManMadeSITE_ID))
res <- as.data.frame(res)
# list of res names
res.names <- unique(res[,2])

# Lakes of interest
lakes <- cbind(lp$pairs, as.character(lp$SITE_ID))
lakes <- as.data.frame(lakes)
# list of lake names
lakes.names <- unique(lakes[,2])

pairs <- rbind(res, lakes)
colnames(pairs) <- c("PAIR_ID", "SITE_ID")

library(plyr)
library(reshape2)
library(ggplot2)

# Function for combining datasets
# filename as "Data/NLA2007_secchi.csv"
# analyte as "variable"
pair <- function(filename, analyte){
  data <- read.csv(filename)
  res <- subset(data, SITE_ID %in% res.names)
  lakes <- subset(data, SITE_ID %in% lakes.names)
  # If >1 entry, this will calculate the mean
  res.mean <- aggregate(res[,analyte], by = list(res$SITE_ID), FUN = "mean")
  res.mean$type <- rep("reservoir")
  names(res.mean) <- c("SITE_ID", "avg", "type")
  lakes.mean <- aggregate(lakes[,analyte], by = list(lakes$SITE_ID), FUN = "mean")
  lakes.mean$type <- rep("lakes")
  names(lakes.mean) <- c("SITE_ID", "avg", "type")
  # combine secchi datasets into one long dataframe
  rl.mean <- rbind(res.mean, lakes.mean)
  # Combine all info into one dataframe
  cdata <- merge(pairs, rl.mean, by="SITE_ID")
  cdata$ID <- paste(cdata$SITE_ID, cdata$type, sep="_")
  cdata$analyte <- paste(analyte)
  cdata$avg_all <- cdata$avg
  return(cdata)
}
# Modification for temperature plots, a set Depth
pair.temp <- function(filename, analyte, a){
  data <- read.csv(filename)
  res.all <- subset(data, SITE_ID %in% res.names)
  res <- subset(res.all, DEPTH == a)
  lakes.all <- subset(data, SITE_ID %in% lakes.names)
  lakes <- subset(lakes.all, DEPTH == a)
  # If >1 entry, this will calculate the mean
  res.mean <- aggregate(res[,analyte], by = list(res$SITE_ID), FUN = "mean")
  res.mean$type <- rep("reservoir")
  names(res.mean) <- c("SITE_ID", "avg", "type")
  lakes.mean <- aggregate(lakes[,analyte], by = list(lakes$SITE_ID), FUN = "mean")
  lakes.mean$type <- rep("lakes")
  names(lakes.mean) <- c("SITE_ID", "avg", "type")
  # combine secchi datasets into one long dataframe
  rl.mean <- rbind(res.mean, lakes.mean)
  # Combine all info into one dataframe
  cdata <- merge(pairs, rl.mean, by="SITE_ID")
  cdata$ID <- paste(cdata$SITE_ID, cdata$type, sep="_")
  cdata$analyte <- paste(analyte)
  cdata$avg_all <- cdata$avg
  return(cdata)
}
# Function for graphing density distributions
# Follows from above pair
dgraph <- function(df, xtitle, legend){
  sdata <- ddply(df, .(type), summarize,
                 mean = mean(avg_all, na.rm=TRUE),
                 median = median(avg_all, na.rm=TRUE))
  p <- ggplot(df, aes(x=avg, fill=type)) + 
    geom_density(alpha=.75, size=1, aes(colour=type)) +
    geom_vline(data=sdata, aes(xintercept=mean,  colour=type),
               linetype="solid", size=1) +
    geom_vline(data=sdata, aes(xintercept=median,  colour=type),
               linetype="dashed", size=1) +
    theme_classic(12) +
    scale_fill_manual(values=c("#222222", "white")) +
    scale_color_manual(values=c("#222222", "#999999")) +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    xlab(xtitle)
  if(legend == "yes") a <-  p + theme(legend.position=c(0.85,0.85))
  if(legend == "no") a <- p + theme(legend.position="none")
  a
}

# Secchi Disk
sec <- pair("Data/NLA2007_secchi.csv", "SECMEAN")
sec$avg[sec$avg > quantile(sec$avg, prob=0.99, na.rm=TRUE)] <- quantile(sec$avg, prob=0.99, na.rm=TRUE)
dsec <- dgraph(sec, "Secchi Depth (m)", "no")
dsec
# Temp at surface
temp_1m <- pair.temp("Data/NLA2007_temp.csv", "TEMP_FIELD", 1)
dtemp_1m <- dgraph(temp_1m, "Temp (oC) Surface", "no")

# Temp at bottom
temp <- read.csv("Data/NLA2007_temp.csv") #temp data
res.temp<- subset(temp, SITE_ID %in% res.names)
lakes.temp <- subset(temp, SITE_ID %in% lakes.names)
# there are multiple records of temp for some sites
# so, taking average of multiple readings
res.tempmean.bot <- ddply(res.temp, .(SITE_ID), summarize,
                          tempmean = min(TEMP_FIELD))
res.tempmean.bot$type <- rep("reservoir")
lakes.tempmean.bot <- ddply(lakes.temp, .(SITE_ID), summarize,
                            tempmean = min(TEMP_FIELD))
lakes.tempmean.bot$type <- rep("lakes")
# combine temp datasets into one long dataframe
ctemp <- rbind(res.tempmean.bot, lakes.tempmean.bot)
# Combine all info into one dataframe
tdatab <- merge(pairs, ctemp, by="SITE_ID")
names(tdatab) <- c("SITE_ID", "PAIR_ID", "avg", "type")
tdatab$avg_all <- tdatab$avg
dtemp_bottom <- dgraph(tdatab, "Bottom Temp (oC)", "no")

# Stratification proxy (Difference between top and bottom/depth)
temp <- read.csv("Data/NLA2007_temp.csv") #temp data
res.temp<- subset(temp, SITE_ID %in% res.names)
lakes.temp <- subset(temp, SITE_ID %in% lakes.names)
# there are multiple records of temp for some sites
# so, taking average of multiple readings
res.tempmean.s <- ddply(res.temp, .(SITE_ID), summarize,
                          templow = min(TEMP_FIELD),
                          temphigh = max(TEMP_FIELD),
                          depth.m = max(DEPTH))
res.tempmean.s$type <- rep("reservoir")
lakes.tempmean.s <- ddply(lakes.temp, .(SITE_ID), summarize,
                            templow = min(TEMP_FIELD),
                            temphigh = max(TEMP_FIELD),
                            depth.m = max(DEPTH))
lakes.tempmean.s$type <- rep("lakes")
strat <- rbind(res.tempmean.s, lakes.tempmean.s)
strat$delTemp_Depth <- (strat$temphigh - strat$templow)/strat$depth.m
pstrat <- merge(pairs, strat, by = "SITE_ID")
names(pstrat) <- c("SITE_ID", "PAIR_ID", "tl", "th", "depth.m", "type", "avg")
pstrat$avg_all <- pstrat$avg
dtemp_change <- dgraph(pstrat, "del Temp m-1", "no")

# Catchment Area
CA_all <- pair("Data/NLA2007_basin.csv", "BASINAREA_KM2")
pr <- 0.92
CA_all$avg[CA_all$avg > quantile(CA_all$avg, prob=pr, na.rm=TRUE)] <- quantile(CA_all$avg, prob=pr, na.rm=TRUE)
dCA <- dgraph(CA_all, "Catchment Area (km2)", "no")
dCA

# Lake area
LA <- pair("Data/NLA2007_lakes.csv", "LAKEAREA") #in km2
LAs <- LA
pr <- 0.9
LAs$avg[LAs$avg > quantile(LAs$avg, prob=pr, na.rm=TRUE)] <- quantile(LAs$avg, prob=pr, na.rm=TRUE)
dLA <- dgraph(LAs, "Lake Area (km2)", "no")

# Catchment area : lake area
CA <- pair("Data/NLA2007_basin.csv", "BASINAREA_KM2")
names(CA) <- c("Site", "PAIR_ID", "Basin.km2", "type", "ID", "analyte", "avg_all")
names(LA) <- c("Site", "PAIR_ID", "Lake.km2", "type", "ID", "analyte", "avg_all")
CA$id <- paste(CA$Site, "_", CA$type)
LA$id <- paste(LA$Site, "_", LA$type)
CA_LA <- merge(CA, LA, by = "id")
CA_LA$avg <- CA_LA$Basin.km2/CA_LA$Lake.km2
CA_LA$avg_all <- CA_LA$avg
CA_LA$type <- CA_LA$type.y
pr2 <- 0.9
CA_LA$avg[CA_LA$avg > quantile(CA_LA$avg, prob=pr2, na.rm=TRUE)] <- quantile(CA_LA$avg, prob=pr2, na.rm=TRUE)
dCALA <- dgraph(CA_LA, "CA:LA", "no")
dCALA

# Perimeter 
peri <- pair("Data/NLA2007_lakes.csv", "LAKEPERIM")
pr3 <- 0.95
peri$avg[peri$avg > quantile(peri$avg, prob=pr3, na.rm=TRUE)] <- quantile(peri$avg, prob=pr3, na.rm=TRUE)
dperi <- dgraph(peri, "Perimeter (km)", "no")
dperi

# Elevation
el <- pair("Data/NLA2007_lakes.csv", "ELEV_PT")
del <- dgraph(el, "Elevation (m)", "yes")
del

# Max Depth
depth <- pair("Data/NLA2007_lakes.csv", "DEPTHMAX")
ddepth <- dgraph(depth, "Max Depth (m)", "no")
boxplot(depth$avg ~ depth$type)

# Residence time
ei <- pair("Data/NLA2007_residence.csv", "E_I")
dei <- dgraph(ei, "E:I", "no")

rt <- pair("Data/NLA2007_residence.csv", "RT")
pr4 <- 0.98
rt$avg[rt$avg > quantile(rt$avg, prob=pr3, na.rm=TRUE)] <- quantile(rt$avg, prob=pr4, na.rm=TRUE)
drt <- dgraph(rt, "Residence Time", "no")
drt

############################
# All density plots as one #
############################
library(gridExtra)
grid.arrange(del, dCALA, dCA, dLA, dperi, 
             ddepth, dsec, dtemp_1m, dtemp_bottom, dtemp_change, ncol=2)

# Density plots as two figures
library(gridExtra)
pdf('Figures/Fig2A_densityplots_catchment.pdf', width=7, height=5)
grid.arrange(del, dCALA, dCA, dLA, dperi, ddepth, drt, ncol=3)
dev.off()
pdf('Figures/Fig2B_densityplots_waterbody.pdf', width=4.5, height=3.5)
grid.arrange(dsec, dtemp_1m, dtemp_bottom, dtemp_change, ncol=2)
dev.off()


# All boxplots as one
par(mfrow=c(2,2))
boxplot(avg ~ type, data= sec, outline=FALSE, ylab= "Secchi Depth (m)")
boxplot(avg ~ type, data= temp_1m, outline=FALSE, ylab= "Temp (oC)")
boxplot(avg ~ type, data= peri, outline=FALSE, ylab= "Perimeter (units)")
boxplot(avg ~ type, data= el, outline=FALSE, ylab= "Elevation (units)")

# Merge all datasets
cdata <- rbind(sec, temp_1m, CA, LA, peri, el, depth)

