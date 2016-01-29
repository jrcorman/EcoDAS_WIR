# Script to divide datasets into those in pairs
# and those in entire dataset
# NOTE: Lakes or reservoirs in pairings are removed from population
# whole.

# Code below gets all reservoir and lake pair data needed
source("Code/lake_pair_data_alignment.R")

lake.info <- read.csv("Data/NLA2007_lakes.csv")
lakes.all <- subset(lake.info, LAKE_ORIGIN == "NATURAL")
res.all <- subset(lake.info, LAKE_ORIGIN == "MAN-MADE")
# Reservoirs and lakes remaining after pairs removed
lakes.rem <- subset(lakes.all, !(SITE_ID %in% lakes.names))
res.rem <- subset(res.all, !(SITE_ID %in% res.names))
lakes.id.rm <- unique(lakes.rem$SITE_ID)
res.id.rm <- unique(res.rem$SITE_ID)

# Function for removing pairs from complete dataset
whole <- function(filename, analyte){
  data <- read.csv(filename)
  res.all <- subset(data, SITE_ID %in% res.id.rm)
  lakes.all <- subset(data, SITE_ID %in% lakes.id.rm)
  # If >1 entry, this will calculate the mean
  res.all.mean <- aggregate(res.all[,analyte], by = list(res.all$SITE_ID), FUN = "mean")
  res.all.mean$type <- rep("reservoir")
  names(res.all.mean) <- c("SITE_ID", "avg", "type")
  lakes.all.mean <- aggregate(lakes.all[,analyte], by = list(lakes.all$SITE_ID), FUN = "mean")
  lakes.all.mean$type <- rep("lakes")
  names(lakes.all.mean) <- c("SITE_ID", "avg", "type")
  # combine secchi datasets into one long dataframe
  rl.all.mean <- rbind(res.all.mean, lakes.all.mean)
  rl.all.mean$ID <- paste(rl.all.mean$SITE_ID, rl.all.mean$type, sep="_")
  rl.all.mean$analyte <- paste(analyte)
  rl.all.mean$avg_all <- rl.all.mean$avg
  return(rl.all.mean)
}

sec.all <- whole("Data/NLA2007_secchi.csv", "SECMEAN")
CA_all.all <- whole("Data/NLA2007_basin.csv", "BASINAREA_KM2")
LA.all <- whole("Data/NLA2007_lakes.csv", "LAKEAREA") #in km2
peri.all <- whole("Data/NLA2007_lakes.csv", "LAKEPERIM")
el.all <- whole("Data/NLA2007_lakes.csv", "ELEV_PT")
depth.all <- whole("Data/NLA2007_lakes.csv", "DEPTHMAX")
ei.all <- whole("Data/NLA2007_residence.csv", "E_I")
rt.all <- whole("Data/NLA2007_residence.csv", "RT")

CA_LA.all <- merge(CA_all.all, LA.all, by = "ID")
CA_LA.all$avg <- CA_LA.all$avg_all.x/CA_LA.all$avg_all.y
CA_LA.all$avg_all <- CA_LA.all$avg
CA_LA.all$type <- CA_LA.all$type.y

# Temp file at non-paired sites
temp <- read.csv("Data/NLA2007_temp.csv") #temp data
res.rm.temp<- subset(temp, SITE_ID %in% res.id.rm)
lakes.rm.temp <- subset(temp, SITE_ID %in% lakes.id.rm)

# Temp at bottom
# there are multiple records of temp for some sites
# so, taking average of multiple readings
res.rm.tempmean.bot <- ddply(res.rm.temp, .(SITE_ID), summarize,
                          avg = min(TEMP_FIELD))
res.rm.tempmean.bot$type <- rep("reservoir")
lakes.rm.tempmean.bot <- ddply(lakes.rm.temp, .(SITE_ID), summarize,
                            avg = min(TEMP_FIELD))
lakes.rm.tempmean.bot$type <- rep("lakes")
# combine temp datasets into one long dataframe
tdatab.all <- rbind(res.rm.tempmean.bot, lakes.rm.tempmean.bot)
# Combine all info into one dataframe
tdatab.all$avg_all <- tdatab.all$avg
tdatab.all$analyte <- paste("temp_bottom")
tdatab$analyte <- paste("temp_bottom")

# Temp change
res.rm.tempmean.s <- ddply(res.rm.temp, .(SITE_ID), summarize,
                        templow = min(TEMP_FIELD),
                        temphigh = max(TEMP_FIELD),
                        depth.m = max(DEPTH))
res.rm.tempmean.s$type <- rep("reservoir")
lakes.rm.tempmean.s <- ddply(lakes.rm.temp, .(SITE_ID), summarize,
                          templow = min(TEMP_FIELD),
                          temphigh = max(TEMP_FIELD),
                          depth.m = max(DEPTH))
lakes.rm.tempmean.s$type <- rep("lakes")
# combine temp datasets into one long dataframe
strat.all <- rbind(res.rm.tempmean.s, lakes.rm.tempmean.s)
strat.all$delTemp_Depth <- (strat.all$temphigh - strat.all$templow)/strat.all$depth.m
strat.all$avg <- strat.all$delTemp_Depth
strat$avg <- strat$delTemp_Depth

# Temp at surface
res.rm.tempmean.sfc <- ddply(res.rm.temp, .(SITE_ID), summarize,
                             avg = max(TEMP_FIELD))
res.rm.tempmean.sfc$type <- rep("reservoir")
lakes.rm.tempmean.sfc <- ddply(lakes.rm.temp, .(SITE_ID), summarize,
                               avg = max(TEMP_FIELD))
lakes.rm.tempmean.sfc$type <- rep("lakes")
# combine temp datasets into one long dataframe
temp_1m.all <- rbind(res.rm.tempmean.sfc, lakes.rm.tempmean.sfc)
# Combine all info into one dataframe
temp_1m.all$avg_all <- temp_1m.all$avg
temp_1m.all$analyte <- paste("TEMP_FIELD")
temp_1m.all$ID <- paste(temp_1m.all$SITE_ID, temp_1m.all$type, sep="_")
