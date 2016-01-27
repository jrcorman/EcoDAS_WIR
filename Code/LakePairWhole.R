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
