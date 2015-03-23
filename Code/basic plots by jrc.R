# NLA lake versus reservoir

# Grab data from this site, http://water.epa.gov/type/lakes/NLA_data.cfm
# Need to write code to download actual files

# Old code, to be removed
# setwd("C:\\Users\\Jess\\Documents\\NLA")

# lake <- read.csv("NLA2007_lakes.csv") # Sampled lake info
# chem <- read.csv("NLA2007_chem.csv") # Chemical Condition Estimates
# temp <- read.csv("NLA2007_temp.csv") # Profile data
# temp5 <- subset(temp, DEPTH == 5)

cdata <- merge(lake, chem, by="SITE_ID")
ddata <- merge(cdata, temp5, by="SITE_ID")

library(ggplot2)

ggplot(ddata, aes(x=EPA_REG.x, y=TEMP_FIELD)) +
  geom_point(size=3, alpha=0.5, aes(color=LAKE_ORIGIN.x, shape=RT_NLA.x), 
             position=position_dodge(0.5)) +
  theme_classic(12) +
  ylab("Temperature @ 5 m")

ggplot(ddata, aes(x=log(LAKEAREA), y=TEMP_FIELD)) +
  geom_point(size=3, alpha=0.5, aes(color=LAKE_ORIGIN.x, shape=RT_NLA.x), 
             position=position_dodge(0.5)) +
  theme_classic(12) +
  ylab("Temperature @ 5 m") +
  xlab("LN(Lake Area)") +
  stat_smooth(data=subset(ddata, LAKE_ORIGIN.x == "MAN-MADE"), 
              color="red", method="lm") +
  stat_smooth(data=subset(ddata, LAKE_ORIGIN.x == "NATURAL"), 
              color="blue", method="lm")