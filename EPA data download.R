# How to download data from EPA website
# This only needs to be done to bring in new datasheets, as
# data is stored locally in the /data file.

# Step 1. Visit website to determine which file(s) you want to download
# http://water.epa.gov/type/lakes/NLA_data.cfm

# Step 2. Copy url of file below. Replace current one listed.

url <- "http://water.epa.gov/type/lakes/assessmonitor/lakessurvey/upload/NLA2007_Basin_Landuse_Metrics_20061022.csv"

newdata <- read.csv(url)

# Step 3. Save data in Data file if needing it in the future

write.csv(newdata, "Data/NLA2007_CHANGETHISNAME.csv")