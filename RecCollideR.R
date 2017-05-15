# R/tidyverse Script
# Author: leerssej
# Date: Updated 28 Nov 2016
# Desc: Peel out record vector (single column) 
# Desc: Slam clones into 'themselves'
# Desc: Determine which records stuck to one another
# Desc: Create Sim(ilarity)Map
# Desc: Tie data back together - effectively condensing in/on/with itself 

options(stringsAsFactors = FALSE)
library(magrittr) 
library(tidyverse)
library(stringdist)
library(RecordLinkage)

###### 102. Remove OverlappingConfounders and prep for final serialized sorting ######
# reload and fuse pSimCity(ProperCased Similar City) names into rclCity(reclusterCity)
# grab the data
VgrsREM2clusterDathRvd <- read.csv("VgrsREM2clusterDathRvd.csv", stringsAsFactors = F, na.strings = c("", " ", "NA"))
glimpse(VgrsREM2clusterDathRvd)

# Making a map of the Old City Names to the fused names
## Yank out the Countries and make sure they match - if not Go back into the .csv and make them 
## or tear them out of the records - Bad google.
## Also make sure that you have propered out the simCity names if you haven't done so already
stdCntryClctn <- VgrsREM2clusterDathRvd %>% select(rctGmCountry, stdCountry) %>% distinct %>% filter(stdCountry != rctGmCountry | is.na(stdCountry))
glimpse(stdCntryClctn)
### Result should be zero

## yank out just the pSimCity Values and then distinct them
pSimCityClctn <- VgrsREM2clusterDathRvd %>% select(pSimcity, stdCountry) %>% distinct # %>% slice(1:20) # just for dev work
glimpse(pSimCityClctn)

## Create vectors 1 and 2 
pSimCity1 <- pSimCityClctn %>% data.frame
pSimCity2 <- pSimCityClctn %>% data.frame

#crash them all into one another
### allowing only the country blocks to collide with one another
pSimStrDist <- compare.linkage(pSimCity1, pSimCity2, blockfld = 2, strcmp = T)
# pSimStrDist

# Unscramble the crash results
pSimStrCompResults <- pSimStrDist$pairs %>% select(id1, id2, rclCitySim = pSimcity)
glimpse(pSimStrCompResults)
# Get back a key (between the rownames and the Cities)
pSimStrCompList <- pSimStrDist$data1 %>% mutate(id1 = as.integer(rownames(.))) %>% select(id1, rclCity = pSimcity)
glimpse(pSimStrCompList)
# Tie the pieces all back together
rclCityResultsIDed <- pSimStrCompList %>% left_join(pSimStrCompResults) %>%
    # Then move all the keys over and get the right half city names decoded
    rename(id = id1, id1 = id2) %>%
    left_join(pSimStrCompList, by = "id1") %>% 
    # Drop keys and reorder to just get names and simValues
    select(pSimCity = rclCity.x, trgCity = rclCity.y , rclCitySim) %>% 
    filter(rclCitySim >= 0.9)
glimpse(rclCityResultsIDed)
write.csv(rclCityResultsIDed, "rclCityREM2ResultsIDed.csv", na = "", row.names = F)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# results reviewed and file name edited to include "mapRclCityREM2.csv"
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# account confounders identify:
    # group_by Accts
    # rclCity Counts
    # tie the Counts back to the data and sort up
    # Review and flag offenders
