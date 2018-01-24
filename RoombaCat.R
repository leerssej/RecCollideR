# R Script - dplyr predominant
# Author: leerssej
# Date;  Tue Jan 23 20:51:13 2018 
 
# Desc: Collide two vectors into one another
# Desc: Keep anything better than 95% similar

library(tidyverse)
library(magrittr)
library(googlesheets)
library(readxl)
library(RecordLinkage)

###### 0. Variable Library ######
path = "../../org_data/"

###### 1. Function Library #######
properCase <- function(x) {
    s <- strsplit(tolower(x), " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

###### 2. Trim down to equal widths & run thru rec_collider ######
# load reference group
load(paste0(path, "org_dist_clean"))
org_dist_trimmed <- 
    org_dist_clean %>%
    select(org_dist_name, org_st)
glimpse(org_dist_trimmed)

# load to match group
load(paste0(path, "clnt_dist_clean"))
clnt_dist_trimmed <-     
    clnt_dist_clean %>%
    select(clnt_dist_name = `District Name`,
           clnt_st = State)
glimpse(clnt_dist_trimmed)

# # test subset
# ## tc
clnt_clctn <- clnt_dist_trimmed # %>% slice(1:10) # uncomment for dev work
glimpse(clnt_clctn)
# ## bb
org_clctn <- org_dist_trimmed # %>% arrange(org_st) %>% slice(1:10) # uncomment for dev work
glimpse(org_clctn)

blockfld = 2 
rec_collider <- function(org_clctn, clnt_clctn, blockfld) {
    #crash them all into one another
    ### allowing only the district blocks to collide with one another
    collision <- compare.linkage(clnt_clctn, org_clctn, blockfld = 2 , strcmp = T)
    collision
    # Unscramble the crash results
    collision_results <- collision$pairs %>% select_(1, 2, ling_dist = 3)
    glimpse(collision_results)
    # Get back a key (between the rownames and the schoolnames)
    ## tc
    train1_docket <- collision$data1 %>% mutate(id1 = as.integer(rownames(.)))# %>% select(id1, clnt_schl_name)
    glimpse(train1_docket)
    ## bb
    train2_docket <- collision$data2 %>% mutate(id2 = as.integer(rownames(.)))# %>% select(id2, org_schl_name)
    glimpse(train2_docket)
    # Tie the pieces all back together
    collision_successes <- train1_docket %>% 
        left_join(collision_results) %>%
        # Then move all the keys over and get the right half schoolnames decoded
        left_join(train2_docket) %>% 
        # Drop keys and reorder to just get names and simValues
        select(-starts_with("id")) %>% 
        filter(ling_dist >= 0.95)
}

# For best of scenarios - when the joins are dirtier and we want a group - then even select the best of the groups
#     left_join(dat_org_nlprpd, by = c("org_schl_name" = "clnSchlName", "org_schl_city" = "city", "org_schl_state" = "ST")) %>% 
#     group_by(clnt_schl_name, clnt_city, clnt_ST) %>% 
#     mutate(rank = rank(desc(pSchlName))) %>% 
#     arrange(clnt_ST, clnt_city, school_name, rank) %>% 
#     select(org_schl_name, everything()) %>% 
#     mutate(best = min(rank)) %>% 
#     filter(best == rank)
# collision_successes

collision_successes_clnt_dist <- rec_collider(clnt_clctn, org_clctn, 2)
glimpse(collision_successes_clnt_dist)
# save data, and write into a .csv for later restart
save(collision_successes_clnt_dist, file = paste0(path, "collision_successes_clnt_dist"))
write_csv(collision_successes_clnt_dist, paste0(path, "collision_successes_clnt_dist.csv"), na = "")

###### 3. Fold District Matched Names back in and pick up the city name with it. ######
# load(paste0(path, "collision_successes_clnt_dist"))
# glimpse(collision_successes_clnt_dist)

# Load the long data
load(paste0(path, "clnt_dist_clean"))
load(paste0(path, "org_dist_clean"))
glimpse(clnt_dist_clean)
glimpse(org_dist_clean)

# join the data back up to the full list
Comparison_List_clnt_recommendations <- 
    clnt_dist_clean %>% 
    rename(clnt_dist_name = `District Name`, schl_state = State) %>% 
    left_join(collision_successes_clnt_dist) %>% 
    left_join(org_dist_clean) %>% 
    distinct %>% 
    mutate_all(funs(replace(., is.na(.), "")))
glimpse(Comparison_List_clnt_recommendations)

# throw data back up into gsheets and begin process of weeding
particip_schools <- gs_title("org_Schools")
gs_ws_new(particip_schools, ws_title = "Comparison_List_clnt_recs", input = Comparison_List_clnt_recommendations, trim = T)
gs_browse(particip_schools)