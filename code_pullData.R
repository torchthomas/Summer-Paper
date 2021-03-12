# SCRIPT TO PULL DATA FOR SUMMER PAPER 2020
# AUTHOR: THOMAS DEVINE
# currdir: "C:/Users/tdevine/Desktop/2020 Summer Paper/Analysis"
# CREATED: (when I made my code neat) 8.20.2020
#.............
    #Comments:     This doesn't deal with CSI data, only Demographic data
{
    # (block-level) Get the dec2010 and acs2011 data similar to above
    dec2010 <- read_decennial(
        year = 2010,
        state = "CA",
        geo_headers = c("ZCTA5","COUNTY","BLKGRP", "TRACT","BLOCK"),
        summary_level = "block", 
        table_contents = dec.tables
    ) %>%
        .[, geoid_blkgrp := stringr::str_sub(GEOID, 6, 19)]%>% # shortening geoid to that of block groups 
        rename(population_dec = population) %>%                # rename the population variable since we duplicate the name with acs2011
        filter(population_dec > 0) %>%                         # drop blocks with no people; dim: 403,398 x 19
        as.data.table(); dim(dec2010)     
    # (block group-level) Get acs2011 demog stats at the block group-level with ALL tables
    acs2011 <- read_acs5year(
        year = 2011,
        states = "CA",
        geo_headers = c("COUNTY"),
        summary_level = "block group", 
        table_contents = acs.tables
    ) %>%.[, geoid_blkgrp := str_sub(GEOID, 6, 19)] %>% 
        arrange(geoid_blkgrp) %>% 
        dplyr::select(-lat,-lon) %>%
        filter(.,population > 0, pop > 0, population>0 & pop>0) %>%                  # drop blocks with no people; dim: 23,139 x 145
        as.data.table(); dim(acs2011)
    
    # Before continuing, we must make sure there are no block groups in dec2010 that are not in acs2011
    dec2010 = dec2010[which(dec2010$geoid_blkgrp %in% unique(acs2011$geoid_blkgrp)),]; #drop uncommon ones
    acs2011 = acs2011[which(acs2011$geoid_blkgrp %in% unique(dec2010$geoid_blkgrp)),]; #in both

}