# AUTHOR: THOMAS DEVINE; CMU 2nd YEAR PHD STUDENT FOR FIRST SUMMER PAPER
    # COMMENTS:
{
    #1. PREVIOUS CODE IS FOUND C:\Users\tdevine\Box Sync\research\solar\Data_California\Census\5year\totalcensuspackagetest, BEFORE I WENT ON VACATION
        #  other store variants are of the current label (code_totalcensus)
        #  corresponding code for the interconnection applications is under: "Summer_Paper_Code.R"
    #2. A lot of commented-out code from trial and error is last seen in code_totalcensus_8-14_copy.R in the deleted section at the very end, comprising 800 lines or so
    #3. two different apportionment procedures were tried, oldest one (which we will be updating) is in code_totalcensus_8-15_copy. 
        #   - the second procedure to apportion allocations from block groups to zctas is saved in code_totalcensus_8-18_copy.R, which we only sample from the update new parts not 
        #     the count-making portion of the code
    #4. KEY IDEA (in CALIFORNIA):
        # counties:         58
        # tracts:           8,057
        # blockgroups:      23,212
        # blocks:           710,145
        # ZCTA5:            1,770
        # blockgroup,ZCTA5: 29,372, mutually shared block groups-zcta5 combos: 28,396
# DIRECTORIES USED:
    # DATA:
    #   CENUS DATA: "C:/Users/tdevine/Desktop/2020 Summer Paper" 
    #       DOWNLOAD:
                # download_census("acs5, states = c("CA"))        # about 1 hour
                # download_census("acs5", 2015, states = c("CA")) # about 1 hour
                # download_census("dec", 2010, states = c("CA"))  # about 1.5 hours; start 7:53
    #       HAS: downloaded census data (big files)
    #   CSI INSTALLATION DATA: "/Users/tdevine/Box Sync/research/solar/Data_California/NEM_InterconnectionApplicationsDataset_2020-04-30.csv"
    #       HAS:  data from CSI, version-date is in filename, 1.4m rows
    # CODE:  "C:/Users/tdevine/Box Sync/research/solar/Analysis/" 
    #       has: various scripts, copies, notes)
    # OUTPUT: "C:/Users/tdevine/Box Sync/research/solar/Analysis/tables"
    #       has: latex tables: 
# NOTES: "C:\Users\tdevine\Box Sync\research\solar\Analysis\Notes for code (not deleted).txt\"
    #    CONTAINS: 1. DOCUMENTATION FROM GILLINGHAM: VARIABLES USED IN STATA CODE TO REPLICATE TABLES
    #              2. NOTES ON WHICH TABLES I PULLED AND THEIR CORRECT NAMES
    # DELETED CODE: 
    #   Most deleted code is in each copy, I try to keep the main file (code_totalcensus.R) free of clutter for readable runs
    #   this naming scheme may change, and likely will be "code_main.R" by the end of the summer
}
#edit 11/19/2020, removed code for graphics because I'm sending data to DTA for stata
#edit 11/23/2020, stata writing install.data (shortpanel)
#edit 11/25/2020, stata writing csi.long (longpanel )


#..................................................................................................
#Clear stuff first; set working directory
rm(list=ls()); gc(); options(digits = 22)
options(scipen=999) # turn off scientific notation
setwd("C:/Users/tdevine/Desktop/2020 Summer Paper")

# Make functions 
source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_makefunctions.R")

# survival: https://cran.r-project.org/web/views/Survival.html
# Load packages:DATA          |DATA MANIPULATION                                     
needPacks <- c( "totalcensus", "dplyr","tidyverse","data.table","xtable","lubridate","zoo", "iterators",
                #|HAZARD RATES                     
                "survival", "survminer", "muhaz", "condSURV", "survey", "coxphw",
                #|other
                "anytime","xts","vars", "ranger", "foreign",
                #|graphing
                "ggplot2","extrafont","ggfortify")
readInPackages(needPacks)

# Data DOWNLOAD and Call; THIS IS DONE ONLY ONCE,see commands above in DIRECTORIES USED > DOWNLOAD; 1. set path, 2. answer prompt
set_path_to_census("C:/Users/tdevine/Desktop/2020 Summer Paper");
2; 
    
# Define tables to pull from ACS5yr2011 and Dec2010 (define elsewhere because they're massive vectors readably written)
source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_tablesToPull.R")

# Create the ZCTA-level data we seek to match BG2012
    library("magrittr")
    # Pull Data; which creates:
        #  VARIABLES          | (DIMENSIONS)   | (AGG.-LEVEL)      | (DESCRIPTION)
        #   dec2010:          | 403,320 x 22   | block-level       | For making weights (doesn't have all the tables we need from dec2010, see dec.blkgrp_zcta5 for complete tables)
        #   acs2011:          | 23,139  x 145  | block group-level | for create zcta5-level aggreg. data (has all tables we need from acs2011)
    source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_pullData.R")
    # Wrangle the data; which creates:
        #(VARIABLES)            | (DIMENSIONS)   | (AGG.-LEVEL)          | (DESCRIPTION)
        #del   w.blkgrp_zcta5      | 28,396  x 3    | block group-level     | weights, used to apportion acs2011 & dec2010 data 
        #del   acs.blkgrp_zcta5    | 28,396  x 142  | block group-zcta-level| part of creating zcta5-level aggreg. data
        #del   dec.blkgrp_zcta5    | 28,396  x 9    | block group-zcta-level| part of creating zcta5-level aggreg. data (has all tables we need from dec2010)
        #del   joined              | 28,396  x 147  | block group-zcta-level| join dec.blkgrp_zcta5 & acs.blkgrp_zcta5 by geoid & zcta5
        #del   counts.zcta5        | 1,760   x 136  | zcta-level            | aggregated count statistics, step before final zcta5-level
        #del   medians.zcta5       | 1,760   x 11   | zcta-level            | aggregated count statistics, step before final zcta5-level
        #   demog.old           | 1,760   x 146  | zcta-level            | join the counts and medians
        #   demog               | 1,759   x 74   | zcta-level            | Rename, recalc., & redef. vars to match BG2012, drop zcta5: 99999
    source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_dataWrangle.R")
    # COMMENT: we make further adjustments to this data later on (trying to drop cols to retain the most complete.cases)

# FILTER INSTALLATION DATA FROM CSI, MY FILE IS  NEM_CurrentlyInterconnectedDataset_2020-04-30 
        #(VARIABLES)            | (DIMENSIONS)   | (AGG.-LEVEL)          | (DESCRIPTION)
        #del   csi.dat             |1018918x 67     | adopter-level         | filter CSI data set see steps below
        #   csi                 | 70091 x 67     | adopter-level         | filter CSI data set see steps below
        #   demog               |   951 x 74     |   zcta5-level         | filtered to have only ZCTA5 in CSI data  
    source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_filterCSI.R")


####################.ANALYSIS SECTION.#########################
    
# TABLE CREATION (TABLE 1)
    #    Variables        |  Description
    #del   tab1              | df for table 1 output 
    #   xtab1             | Latex code for table 1 output 
    setwd("C:/Users/tdevine/Box Sync/research/solar/Analysis/")
    suppressWarnings(source("code_maketables.R")) # we suppress warnings based on deleting variables that don't exist, I keep them there in case I file their place (lines in a table)

    setwd("C:/Users/tdevine/Box Sync/research/solar/Analysis/")
    
# CREATE A PANEL (it's in wide format at the moment)
    # use person-period observations from CSI
    # append demographic statistics from demog    (constant variables)
    # create new varaibles that changes from person-period observations counting n adopters per zip-day (by app.complete.date)
    #create panel (long and short)
    #save short panel (install.data) to .dta for stata
source("code_makePanel.R") #source("code_hazard.R") #create data for hazard,
    
    
    #WRITE TO DTA FILE
    "dimensions of short panel we print to stata";dim(install.data)
    foreign::write.dta(install.data,"c:/Users/tdevine/Box Sync/research/solar/Data_California/stata/installdata.dta")
    foreign::write.dta(csi.long,"c:/Users/tdevine/Box Sync/research/solar/Data_California/stata/csilong.dta")
    
    
#...................deleted/commented-out
    #see code_totalcensus_11-24_copy.R
    
    

    
