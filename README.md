# Summer-Paper
Equivalent to my Master's Thesis. My overleaf link in my Projects section of http://thdevine.github.io/ has a much nicer and more concise portion of code, but this contains a lot more notes, highlights what I needed to delete, discussed variables I excluded from the census, and more. more. 

# The code below is an abbreviated version of code_totalcensus which highlights the order of the other scripts which care source(.)d:
#Clear stuff first; set working directory
rm(list=ls()); gc(); options(digits = 22)
options(scipen=999) # turn off scientific notation
setwd("C:/Users/tdevine/Desktop/2020 Summer Paper")

#Make functions 
source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_makefunctions.R")

#survival: https://cran.r-project.org/web/views/Survival.html
#Load packages:DATA          |DATA MANIPULATION                                     
needPacks <- c( "totalcensus", "dplyr","tidyverse","data.table","xtable","lubridate","zoo", "iterators",
                #|HAZARD RATES                     
                "survival", "survminer", "muhaz", "condSURV", "survey", "coxphw",
                #|other
                "anytime","xts","vars", "ranger", "foreign",
                #|graphing
                "ggplot2","extrafont","ggfortify")
readInPackages(needPacks)

#Data DOWNLOAD and Call; THIS IS DONE ONLY ONCE,see commands above in DIRECTORIES USED > DOWNLOAD; 1. set path, 2. answer prompt
set_path_to_census("C:/Users/tdevine/Desktop/2020 Summer Paper");
2; 
    
#Define tables to pull from ACS5yr2011 and Dec2010 (define elsewhere because they're massive vectors readably written)
source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_tablesToPull.R")

#Create the ZCTA-level data we seek to match BG2012
    library("magrittr")
    # Pull Data; which creates:
    source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_pullData.R")
    # Wrangle the data; which creates:
    source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_dataWrangle.R")
    # COMMENT: we make further adjustments to this data later on (trying to drop cols to retain the most complete.cases)

#FILTER INSTALLATION DATA FROM CSI, MY FILE IS  NEM_CurrentlyInterconnectedDataset_2020-04-30 
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
    
#CREATE A PANEL (it's in wide format at the moment)
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
    
    
