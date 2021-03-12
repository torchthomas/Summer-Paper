# SCRIPT TO FILTER CSI DATA FOR SUMMER PAPER 2020
# AUTHOR: THOMAS DEVINE
# currdir: "C:/Users/tdevine/Desktop/2020 Summer Paper/Analysis"
# CREATED: (when I made my code neat) 8.20.2020
#................................................................................
# STEP 1: read in csi data
# STEP 2: filter on residential customer.sector 
# STEP 3: filter between jan 01 2001 and dec 31 2011
#        - lower bound date is from App.Received.Date and the upperbound date is for App.Complete.Date
# STEP 4: grab zipcodes inCSI that are in demographic data and reverse 
# STEP 5: drop cols with ONLY na values.
#................................................................................
# CREATED: (when I made my code neat) 8.20.2020
# EDITED: -8/26/2020 I made it so:  12-31-2000 < app.complete <= DATES.KEPT =< app.recd < 01-01-2012
#                                   which is commented out (this is me adding the FIRST filtering process I discuss
#                                   in my summer paper)
#         -8/27/2020 I add some comments to the above edit
#edited 8-31-2020, moved self-installer mutating binary definition up to check obns counts (ctrl+f "self")
#edited 10/6/2020, at the very end of code, instead of drop cols w/ all NAs we drop 48-end
#edited 10/8/2020, at the very end I add a section to get the cost of PV systems in my end data
#edited 11/1/2020, get step and incentive information from working dataset, join this with csi.data (NEM_InterconnectionApplicationsDataset_2020)
        #see old copy of previous code in backups code_filterCSI_11-1-2020.r
    #this forces me to have less observations, also forced to use a recent copy of working dataset
        # instead of an older version (ideally, something from 2012 would be great) but idk
        # where to get the decommissioned systems(although decom. data is in nems,
        # bg2012 dont say if that matters)

## FILTER INSTALLATION DATA FROM CSI, MY FILE IS  NEM_CurrentlyInterconnectedDataset_2020-04-30 
    #READ IN DATA: FROM THE NEM CURRENTLY CONNECTED DATASET
    csi.dat <- data.table::fread("/Users/tdevine/Box Sync/research/solar/Data_California/NEM_InterconnectionApplicationsDataset_2020-04-30.csv",
                                 na.strings=c("","NA"),
                                 check.names = T); dim(csi.dat) # dim: 1,046,842 x 101
    csi.untouched <- csi.dat
    #FILTER CSI (nem-interconnections) DATA:
    # Customer sector filter: residential only dim: 1,018,918 x 101
    csi.dat %<>% filter(Customer.Sector == "Residential") %>%
        mutate(Self.Installer = ifelse(tolower(Self.Installer)=="yes",1,0)); dim(csi.dat) #dim: 1,018,918 x 101
        # Make dates
        csi.dat[,18:20] <- lapply(csi.dat[,18:20],lubridate::mdy);dim(csi.dat) #.6 sec
        
       # THIS IS THE PLACE FOR THE FIRST AND SECOND FILTERING PROCESS 
        #Time-period filter; we make the date based off of... App.Complete.Date  OR  App.Received.Date  \st{OR  App.Approved.Date}
        csi = csi.dat%>% # dim: 70,218 x 101 for app.rec > 2000 & 2012< app.com
            # #FIRST FILTERING PROCESS
            # filter(App.Complete.Date > as.Date("12-31-2000","%m-%d-%Y") & App.Received.Date < as.Date("01-01-2012","%m-%d-%Y")); dim(csi)
            # #SECOND FILTERING PROCESS
            # filter(App.Received.Date > as.Date("12-31-2000","%m-%d-%Y") & 
            #        App.Complete.Date < as.Date("01-01-2012","%m-%d-%Y")); dim(csi)
        
          #third FILTERING PROCESS
                #Self-installer is included in the <| is.na(csi.dat$App.Complete.Date)> code 3-4 lines below 
            filter(App.Received.Date >= as.Date("01-01-2001","%m-%d-%Y") & 
                   (
                   # (App.Complete.Date <= as.Date("12-31-2011","%m-%d-%Y")  )& 
                   (App.Complete.Date <= as.Date("12-31-2011","%m-%d-%Y") | is.na(csi.dat$App.Complete.Date) )&
                       App.Received.Date <= as.Date("12-31-2011","%m-%d-%Y")
                    ) ); dim(csi) #after  adding <| is.na(csi.dat$App.Complete.Date)> above , we go from 69,699 to 79561
        
        #filter csi data which isn't in the demographic data
        csi %<>% .[.$Service.Zip %in% unique(demog$ZCTA5),] %>% #dim: 70,088 x 101
            mutate(System.Size.DC=as.numeric(System.Size.DC),
                   System.Size.AC=as.numeric(System.Size.AC)); dim(csi)
               
        demog  = demog[demog$ZCTA5 %in% unique(csi$Service.Zip),]; dim(demog)   #dim: 951 x 74
        
        csi  %<>%.[.$Service.Zip %in% unique(demog$ZCTA5),]; dim(csi)  # dim: 100,970 x 101    
        
    # Drop columns after 48 (model1.quantity), we wanted to drop all cols of NAs, but too troublesome   
        csi <- csi[,1:48]; dim(csi) # dim: 94,474 x 48
        
        
    ##11/1/2020
        # #READ IN DATA: FROM WorkingDataSet_5-28-2020
        # csi.working.dat <- data.table::fread("/Users/tdevine/Box Sync/research/solar/Data_California/WorkingDataSet_5-28-2020.csv",
        #                                      na.strings=c("","NA"),
        #                                      check.names = T) %>%
        #     #RENAME the var for application.number to Matched.CSI.Application.Number
        #     rename(.,Matched.CSI.Application.Number=Application.Number); dim(csi.working.dat) # 174344 x 124; head(names(csi.working.dat))
        # 
        # #JOINED CSI DATA SETS (NEM-INTER. AND WORKING_DAT)   
        # #MATCH DATA SETS: csi.dat with csi.working.dat, joinING BY: csi.dat$Matched.CSI.Application.Number & csi.working.dat$Application.Number 
        # #FILTER: CSI.WORKING.DAT for substring in COLNAMES c("incentive","date", "rating","factor", "program")
        # s <- paste0("c(", paste0(sapply(iter(c("application","incentive","date", "rating","factor", "program")),
        #                                 function(x) paste0("contains(\"", x, "\")")), collapse=","), ")")
        # csi.working.dat.dates.incentives.etc = csi.working.dat %>% 
        #     select_(., s); dim(csi.working.dat.dates.incentives.etc);#str(csi.working.dat.dates.incentives.etc) #dim: 174344 x  30
        # 
        # #JOINED
        # csi.b4.joined.appnumbers <-csi ; dim(csi.b4.joined.appnumbers)      #dim: 94474 x 48
        # csi.joined <- plyr::join(csi, csi.working.dat.dates.incentives.etc,
        #                          type = "inner");dim(csi)                  #dim: 56267 x 91
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
# #................... C O S T   O F    S O L A R    P V
#     csi.cost = csi.dat %>%
#         filter(App.Received.Date >= as.Date("01-01-2001","%m-%d-%Y") &
#                    (
#                        # (App.Complete.Date <= as.Date("12-31-2011","%m-%d-%Y")  )&
#                        (App.Complete.Date <= as.Date("12-31-2011","%m-%d-%Y") | is.na(csi.dat$App.Complete.Date) )&
#                            App.Received.Date <= as.Date("12-31-2011","%m-%d-%Y")
#                    ) ); dim(csi.cost)
#         # csi.cost$Total.System.Cost
        
        
### from edits on 11/1/2020, meant to check counts and see what changes will llook like roughly
    # #getting the num of shared applications 
    #     # sum(csi.dat$Application.Id %in%csi.working.dat$Application.Number)    
    #     # sum(csi.dat$Preceding.Id %in%csi.working.dat$Application.Number)    
    #     sum(csi.dat$Matched.CSI.Application.Number %in%csi.working.dat$Application.Number)# 123k of 1m (matched in csi.dat) of/from 174k csi.working.dat
    #     # sum(csi.dat$Superceding.Id %in%csi.working.dat$Application.Number)
    #     sum(csi$Matched.CSI.Application.Number %in%
    #             csi.working.dat$Application.Number)  # 56k of 94k in csi.dat in csi.working.dat
    #         #check if there are common var names
    #             csi.dat.names=names(csi.dat);                         #length: 101
    #             csi.working.dat.names = names(csi.working.dat)        #length: 124
    #             joined.csi.dat.names.csi.working.dat.names = c(csi.dat.names,csi.working.dat.names); 
    #                 length(joined.csi.dat.names.csi.working.dat.names)#length: 225