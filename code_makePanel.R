# SCRIPT TO CREATE FUNCTIONS FOR SUMMER PAPER 2020
# AUTHOR: THOMAS DEVINE
# currdir: "C:/Users/tdevine/Desktop/2020 Summer Paper/Analysis"
#CREATED: (when I made my code neat) 8.24.2020
#EDITED: 8/25/2020
#EDITED: 8/27/2020, add n.zip.day by received date in df.haz for counts
#EDITED: 10/5/2020, added "Total.System.Cost", "Itc.Cost.Basis", "Cost.Watt"to cols2keep
#EDITED: 11/1/2020, added added lastprice, prob, dow (day of week), dom (day of month),step, incentives
    #see "11/1/2020"
#EDITED 11/6/2020, renamed panel.receivedDate,m2 so it's of format panel.DATEVARIABLE
#EDITED 11/8/2020, added the part from code_smallerGraphics that actually organizes the panel I need to xfer to stata
                #  fixed counts: n.zcta5.day= sum n.zcta5.day[t-1 @ t]
                # remove df.haz code, see commented out stuff under "11/8/2020"
#EDITED 11/11/2020, see 11/25/2020 MAJOR FIX wrt changes of install.data&csi.long, fixed NA rows (no ID), 
#EDITED 11/12/2020, see 11/25/2020 MAJOR FIX wrt changes of install.data&csi.long, verify NEM and WD raw data join (128k obsn.), then filter, 
            #       compare other date filters from WD, I get more observations than just app.received.date and app.complete.date, 
#edited 11/18/2020see 11/25/2020 MAJOR FIX wrt changes of install.data&csi.long, there's an issue with the cumsum and counts, I need to do a group by day,zipcode, bc I can't accurately get the appropriate counts
            #   1. removed cumsum defn from install.data, will put it later once I get rows for zip_days only (removing ID vars)
                    # this might seem controversial, but since we're at zip-lvl, I only need this group_by(ZCTA5, App.Complete.Date)
            #   2. remove csi.long for csi.long (change this everywhere)
            #   3. this means replacing app.complete.date with First.Pending.Payment.Date for csi.long
            #   4. remove first.entrant.date because I'll essentially get that later on with last.noncontemp.adopter.zcta5 or whatever var that is
            #   5. to address cumsum issue, make a new var csi.zcta5.date and make it like I need where I get 1 row per zip.day
                    # add last adopter var for install.data, issue with first installation remains
#edited 11/23/2020, see 11/25/2020 MAJOR FIX wrt changes of install.data&csi.long, seems to be an issue with the filler variable and I'm not quite sure why, I see the 
    #error is number of items to replace is not a multiple of replacement length"
#edited 11/24/2020, see 11/25/2020 MAJOR FIX wrt changes of install.data&csi.long, see 11/25/2020 MAJOR FIX wrt changes of install.data&csi.long, need variable Y_zt, Yzt is the fraction of owner-occupied households in z that had not 
    # previously adopted solar panels and decide to adopt on day t;
        # adding: install base by system.size.ac (not located in the short panel definition for 11/24/2020), added into csi.long and install.data (long and short panel, respectively)
        #         
#edited 11/25/2020, save copy, delete commented-out stuff
        # FIX:
        #       take last entry per zcta5-day for install.data (short panel)
        # ADD:
        #       quarter in short panel install.data (ymd)
        # MAJOR FIX:
        #       make csi.long (long panel) contain most definitions for install.data (short panel), so we only take slice(n()) in install.data
#edited 11/29/2020: 
        # REMOVE
        #       columns with "inverter"in them
        #       female_dec,male_dec
        #       rowNum (in csi.long's last defn)
        # ADD/redefine: 
        #       first.entrant.date back in for survival analysis because I messed up calculating probabilities
        #       timefirstInstall.zcta5, like timelastinstall.zcta5, we get the survival since first exposure
        #       status = 1 if adopt (all have it given tracked data)
        #       csi.old, remove cols with "axis"in them
        #       dow, day of week from First.Pending.Payment.Date
        #    graphs
        #       survival stuff at end
        # FIX: 
        #       last.noncontemp.adopter.zcta5, must be computed using difftime() see www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html (sect "Calculating survival times - base R")
        #       remove system.size.ac because footnote 16 in bg2012 mentions direct current, and we 
        #           use nameplate.rating in lieu bc it makes a bit more sense
#edited 12/18/2020
    # REDeFINE / CHANGE
    #       prob

#-----------------------------------------
# The columns we care to keep (that I can see we may need to use)
    cols2keep = c(
        "Application.Id"     ,"Preceding.Id"      ,"Superceding.Id"   ,"Matched.CSI.Application.Number",  "Application.Status" ,"Utility"           ,"Service.City"     ,"Service.Zip",                  
        "Service.County"     ,"Technology.Type"   ,"System.Size.DC"   ,"System.Size.AC",      "Customer.Sector"    ,"App.Received.Date" ,"App.Complete.Date","App.Approved.Date",                
        "Decommissioned.Date","Self.Installer"    ,"Installer.Name"   ,"Installer.Phone",     "Installer.City"     ,"Installer.State"   ,"Installer.Zip"    ,"CSLB.Number",                      
        "Third.Party.Owned"  ,"Third.Party.Owned.Type" ,"Third.Party.Name" ,"Pace.Financed",  "Electric.Vehicle",   "NEM.Tariff"         ,"VNEM..NEM.V..NEM.Agg"   ,"Project.is.VNEM..NEM.V..NEM.Agg.",
        "Total.System.Cost"  ,"Itc.Cost.Basis"    ,"Cost.Watt","Mounting.Method"
    )
#save a copy of CSI #editted 11/29/2020
    csi.old=csi 
    
#select columns we want
    csi%<>%
        dplyr::select(.,cols2keep) %>%     #selects cols in cols2keep
        group_by(Application.Id)   %>%     #group by id
        arrange(App.Complete.Date) %>%     #arrange by complete date
        ungroup(); dim(csi) #dim: 94474 x 48
#11/1/2020: added added lastprice, prob, dow (day of week), dom (day of month)
        #day of week, 1 is sunday, 2 month, 3 tuesday... etc
          csi$dow.app <- wday(csi$App.Approved.Date);csi$dow.com=wday(csi$App.Complete.Date);csi$dow.rec=wday(csi$App.Received.Date)
        #month-year
          csi$ym.app=zoo::as.yearmon(csi$App.Approved.Date); csi$ym.com=zoo::as.yearmon(csi$App.Complete.Date); csi$ym.rec=zoo::as.yearmon(csi$App.Received.Date)
        #day of month
          csi$dom.app=mday(csi$App.Approved.Date);csi$dom.com=mday(csi$App.Complete.Date);csi$dom.rec=wday(csi$App.Received.Date)
        #READ IN DATA: FROM WorkingDataSet_5-28-2020
            csi.working.dat <- data.table::fread("/Users/tdevine/Box Sync/research/solar/Data_California/WorkingDataSet_5-28-2020.csv",
                                                 na.strings=c("","NA"),check.names = T) %>%
                #RENAME the var for application.number to Matched.CSI.Application.Number
                rename(.,Matched.CSI.Application.Number=Application.Number); dim(csi.working.dat) # 174344 x 124; head(names(csi.working.dat))
    #JOINED CSI DATA SETS (NEM-INTER. AND WORKING_DAT)   
            #MATCH DATA SETS: csi.dat with csi.working.dat, joinING BY: csi.dat$Matched.CSI.Application.Number & csi.working.dat$Application.Number 
            #FILTER: CSI.WORKING.DAT for substring in COLNAMES c("incentive","date", "rating","factor", "program")
            s <- paste0("c(", paste0(sapply(iter(c("application","incentive","date", "rating","factor", "program")),
                                            function(x) paste0("contains(\"", x, "\")")), collapse=","), ")")
            csi.working.dat.dates.incentives.etc = csi.working.dat %>% 
                select_(., s); dim(csi.working.dat.dates.incentives.etc);#str(csi.working.dat.dates.incentives.etc) #dim: 174344 x  30
        #JOINED
        csi.b4.joined.appnumbers <- csi ; dim(csi.b4.joined.appnumbers)      #dim: 94474 x 48
        csi <- plyr::join(csi, csi.working.dat.dates.incentives.etc,
                                     type = "inner");dim(csi)               #dim: 56267 x 88, (prev. was 56267 x 91)

#................................................................
#       THIS IS WHERE EVERYTHING CHANGED ON 11/18/2020, WHERE i MATCHED NEM CSI TO WD
#................................................................
        #Combine NEM and WD (mostly) RAW datasets (1.01m obsn. with .174m obsn.)
            matched.raw.NEM.WD <-plyr::join(csi.untouched, csi.working.dat.dates.incentives.etc,
                                           type = "inner") %>% #; dim(matched.raw.NEM.WD) # dim: 128563 x 144
                filter(Customer.Sector == "Residential") %>%
                mutate(Self.Installer = ifelse(tolower(Self.Installer)=="yes",1,0),
                       dow = wday(First.Pending.Payment.Date)); dim(matched.raw.NEM.WD) #dim: 123477 x 144
                # Make dates
                matched.raw.NEM.WD[,18:20] <- lapply(matched.raw.NEM.WD[,18:20],lubridate::mdy);dim(matched.raw.NEM.WD) #.6 sec

            #Date FILTER: First.Pending.Payment.Date is better than App.Received.Date and App.Complete.Date
                          # defn: First.Pending.Payment.Date:.....Date Incentive Claim Request has been approved as eligible for payment, and claim form forwarded to Program Administrator accounting department for payment. OR Date Incentive Claim Request has been approved, and a letter from the Program Administrator to appropriate parties requesting data from the Performance Data Provider has been sent.
            matched.raw.NEM.WD %<>%
                filter(First.Pending.Payment.Date >= as.Date("01-01-2001","%m-%d-%Y") &
                           ((First.Pending.Payment.Date <= as.Date("12-31-2011","%m-%d-%Y")  )
                           )) %>%                                                  #dim at this point 51944 x 144
                .[.$Service.Zip %in% unique(demog$ZCTA5),] %>%
                mutate(System.Size.DC=as.numeric(System.Size.DC),
                       System.Size.AC=as.numeric(System.Size.AC)); dim(matched.raw.NEM.WD)#dim: 51879 x 144 
        
        #edit: 11/29/2020
            #remove some cols (from dim=51879x144  to dim=51879x117)
            matched.raw.NEM.WD = matched.raw.NEM.WD[,!grepl(c("^Module.Manufacturer"),names(matched.raw.NEM.WD))]; dim(matched.raw.NEM.WD);  
            matched.raw.NEM.WD = matched.raw.NEM.WD[,!grepl(c("^Module.Model"),names(matched.raw.NEM.WD))]; dim(matched.raw.NEM.WD);  
            matched.raw.NEM.WD = matched.raw.NEM.WD[,!grepl(c("^MASH"),names(matched.raw.NEM.WD))]; dim(matched.raw.NEM.WD)
            matched.raw.NEM.WD = matched.raw.NEM.WD[,!grepl(c("^Inverter.Model"),names(matched.raw.NEM.WD))]; dim(matched.raw.NEM.WD);       
            matched.raw.NEM.WD = matched.raw.NEM.WD[,!grepl(c("^Inverter.Manufacturer"),names(matched.raw.NEM.WD))]; dim(matched.raw.NEM.WD);       
            matched.raw.NEM.WD = matched.raw.NEM.WD[,!grepl(c("^System.Output.Monitoring.Provide"),names(matched.raw.NEM.WD))]; dim(matched.raw.NEM.WD)
            matched.raw.NEM.WD = matched.raw.NEM.WD[,!grepl(c("*Axis"),names(matched.raw.NEM.WD))]; dim(matched.raw.NEM.WD)#dim: 51879 x 117; end edti
          #end
            
            
##REDEFINE CSI
csi.11_18_2020 = matched.raw.NEM.WD; dim(csi.11_18_2020)#dim: 51879x117

## MAKE LONG DATA FORMAT (STEPS); get sums, (removed for install.data definition) cumsums, by group, month, day, etc.
  ### MADE aug 25 2020, 8/25/2020, edited aug 26 to fix status definition if we have received date being the upper threshold for right censoring
    csi.long = csi.11_18_2020 %>%
    #REDEFINE service.zip as a character and rename it to ZCTA5
        mutate(Service.Zip = as.character(Service.Zip),
               status=1) %>%#added 11/29/2020
        rename(ZCTA5 = Service.Zip)  %>%
        #RIGHT JOIN the demographic data to application by adopter's ZCTA5
        right_join(., demog, by="ZCTA5") %>%
    #DEFINE first.entrant.date. Survival is based on this var (exposure)
        arrange(First.Pending.Payment.Date) %>%
            group_by(ZCTA5) %>%
                #first installers date isn't set as first.entrant date, so I have to fix that, survival is based on first adopter
                mutate(first.entrant.date = min(First.Pending.Payment.Date)) %>%
                   #the fix
                    mutate(first.entrant.date = ifelse(is.na(first.entrant.date),
                                                              as.Date(First.Pending.Payment.Date,format="%Y-%m-%d"), # if first entrant, then set first date as App.Complete.Date
                                                              as.Date(first.entrant.date,format="%Y-%m-%d"))) %>% #else, what it was (from first_entrant)
                mutate(first.entrant.date = as.Date(first.entrant.date,format="%Y-%m-%d")) %>%
        #         mutate(DateOfLastAdopter = lag(First.Pending.Payment.Date)) %>%
                ungroup() %>% 
        #REDEFINE 11/29/2020 electric.vehicle
            #11/29/2020
            mutate(Electric.Vehicle = tolower(Electric.Vehicle)) %>%
            mutate(Electric.Vehicle = ifelse(is.na(Electric.Vehicle),NA,ifelse(Electric.Vehicle=="yes",1,0))) %>%
                    # #DEFINE: everyone eventually adopts so set status, maybe a way to say it's censored
                    #     mutate(status = ifelse(App.Complete.Date <= as.Date(c("2011-12-31"), format = "%Y-%m-%d"),1,0 )) %>%
                    #        # mutate(status = 1) %>% #before edit above with right censoring for reeived application
                    # #DEFINE: var for time since last adoption, then adjust NA for first adopters to 0
                    #     mutate(time2adopt = as.numeric(App.Complete.Date - first.entrant.date) ) %>%
                    #     # mutate(., time2adopt = ifelse(is.na(time2adopt), 0, time2adopt)) %>%
                    #     # mutate(timeSinceLastAdopter = as.numeric(App.Complete.Date - first.entrant.date) ) %>%
    #REMOVE NA for application IDs (..................doesn't work we correct this after this dplyr pipe.....................................................)
        filter(!is.na(Application.Id) | !is.null(Application.Id)) %>% 
####MAKE IT A LONG PANEL
    #CREATE a year-month var
        mutate(ym=as.yearmon(First.Pending.Payment.Date)) %>% #create the yearmonth variable for the groupings/cumsum
    #CALC. number of adopters in that ZCTA5 & on that App.Complete.Date
        arrange(First.Pending.Payment.Date)  %>% # cannot remove this
        group_by(ZCTA5,First.Pending.Payment.Date) %>%
            mutate(n.ZCTA5.day = n()) %>%
            ungroup() %>%
    #REORDER: put the vars I am most interested in looking at first
        dplyr::select(ZCTA5,Service.County,First.Pending.Payment.Date,first.entrant.date,n.ZCTA5.day,everything()) %>%
        arrange(ZCTA5)#  %>% #dim by here is  51968   194
    
## 11/18/2020 point #5
  #1. get last noncontemp installer date
  #2. merge with csi.long (named temp, here)
  #3. only take first row in group (slice(1))
    # GET DATE OF THE NONCONTEMPORANEOUS LAST INSTALLER IN ZCTA5  
        temp = csi.long%>% 
            rename(., Service.Zip= ZCTA5) %>%
            arrange(Service.Zip,First.Pending.Payment.Date) %>%
            mutate(First.Pending.Payment.Date = as.Date(First.Pending.Payment.Date),
                   rowNum = row_number())
        yyy = temp %>%
            left_join(temp, by = "Service.Zip") %>%
            # discard all the out-of-scope dates
            mutate(First.Pending.Payment.Date.y = ifelse(First.Pending.Payment.Date.y < First.Pending.Payment.Date.x,First.Pending.Payment.Date.y, NA)) %>%
            # we need to include row number here to preserve all rows in the original
            group_by(Service.Zip, First.Pending.Payment.Date.x, rowNum.x) %>% #dim: 10,691,355       35
            # na.rm = TRUE handles all the missing values removed in the previous mutate
            summarise(First.Pending.Payment.Date.y = max(First.Pending.Payment.Date.y, na.rm = TRUE), .groups = 'drop') %>% #dim: 58,957 x 5
            # summarise may return numeric type rather than date type - convert back
            mutate(First.Pending.Payment.Date.y = as.Date(First.Pending.Payment.Date.y, origin = "1970-01-01")) %>%
            # rename to output
                # dplyr:::select(.,First.Pending.Payment.Date = First.Pending.Payment.Date.x,Last.Unique.Date.In.ZCTA5 = First.Pending.Payment.Date.y) %>%
            ungroup(); head(yyy);dim(yyy);class(yyy); str(yyy)  #dim: 58,957 x 3
        #DEFINE timeSinceLastZCTA5adoption
            #11/23/2020, seems to be an issue with the filler variable and I'm not quite sure why, I see the 
                # error is: "number of items to replace is not a multiple of replacement length"
            xx = as.numeric(yyy$First.Pending.Payment.Date.y);#xx
            filler= as.numeric(yyy$First.Pending.Payment.Date.x); #filler
            xx[which(!is.finite(yyy$First.Pending.Payment.Date.y))] = filler[which(!is.finite(xx))];  length(xx); #xx;
            xx[which(!is.finite(xx))]<-0;xx; length(xx)
            xx[which(xx==0)]<-filler;head(xx)#integer form of the final (and correct) dates for last installed PV system 
            yyy$First.Pending.Payment.Date.y=xx;# head(yyy); view(yyy);class(yyy);str(yyy);dim(yyy)
            yyy %<>% mutate(First.Pending.Payment.Date.y = as.Date(First.Pending.Payment.Date.y,origin = "1970-01-01")) %>%
                rename(rowNum = rowNum.x)  #rename yyy$rowNum.x

        #JOIN temp and yyy; results for correct rows 
            install.data = left_join(temp, yyy, by = "rowNum"); dim(install.data) #dim: 51968 x 199 
    ##Finish variable naming (First.Pending.Payment.Date.y, Service.Zip.x) 
            #drop redundant rows (First.Pending.Payment.Date.x, rowNum, .groups) and poorly defined (time2adopt)
        #DEFINE new variable timelastInstall.zcta5 (y from eqn 7 [from sbg2010])
            #MAKE PANEL (install.data is made from temp which is made from csi.long, so we're functionally JUST adding a few cols)
            csi.long=install.data %>% 
                rename(ZCTA5 = Service.Zip.x, Last.noncontemp.Adoption.Date = First.Pending.Payment.Date.y) %>%
                dplyr::select(.,-First.Pending.Payment.Date.x, -Service.Zip.y, -.groups) %>% #drop some rows
                #DEFINE timelastInstall.zcta5 and timefirstInstall.zcta5
                    arrange(First.Pending.Payment.Date)%>%
                    group_by(ZCTA5) %>%
                    mutate(timelastInstall.zcta5 = as.numeric(First.Pending.Payment.Date)- as.numeric(Last.noncontemp.Adoption.Date)) %>%
                    mutate(timefirstInstall.zcta5 = as.numeric(First.Pending.Payment.Date)- as.numeric(first.entrant.date) ) %>% #added 11/29/2020
                #edit 11/24/2020, add variable for installed MW (Nameplate.Rating is in KW so I need to divide by 1000)
                    # I assume nameplate.rating is best versus system.size.dc (MUST BE DC because that's mentioned pg 907 footnote 16)
                drop_na(First.Pending.Payment.Date) %>% 
                #DEFINE VARIABLES FOR INSTALLED CAPACITY
                    arrange(First.Pending.Payment.Date) %>%
                    group_by(ZCTA5) %>%
                        mutate(kw.zcta5.day = Nameplate.Rating,            mw.zcta5.day = Nameplate.Rating/1000, 
                               cumsum.kw.zcta5 = cumsum(Nameplate.Rating), cumsum.mw.zcta5 = cumsum(mw.zcta5.day)) %>%
                        ungroup() %>%
                #CALC. cumulative sum of counts per zcta5, can't have this until the row per ZCTA & DATE are controlled for
                arrange(First.Pending.Payment.Date)  %>% # just ensuring it's enforced
                group_by(ZCTA5) %>%
                    mutate(cumsum.ZCTA5 = cumsum(n.ZCTA5.day)) %>%
                    ungroup() %>%
                arrange(ZCTA5) %>%
                #CALC. installed base
                arrange(First.Pending.Payment.Date) %>% # cannot remove this.........#dim by here, 47359   224 (dropped 89 rows)
                    group_by(First.Pending.Payment.Date, ZCTA5) %>%
                    mutate(base.ZCTA5.day = ifelse(is.na(First.Pending.Payment.Date),NA,cumsum.ZCTA5 - n.ZCTA5.day),
                           base.kw.ZCTA5.day = ifelse(is.na(First.Pending.Payment.Date),NA,cumsum.kw.zcta5 - kw.zcta5.day),      #added 11/24/2020
                           base.mw.ZCTA5.day = ifelse(is.na(First.Pending.Payment.Date),NA,cumsum.mw.zcta5 - mw.zcta5.day)  ) %>%#added 11/24/2020
                    ungroup() %>%
              #EDIT 11/24/2020
                #DEFINE 
                arrange(First.Pending.Payment.Date)  %>% # just ensuring it's enforced
                group_by(ZCTA5) %>%     
                    mutate(pop.less.base.zcta5 = pop - base.ZCTA5.day ) %>% 
                    mutate(ownocchousing.less.base.zcta5 = occ.h.ownocc - base.ZCTA5.day) %>% 
                    ungroup()%>%
                #DEFINE Y_ZT
                arrange(First.Pending.Payment.Date)  %>% # just ensuring it's enforced
                group_by(ZCTA5) %>%    
                    mutate(prob.pop.less.base.zcta5     = n.ZCTA5.day/pop.less.base.zcta5) %>%
                    mutate(prob.ownocch.less.base.zcta5 = n.ZCTA5.day/ownocchousing.less.base.zcta5) %>%
                    ungroup()%>%
              #end    
              #EDIT 11/25/2020
                arrange(First.Pending.Payment.Date)  %>% # just ensuring it's enforced
                mutate(quarter.FPPD=quarter(First.Pending.Payment.Date)) %>%
              #END
                #ARRANGE COLS
                dplyr::select(ZCTA5,First.Pending.Payment.Date,Last.noncontemp.Adoption.Date,timelastInstall.zcta5,timefirstInstall.zcta5,
                              n.ZCTA5.day,kw.zcta5.day,mw.zcta5.day,                                               #PV: count, system kw(/mw) per installation
                              cumsum.ZCTA5,cumsum.kw.zcta5,cumsum.mw.zcta5,                                        #cumsum PV count, PV install cap (kw,mw)
                              base.ZCTA5.day,base.kw.ZCTA5.day,base.mw.ZCTA5.day,                                  #installed base
                              prob.ownocch.less.base.zcta5,prob.pop.less.base.zcta5,                                 #Y_zt
                              Program.Administrator,Incentive.Design,Incentive.Step,Incentive.Type,Incentive.Amount, #relates to incentive
                              quarter.FPPD,dow,
                              everything(),
                              -rowNum,-w) %>%
                #ARRANGE ROWS
                arrange(ZCTA5); dim(csi.long) #dim: 51879 x 190
        #edit 11/29/2020
        csi.long = csi.long[,!grepl(c("_dec"),names(csi.long))]; dim(csi.long)#dim: 51879 x 188; 
            

            
   #.................BIG IDEA......................
        #SHORT PANEL? 
            #MAKE INSTALLATION DATA (DROPPING NONCONTEMPORANEOUS INSTALLATIONS)
                #NOTE: ANY ID VARIABLE OR SELF-INSTALLER INFORMATION IS MADE USELESS HERE
            install.data = csi.long %>%
                #DROP NA rows by variable: First.Pending.Payment.Date 
                drop_na(First.Pending.Payment.Date) %>% #dim: 47359 x 224 (dropped 89 rows).....................probably should have been done earlier, must redo (I later added it to the csi.long definition, so it's NOW redundant)
                #SELECT.....LAST row in group (for cumsums)
                    group_by(ZCTA5,First.Pending.Payment.Date) %>% 
                        slice(n()) %>%                     
                        ungroup() %>%
                dplyr::select(ZCTA5,First.Pending.Payment.Date,Last.noncontemp.Adoption.Date,timelastInstall.zcta5,
                              n.ZCTA5.day,kw.zcta5.day,mw.zcta5.day,                                                 #PV: count, system kw(/mw) per installation
                                cumsum.ZCTA5,cumsum.kw.zcta5,cumsum.mw.zcta5,                                        #cumsum PV count, PV install cap (kw,mw)
                                base.ZCTA5.day,base.kw.ZCTA5.day,base.mw.ZCTA5.day,                                  #installed base
                              prob.ownocch.less.base.zcta5,prob.pop.less.base.zcta5,                                 #Y_zt
                              Program.Administrator,Incentive.Design,Incentive.Step,Incentive.Type,Incentive.Amount, #relates to incentive
                              quarter.FPPD,dow,
                              everything()) %>%
                arrange(ZCTA5); dim(install.data) #dim: 47,359 X 188
            # View(install.data)
            
    #edit 11/29/2020,
        #DEFINE: NEW TYPE CSI.LONG
    #             csi.long.short = csi.long[1:80,] %>%
    #                             dplyr::select(
    #                         #CSI
    #                             "Application.Id","status","first.entrant.date",
    #                             "ZCTA5","First.Pending.Payment.Date","Last.noncontemp.Adoption.Date","timelastInstall.zcta5",
    #                             "n.ZCTA5.day","kw.zcta5.day","mw.zcta5.day",                                              #PV: count","system kw(/mw) per installation
    #                             "cumsum.ZCTA5","cumsum.kw.zcta5","cumsum.mw.zcta5",                                       #cumsum PV count","PV install cap (kw","mw)
    #                             "base.ZCTA5.day","base.kw.ZCTA5.day","base.mw.ZCTA5.day",                                 #installed base
    #                             # "prob.ownocch.less.base.zcta5","prob.pop.less.base.zcta5",                                #Y_zt
    #                             "Program.Administrator","Incentive.Design","Incentive.Step","Incentive.Type","Incentive.Amount",#relates to incentive
    #                             "quarter.FPPD","dow",
    #                             "Electric.Vehicle","Electric.Vehicle.Count",
    #                         #DEMOGRAPHICS seen in stata code
    #                             "pop",
    #                                 "age0019","age2045","age65plus",# "pop.house.tenure.total","pop.house.tenure.ownocc.loan","pop.house.tenure.ownocc.loanfree", "pop.house.tenure.rent","population",
    #                             "Male",#"Female",
    #                             "collegedegrees",
    #                             "whitepop",
    #                             "workcar","carpool","pubtransit","workhome","t30mincommute",#"total.commuting",
    #                             "med_inc_HH",#"med_inc_family",
    #                             "medbeds",
    #                             "size_HH_OwnOc",
    #                                 "medhomeval_ownOcc",
    #                                 "homeval.totalCount",
    #                                     "homeval.0kto50k","homeval.50kto90k","homeval.90kto175k","homeval.175kto400k",
    #                                     "homeval.400kplus"
    #                         # #NEW VARS    
    #                         #         "fuel_total","fuel_gas","fuel_gasother","fuel_electricity","fuel_kero_oil","fuel_coal","fuel_wood","fuel_solar","fuel_other","fuel_none",
    #                         #         "medMort_ownW",
    #                         #         "occ.h.ownocc"
    #                             ); dim(csi.long.short) #dim:  51879 x 44
    # #----............................
    # #REMOVE MASSIVE MEMORY STEALERS
    #     #CANNOT DELETE csi OR csi.untouched OR demog
    #     rm(acs.blkgrp_zcta5,acs2011);#rm(all.na);#rm(cols2keep);rm(csi.11_18_2020); rm(csi.old);rm(csi.long);     
    #     rm(counts.zcta5, csi.b4.joined.appnumbers, csi.dat);
    #     rm(csi.old, csi.working.dat,  csi.working.dat.dates.incentives.etc,  dec.blkgrp_zcta5)
    #     rm(cols2keep, dec2010, joined, medians.zcta5,matched.raw.NEM.WD)
    #     rm(demog.old, temp, yyy, w.blkgrp_zcta5)
    #     #clear memory, literally halved the active memory seen on task manager
    #     gc()
    # #----............................
    #     
    #     # NEW DF: expand rows for each id to have status=0 dates per application between first.entrant date (by zcta5) and first.pending.payment.date
    #         #NOTE: 
    #         #       dim(csi.long) = 51879 x dim188
    #         #------
    #         #first try
    #         csi.wide = csi.long.short %>%
    #             group_by(Application.Id, status) %>%# ZCTA5
    #             mutate(date = list(seq.Date(first.entrant.date, First.Pending.Payment.Date, by = "day"))) %>%
    #             tidyr::unnest(); dim(csi.wide)
    #             
    # #11/29/2020: logistic regression for conditional probability of adoption given an adoption that zcta5-day
    #             glm(formula = factor(status) ~ Electric.Vehicle + ZCTA5 + First.Pending.Payment.Date + Last.noncontemp.Adoption.Date + 
    #                     timelastInstall.zcta5 + timefirstInstall.zcta5 + n.ZCTA5.day + kw.zcta5.day + mw.zcta5.day + cumsum.ZCTA5 + cumsum.kw.zcta5 + cumsum.mw.zcta5 + 
    #                     base.ZCTA5.day + base.kw.ZCTA5.day + base.mw.ZCTA5.day + prob.ownocch.less.base.zcta5 + prob.pop.less.base.zcta5 + 
    #                     Electric.Vehicle.Count+
    #                     population_dec+pop.house.tenure.total+pop.house.tenure.ownocc.loan+pop.house.tenure.ownocc.loanfree+pop.house.tenure.rent+population+Male+Female+whitepop+
    #                     total.commuting+carpool+pubtransit+workhome+t30mincommute+workcar+
    #                     collegedegrees+
    #                     medbeds+
    #                     fuel_total+fuel_gas+fuel_gasother+fuel_electricity+fuel_kero_oil+fuel_coal+fuel_wood+fuel_solar+fuel_other+fuel_none+
    #                     homeval.totalCount+
    #                     medMort_ownW+
    #                     occ.h.ownocc+
    #                     size_HH_OwnOc+med_inc_HH+med_inc_family+medhomeval_ownOcc+
    #                     pop+age0019+age2045+age65plus+
    #                     homeval.0kto50k+homeval.50kto90k+homeval.90kto175k+homeval.175kto400k+homeval.400kplus, 
    #                 family = "binomial", data = csi.long)
    # 
    # # 11/29/2020 survival probs
    #             #timefirstInstall.zcta5
    #             plot(survfit(Surv(timefirstInstall.zcta5, status) ~ 1, data = csi.long), 
    #                  xlab = "Days", 
    #                  ylab = "Overall survival probability")
    #             ggsurvplot(
    #                 fit = survfit(Surv(timefirstInstall.zcta5, status) ~ 1, data = csi.long), 
    #                 xlab = "Days", 
    #                 ylab = "Overall survival probability")    
    #             #timelastInstall.zcta5
    #             plot(survfit(Surv(timelastInstall.zcta5, status) ~ 1, data = csi.long), 
    #                  xlab = "Days", 
    #                  ylab = "Overall survival probability")
    #             ggsurvplot(
    #                 fit = survfit(Surv(timelastInstall.zcta5, status) ~ 1, data = csi.long), 
    #                 xlab = "Days", 
    #                 ylab = "Overall survival probability")            
    
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    
    
    
    
    
    
    
    
    
    
    


# c(csi.$ZCTA5,csi.$First.Pending.Payment.Date,csi.$Last.noncontemp.Adoption.Date,csi.$timelastInstall.zcta5,csi.$timefirstInstall.zcta5,csi.$n.ZCTA5.day,csi.$kw.zcta5.day,csi.$mw.zcta5.day,csi.$cumsum.ZCTA5,csi.$cumsum.kw.zcta5,csi.$cumsum.mw.zcta5,csi.$base.ZCTA5.day,csi.$base.kw.ZCTA5.day,csi.$base.mw.ZCTA5.day,csi.$prob.ownocch.less.base.zcta5,csi.$prob.pop.less.base.zcta5,csi.$Program.Administrator,csi.$Incentive.Design,csi.$Incentive.Step,csi.$Incentive.Type,csi.$Incentive.Amount,csi.$quarter.FPPD,csi.$Service.County,csi.$first.entrant.date,csi.$Application.Id,csi.$Preceding.Id,csi.$Superceding.Id,csi.$Matched.CSI.Application.Number,csi.$Application.Status,csi.$Utility,csi.$Service.City,csi.$Technology.Type,csi.$System.Size.DC,csi.$System.Size.AC,csi.$Tilt,csi.$Azimuth,csi.$Mounting.Method,csi.$Tracking,csi.$Customer.Sector,csi.$App.Received.Date,csi.$App.Complete.Date,csi.$App.Approved.Date,csi.$Decommissioned.Date,csi.$Self.Installer,csi.$Installer.Name,csi.$Installer.Phone,csi.$Installer.City,csi.$Installer.State,csi.$Installer.Zip,csi.$CSLB.Number,csi.$Third.Party.Owned,csi.$Third.Party.Owned.Type,csi.$Third.Party.Name,csi.$Pace.Financed,csi.$Pace.Financier,csi.$Electric.Vehicle,csi.$Electric.Vehicle.Count,csi.$System.Output.Monitoring,csi.$System.Output.Reports.To.Vendor.,csi.$System.Output.Monitoring.Provider,csi.$Total.System.Cost,csi.$Itc.Cost.Basis,csi.$Cost.Watt,csi.$NEM.Tariff,csi.$VNEM..NEM.V..NEM.Agg,csi.$Project.is.VNEM..NEM.V..NEM.Agg.,csi.$Module.Model.1,csi.$Module.Manufacturer.1,csi.$Module.Quantity.1,csi.$Module.Model.2,csi.$Module.Manufacturer.2,csi.$Module.Quantity.2,csi.$Module.Model.3,csi.$Module.Manufacturer.3,csi.$Module.Quantity.3,csi.$Module.Model.4,csi.$Module.Manufacturer.4,csi.$Module.Quantity.4,csi.$Module.Model.5,csi.$Module.Manufacturer.5,csi.$Module.Quantity.5,csi.$Module.Model.6,csi.$Module.Manufacturer.6,csi.$Module.Quantity.6,csi.$Module.Model.7,csi.$Module.Manufacturer.7,csi.$Module.Quantity.7,csi.$Module.Model.8,csi.$Module.Manufacturer.8,csi.$Module.Quantity.8,csi.$Module.Model.9,csi.$Module.Manufacturer.9,csi.$Module.Quantity.9,csi.$Module.Model.10,csi.$Module.Manufacturer.10,csi.$Module.Quantity.10,csi.$Inverter.Model.1,csi.$Inverter.Manufacturer.1,csi.$Inverter.Quantity.1,csi.$Inverter.Model.2,csi.$Inverter.Manufacturer.2,csi.$Inverter.Quantity.2,csi.$Inverter.Model.3,csi.$Inverter.Manufacturer.3,csi.$Inverter.Quantity.3,csi.$Inverter.Model.4,csi.$Inverter.Manufacturer.4,csi.$Inverter.Quantity.4,csi.$Inverter.Model.5,csi.$Inverter.Manufacturer.5,csi.$Inverter.Quantity.5,csi.$Inverter.Model.6,csi.$Inverter.Manufacturer.6,csi.$Inverter.Quantity.6,csi.$Inverter.Model.7,csi.$Inverter.Manufacturer.7,csi.$Inverter.Quantity.7,csi.$Inverter.Model.8,csi.$Inverter.Manufacturer.8,csi.$Inverter.Quantity.8,csi.$Inverter.Model.9,csi.$Inverter.Manufacturer.9,csi.$Inverter.Quantity.9,csi.$Current.Incentive.Application.Status,csi.$Is.a.PBI.Buyout.Application,csi.$First.Online.Incentive.Claim.Request.Submitted.Date,csi.$First.Incentive.Claim.Request.Review.Date,csi.$First.Suspended...Incentive.Claim.Request.Review.Date,csi.$MASH.Track.1A.Incentive.Amount,csi.$MASH.Track.1B.Incentive.Amount,csi.$MASH.Track.2.Incentive.Amount,csi.$First.New.Reservation.Request.Date,csi.$First.Online.Reservation.Request.Submitted.Date,csi.$First.Reservation.Request.Review.Date,csi.$First.Pending.RFP.Date,csi.$First.RFP.Review..Gov.t.Non.Profit.only..Date,csi.$First.Suspended...RFP.Review.Date,csi.$First.Reservation.Reserved.Date,csi.$First.Online.Proof.of.Project.Milestones.Submitted.Date,csi.$First.Proof.of.Project.Milestones.Review.Date,csi.$First.Suspended...Milestone.Review.Date,csi.$First.Suspended...Reservation.Review.Date,csi.$First.Confirmed.Reservation.Date,csi.$First.PBI...In.Payment.Date,csi.$First.Completed.Date,csi.$First.Withdrawn.Date,csi.$First.Cancelled.Date,csi.$First.Site.Transferred.Date,csi.$First.System.Removed.Date,csi.$Nameplate.Rating,csi.$CEC.PTC.Rating,csi.$CSI.Rating,csi.$CEC.PTC.Rating.Fixed,csi.$CEC.PTC.Rating.Single.Axis.Tracking,csi.$CEC.PTC.Rating.Dual.Axis.Tracking,csi.$CSI.Rating.Fixed,csi.$CSI.Rating.Single.Axis.Tracking,csi.$CSI.Rating.Dual.Axis.Tracking,csi.$Design.Factor,csi.$Program,csi.$status,csi.$population_dec,csi.$pop.house.tenure.total,csi.$pop.house.tenure.ownocc.loan,csi.$pop.house.tenure.ownocc.loanfree,csi.$pop.house.tenure.rent,csi.$population,csi.$Male,csi.$Female,csi.$whitepop,csi.$work_at_home_imputedtraveltime,csi.$carpool,csi.$pubtransit,csi.$beds_total,csi.$beds_0,csi.$beds_1,csi.$beds_2,csi.$beds_3,csi.$beds_4,csi.$beds_5more,csi.$fuel_total,csi.$fuel_gas,csi.$fuel_gasother,csi.$fuel_electricity,csi.$fuel_kero_oil,csi.$fuel_coal,csi.$fuel_wood,csi.$fuel_solar,csi.$fuel_other,csi.$fuel_none,csi.$mort_status,csi.$mort_agg_val_total,csi.$mort_agg_val_wMort,csi.$mort_agg_val_woMort,csi.$mort_agg_own_total_cost,csi.$mort_agg_own_wMort,csi.$mort_agg_own_woMort,csi.$workcar,csi.$occ.h.total,csi.$occ.h.ownocc,csi.$occ.h.not.ownocc,csi.$hbt.total,csi.$hbt.tfhmcoo,csi.$hbt.tfhofmaleoo,csi.$hbt.tfhoffemaleoo,csi.$hbt.tnfoo,csi.$size_HH_tot,csi.$size_HH_OwnOc,csi.$med_inc_HH,csi.$med_inc_family,csi.$medhomeval_ownOcc,csi.$medbeds,csi.$medmort_ownTC,csi.$medMort_ownW,csi.$medmort_ownWO,csi.$pop,csi.$age0019,csi.$age2045,csi.$age65plus,csi.$collegedegrees,csi.$homeval.0kto50k,csi.$homeval.50kto90k,csi.$homeval.90kto175k,csi.$homeval.175kto400k,csi.$homeval.400kplus,csi.$t30mincommute,csi.$homeval.totalCount,csi.$total.commuting,csi.$medmort_ownW,csi.$mort_agg_ownW,csi.$workhome)
# c(csi.$ZCTA5,First.Pending.Payment.Date,Last.noncontemp.Adoption.Date,timelastInstall.zcta5,timefirstInstall.zcta5,n.ZCTA5.day,kw.zcta5.day,mw.zcta5.day,cumsum.ZCTA5,cumsum.kw.zcta5,cumsum.mw.zcta5,base.ZCTA5.day,base.kw.ZCTA5.day,base.mw.ZCTA5.day,prob.ownocch.less.base.zcta5,prob.pop.less.base.zcta5,Program.Administrator,Incentive.Design,Incentive.Step,Incentive.Type,Incentive.Amount,quarter.FPPD,Service.County,first.entrant.date,Application.Id,Preceding.Id,Superceding.Id,Matched.CSI.Application.Number,Application.Status,Utility,Service.City,Technology.Type,System.Size.DC,System.Size.AC,Tilt,Azimuth,Mounting.Method,Tracking,Customer.Sector,App.Received.Date,App.Complete.Date,App.Approved.Date,Decommissioned.Date,Self.Installer,Installer.Name,Installer.Phone,Installer.City,Installer.State,Installer.Zip,CSLB.Number,Third.Party.Owned,Third.Party.Owned.Type,Third.Party.Name,Pace.Financed,Pace.Financier,Electric.Vehicle,Electric.Vehicle.Count,System.Output.Monitoring,System.Output.Reports.To.Vendor.,System.Output.Monitoring.Provider,Total.System.Cost,Itc.Cost.Basis,Cost.Watt,NEM.Tariff,VNEM..NEM.V..NEM.Agg,Project.is.VNEM..NEM.V..NEM.Agg.,Module.Model.1,Module.Manufacturer.1,Module.Quantity.1,Module.Model.2,Module.Manufacturer.2,Module.Quantity.2,Module.Model.3,Module.Manufacturer.3,Module.Quantity.3,Module.Model.4,Module.Manufacturer.4,Module.Quantity.4,Module.Model.5,Module.Manufacturer.5,Module.Quantity.5,Module.Model.6,Module.Manufacturer.6,Module.Quantity.6,Module.Model.7,Module.Manufacturer.7,Module.Quantity.7,Module.Model.8,Module.Manufacturer.8,Module.Quantity.8,Module.Model.9,Module.Manufacturer.9,Module.Quantity.9,Module.Model.10,Module.Manufacturer.10,Module.Quantity.10,Inverter.Model.1,Inverter.Manufacturer.1,Inverter.Quantity.1,Inverter.Model.2,Inverter.Manufacturer.2,Inverter.Quantity.2,Inverter.Model.3,Inverter.Manufacturer.3,Inverter.Quantity.3,Inverter.Model.4,Inverter.Manufacturer.4,Inverter.Quantity.4,Inverter.Model.5,Inverter.Manufacturer.5,Inverter.Quantity.5,Inverter.Model.6,Inverter.Manufacturer.6,Inverter.Quantity.6,Inverter.Model.7,Inverter.Manufacturer.7,Inverter.Quantity.7,Inverter.Model.8,Inverter.Manufacturer.8,Inverter.Quantity.8,Inverter.Model.9,Inverter.Manufacturer.9,Inverter.Quantity.9,Current.Incentive.Application.Status,Is.a.PBI.Buyout.Application,First.Online.Incentive.Claim.Request.Submitted.Date,First.Incentive.Claim.Request.Review.Date,First.Suspended...Incentive.Claim.Request.Review.Date,MASH.Track.1A.Incentive.Amount,MASH.Track.1B.Incentive.Amount,MASH.Track.2.Incentive.Amount,First.New.Reservation.Request.Date,First.Online.Reservation.Request.Submitted.Date,First.Reservation.Request.Review.Date,First.Pending.RFP.Date,First.RFP.Review..Gov.t.Non.Profit.only..Date,First.Suspended...RFP.Review.Date,First.Reservation.Reserved.Date,First.Online.Proof.of.Project.Milestones.Submitted.Date,First.Proof.of.Project.Milestones.Review.Date,First.Suspended...Milestone.Review.Date,First.Suspended...Reservation.Review.Date,First.Confirmed.Reservation.Date,First.PBI...In.Payment.Date,First.Completed.Date,First.Withdrawn.Date,First.Cancelled.Date,First.Site.Transferred.Date,First.System.Removed.Date,Nameplate.Rating,CEC.PTC.Rating,CSI.Rating,CEC.PTC.Rating.Fixed,CEC.PTC.Rating.Single.Axis.Tracking,CEC.PTC.Rating.Dual.Axis.Tracking,CSI.Rating.Fixed,CSI.Rating.Single.Axis.Tracking,CSI.Rating.Dual.Axis.Tracking,Design.Factor,Program,status,population_dec,pop.house.tenure.total,pop.house.tenure.ownocc.loan,pop.house.tenure.ownocc.loanfree,pop.house.tenure.rent,population,Male,Female,whitepop,work_at_home_imputedtraveltime,carpool,pubtransit,beds_total,beds_0,beds_1,beds_2,beds_3,beds_4,beds_5more,fuel_total,fuel_gas,fuel_gasother,fuel_electricity,fuel_kero_oil,fuel_coal,fuel_wood,fuel_solar,fuel_other,fuel_none,mort_status,mort_agg_val_total,mort_agg_val_wMort,mort_agg_val_woMort,mort_agg_own_total_cost,mort_agg_own_wMort,mort_agg_own_woMort,workcar,occ.h.total,occ.h.ownocc,occ.h.not.ownocc,hbt.total,hbt.tfhmcoo,hbt.tfhofmaleoo,hbt.tfhoffemaleoo,hbt.tnfoo,size_HH_tot,size_HH_OwnOc,med_inc_HH,med_inc_family,medhomeval_ownOcc,medbeds,medmort_ownTC,medMort_ownW,medmort_ownWO,pop,age0019,age2045,age65plus,collegedegrees,homeval.0kto50k,homeval.50kto90k,homeval.90kto175k,homeval.175kto400k,homeval.400kplus,t30mincommute,homeval.totalCount,total.commuting,medmort_ownW,mort_agg_ownW,workhome)
# Male_dec,Female_dec,w,
# ym,rowNum,
# pop.less.base.zcta5,ownocchousing.less.base.zcta5
            ## removed 11/29/2020
            # glm(formula = factor(status) ~ factor(Electric.Vehicle) + ZCTA5 + First.Pending.Payment.Date + Last.noncontemp.Adoption.Date + 
            #         timelastInstall.zcta5 + timefirstInstall.zcta5 + n.ZCTA5.day + kw.zcta5.day + mw.zcta5.day + cumsum.ZCTA5 + cumsum.kw.zcta5 + cumsum.mw.zcta5 + 
            #         base.ZCTA5.day + base.kw.ZCTA5.day + base.mw.ZCTA5.day + prob.ownocch.less.base.zcta5 + prob.pop.less.base.zcta5 + 
            #         Electric.Vehicle.Count+
            #         #Program.Administrator+Incentive.Design+Incentive.Step+Incentive.Type+Incentive.Amount+quarter.FPPD+Service.County+first.entrant.date+Application.Id+Preceding.Id+Superceding.Id+Matched.CSI.Application.Number+Application.Status+Utility+Service.City+Technology.Type+System.Size.DC+System.Size.AC+Tilt+Azimuth+Mounting.Method+Tracking+Customer.Sector+App.Received.Date+App.Complete.Date+App.Approved.Date+Decommissioned.Date+Self.Installer+Installer.Name+Installer.Phone+Installer.City+Installer.State+Installer.Zip+CSLB.Number+Third.Party.Owned+Third.Party.Owned.Type+Third.Party.Name+Pace.Financed+Pace.Financier+
            #         # System.Output.Monitoring+System.Output.Reports.To.Vendor.+System.Output.Monitoring.Provider+
            #         # Total.System.Cost+Itc.Cost.Basis+Cost.Watt+NEM.Tariff+VNEM..NEM.V..NEM.Agg+Project.is.VNEM..NEM.V..NEM.Agg.+
            #         # Module.Model.1+Module.Manufacturer.1+Module.Quantity.1+Module.Model.2+Module.Manufacturer.2+Module.Quantity.2+Module.Model.3+Module.Manufacturer.3+Module.Quantity.3+Module.Model.4+Module.Manufacturer.4+Module.Quantity.4+Module.Model.5+Module.Manufacturer.5+Module.Quantity.5+Module.Model.6+Module.Manufacturer.6+Module.Quantity.6+Module.Model.7+Module.Manufacturer.7+Module.Quantity.7+Module.Model.8+Module.Manufacturer.8+Module.Quantity.8+Module.Model.9+Module.Manufacturer.9+Module.Quantity.9+Module.Model.10+Module.Manufacturer.10+Module.Quantity.10+Inverter.Model.1+Inverter.Manufacturer.1+Inverter.Quantity.1+Inverter.Model.2+Inverter.Manufacturer.2+Inverter.Quantity.2+Inverter.Model.3+Inverter.Manufacturer.3+Inverter.Quantity.3+Inverter.Model.4+Inverter.Manufacturer.4+Inverter.Quantity.4+Inverter.Model.5+Inverter.Manufacturer.5+Inverter.Quantity.5+Inverter.Model.6+Inverter.Manufacturer.6+Inverter.Quantity.6+Inverter.Model.7+Inverter.Manufacturer.7+Inverter.Quantity.7+Inverter.Model.8+Inverter.Manufacturer.8+Inverter.Quantity.8+Inverter.Model.9+Inverter.Manufacturer.9+Inverter.Quantity.9+Current.Incentive.Application.Status+Is.a.PBI.Buyout.Application+First.Online.Incentive.Claim.Request.Submitted.Date+First.Incentive.Claim.Request.Review.Date+First.Suspended...Incentive.Claim.Request.Review.Date+MASH.Track.1A.Incentive.Amount+MASH.Track.1B.Incentive.Amount+
            #         # MASH.Track.2.Incentive.Amount+First.New.Reservation.Request.Date+First.Online.Reservation.Request.Submitted.Date+First.Reservation.Request.Review.Date+First.Pending.RFP.Date+First.RFP.Review..Gov.t.Non.Profit.only..Date+First.Suspended...RFP.Review.Date+First.Reservation.Reserved.Date+First.Online.Proof.of.Project.Milestones.Submitted.Date+First.Proof.of.Project.Milestones.Review.Date+First.Suspended...Milestone.Review.Date+First.Suspended...Reservation.Review.Date+First.Confirmed.Reservation.Date+First.PBI...In.Payment.Date+First.Completed.Date+First.Withdrawn.Date+First.Cancelled.Date+First.Site.Transferred.Date+First.System.Removed.Date+Nameplate.Rating+CEC.PTC.Rating+CSI.Rating+CEC.PTC.Rating.Fixed+CEC.PTC.Rating.Single.Axis.Tracking+CEC.PTC.Rating.Dual.Axis.Tracking+CSI.Rating.Fixed+CSI.Rating.Single.Axis.Tracking+CSI.Rating.Dual.Axis.Tracking+Design.Factor+
            #         # Electric.Vehicle+ Program+ status+
            #         population_dec+pop.house.tenure.total+pop.house.tenure.ownocc.loan+pop.house.tenure.ownocc.loanfree+pop.house.tenure.rent+population+Male+Female+whitepop+
            #         total.commuting+carpool+pubtransit+workhome+t30mincommute+workcar+#work_at_home_imputedtraveltime+
            #         collegedegrees+
            #         medbeds+#beds_total+beds_0+beds_1+beds_2+beds_3+beds_4+beds_5more+
            #         fuel_total+fuel_gas+fuel_gasother+fuel_electricity+fuel_kero_oil+fuel_coal+fuel_wood+fuel_solar+fuel_other+fuel_none+
            #         homeval.totalCount+
            #         medMort_ownW+#medmort_ownWO+medmort_ownW+mort_agg_ownW+mort_status+mort_agg_val_total+mort_agg_val_wMort+mort_agg_val_woMort+mort_agg_own_total_cost+mort_agg_own_wMort+mort_agg_own_woMort+medmort_ownTC+
            #         occ.h.ownocc+#occ.h.total+occ.h.not.ownocc+hbt.total+hbt.tfhmcoo+hbt.tfhofmaleoo+hbt.tfhoffemaleoo+hbt.tnfoo+size_HH_tot+
            #         size_HH_OwnOc+med_inc_HH+med_inc_family+medhomeval_ownOcc+
            #         pop+age0019+age2045+age65plus+
            #         homeval.0kto50k+homeval.50kto90k+homeval.90kto175k+homeval.175kto400k+homeval.400kplus, 
            #     family = "binomial", data = csi.long)
            
            
            
            
            
            
            
            # 12/3/2020, don't work because they don't preserve other information well
            # #second try
            # csi.wide2 = reshape(csi.long[1:8,],
            #                     idvar = "ZCTA5",
            #                     timevar = "First.Pending.Payment.Date", 
            #                     direction = "wide");dim(csi.wide2)
            # #third try
            # csi.wide3 = spread(csi.long, 
            #                    key = ZCTA5, 
            #                    value = First.Pending.Payment.Date);dim(csi.wide3)
            # #fourth try
            # csi.wide4 = csi.long %>% 
            #                 pivot_wider(names_from = ZCTA5, values_from = First.Pending.Payment.Date)
            