# SCRIPT TO CREATE FUNCTIONS FOR SUMMER PAPER 2020
# AUTHOR: THOMAS DEVINE
# currdir: "C:/Users/tdevine/Desktop/2020 Summer Paper/Analysis"
# CREATED: (when I made my code neat) 8.24.2020

# The columns we care to keep (that I can see we may need to use)
    cols2keep = c(
        "Application.Id"        ,        #"Preceding.Id"              ,"Superceding.Id"       ,"Matched.CSI.Application.Number",   
        "Application.Status"    ,"Utility"                   ,"Service.City"         ,"Service.Zip",                  
        "Service.County"        ,"Technology.Type"           ,"System.Size.DC"       ,"System.Size.AC",
        "Customer.Sector"       ,"App.Received.Date"         ,"App.Complete.Date"    ,"App.Approved.Date",                
        "Decommissioned.Date"   ,"Self.Installer"            ,"Installer.Name"       ,"Installer.Phone",                  
        "Installer.City"        ,"Installer.State"           ,"Installer.Zip"        ,"CSLB.Number",                      
        "Third.Party.Owned"     ,"Third.Party.Owned.Type"    ,"Third.Party.Name"     ,"Pace.Financed",     "Electric.Vehicle",
        "NEM.Tariff"            ,"VNEM..NEM.V..NEM.Agg"      ,"Project.is.VNEM..NEM.V..NEM.Agg."
    )
    
#save a copy of CSI
    csi.old=csi
    
# select columns we want
    csi%<>%
        dplyr::select(.,cols2keep) %>%                        # selects cols in cols2keep
        group_by(Application.Id)   %>%                        # group by id
        arrange(App.Received.Date, App.Complete.Date) %>%     # arrange by received data, then by complete date
        ungroup(); dim(csi)
    
## MAKE LONG DATA FORMAT (STEPS); get sums, cumsums, by group, month, day, etc.
    
    # first get the number of adopters a day by app.complete.date and app.received.date
    #number of adopters by for app.complete.date
    n.complete <- csi %>%  
        mutate(App.Complete.Date = as.Date(App.Complete.Date))%>%  #ensure col is dates
        count(App.Complete.Date,Service.Zip, #create count for the zip-day#count by group of appcomplete and zcta5, label count nadopts
              name="n.zip.day.complete")%>%
        #summarise to get n adopters that day 
        group_by(Service.Zip, App.Complete.Date) %>%
        summarise(n.zip.day.complete = sum(n.zip.day.complete)) %>%
        ungroup();dim(n.complete)
    
    #number of adopters by for app.received.date
    n.received <- csi %>%  
        mutate(App.Received.Date = as.Date(App.Received.Date))%>%  #ensure col is dates
        count(App.Received.Date,Service.Zip, #create count for the zip-day#count by group of appcomplete and zcta5, label count nadopts
              name="n.zip.day.received")%>%
        
        #summarise to get n adopters that day 
        group_by(Service.Zip, App.Received.Date) %>%
        summarise(n.zip.day.received = sum(n.zip.day.received)) %>%
        ungroup(); dim(n.received)
    
    #
    df.haz = csi %>%  # complete(., App.Complete.Date=seq(from=min(.$App.Complete.Date),to=max(.$App.Complete.Date), by=1),fill=list(n=0))%>%
        mutate(App.Complete.Date = as.Date(App.Complete.Date))%>%#ensure col is dates
        count(App.Complete.Date,Service.Zip, #create count for the zip-day#count by group of appcomplete and zcta5, label count nadopts
              name="n.zip.day")%>%
        
        #summarise to get n adopters that day 
        group_by(Service.Zip, App.Complete.Date) %>%
        summarise(n.zip.day = sum(n.zip.day)) %>%
        ungroup() %>%
        
        #complete dates not shown (0 adopters in that zipday)
        complete(nesting(Service.Zip), App.Complete.Date = seq(min(App.Complete.Date), max(App.Complete.Date), by = "day")) %>%
        replace(is.na(.), 0) %>%
        arrange(App.Complete.Date); dim(df.haz); head(df.haz)
    
    #Append population count from census data
    temp = demog %>%
        select(ZCTA5,pop) %>%
        mutate(Service.Zip = as.integer(ZCTA5)) %>%
        mutate(pop =floor(pop*1e5)) %>%
        select(Service.Zip,pop)
    
    df.haz <-right_join(temp,df.haz) %>%
        mutate(ym=as.yearmon(App.Complete.Date)) %>% #create the yearmonth variable for the groupings/cumsum
        
        group_by(ym,Service.Zip) %>%
        arrange(App.Complete.Date) %>% #arrange matters
        mutate(cumsum.zip.month = cumsum(n.zip.day.complete) ) %>%
        mutate(n.zip.month = n()) %>%
        ungroup() %>%
        
        ### following Iyengar the number of adopters divided by the number of those 
        ### who have not adopted before
        group_by(Service.Zip) %>% #running total for the zip (cumsum) for the
        #and the hazard rate for the day
        arrange(App.Complete.Date) %>% #arrange matters
        mutate(cumsum.zip.total = cumsum(n.zip.day.complete))%>%
        mutate(haz.zip.day = n.zip.day.complete/(pop - cumsum.zip.total+1)) %>%
        ungroup() %>%
        
        ##...next two are not really useful
        # group_by(ym) %>% #cumsum for each month
        #     arrange(App.Complete.Date) %>% #arrange matters
        #     mutate(cumsum.month = cumsum(n.zip.day.complete)) %>%
        #     ungroup() %>%
        
        # group_by(App.Complete.Date ) %>% #total cumsum (i.e., running total)
        #     arrange(App.Complete.Date)%>% #arrange matters
        #     mutate(cumsum.total = cumsum(n.zip.day.complete))%>%
        #     ungroup() %>%
    
    # now we use the above to calculate what's the main thing in the graph,
    # the "average daily hazard rate for each month" (averaging across all zipcodes hr.zip.day)
    group_by(ym) %>%
        arrange(App.Complete.Date) %>%
        mutate(avg.monthly.haz = mean(haz.zip.day,na.rm=T)) %>%
        ungroup(); #View(df.haz)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #Visuals
    # 
    #     #
    #     ts.df.haz <- ts(df.haz%>%group_by(ym) %>% slice(1) %>% .$mean.haz.month,
    #                     frequency = 12, start = 2001)
    #     plot(main="Zip Code Empirical Hazard Rates",
    #          as.xts(ts.df.haz), major.format = "%Y-%m")
    #     #mean.haz.month.zip
    #     ts.df.haz <- ts(df.haz%>%
    #                         group_by(ym,Service.Zip) %>%
    #                         mutate(mean.haz.month.zip=mean(mean.haz.month))%>%
    #                         slice(1)%>%
    #                         .$mean.haz.month.zip,
    #                     frequency = 12, start = 2001)
    #     plot(main="Zip Code Empirical Hazard Rates",
    #          as.xts(ts.df.haz), major.format = "%Y-%m")
    