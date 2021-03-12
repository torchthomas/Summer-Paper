#mAKES GRAPHICS FOR THE STUFF THAT i DON'T REALLY NEED (FROM HAZARD STUFF)
#edited: 10/5/2020, new graphs added on 10/5/2020 (ctrl+f that date)
#edited: 10/6/2020, try creating avg monthly hazard rate
#edited: 11/8/2020, separate the image making from panel making;
                  # panel-making is in "code_makePanel.R"
#comment 11/8/2020, after "#CALC. cox proportional hazard model;", that's the stuff I don't care about anymore since estimation of the data begins from thereon

#######################################################################################################
#11/8/2020: WHERE THE CODE deleted at bottom should be
    #plot it and save it
    png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/avgReqAppsMon_mostrecent.png")
    plot(y = temp$mean_ym_across.zcta5,
             x=c(0,1:(length(temp$mean_ym_across.zcta5)-1) ),
             xlab="Months",ylab="Avg. number of App.s Received",
             main = "Avg. daily requested installations per month",
             type="l")
    dev.off()
    
    #plot it and save it
    png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/avgComAppsMon_mostrecent.png")
    plot( temp2$mean_ym_across.zcta5, #x=c(1:length(temp2$mean_ym_across.zcta5)),
              xlab="Months",ylab="Avg. number of App.s Completed",
              main = "Avg. daily completed installations per month",
              type="l", col = "blue")
    dev.off()
    
    #both graphs in 1
    plot.new()
    png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/avg_CompReq_Mon_mostrecent.png")
    plot( temp2$mean_ym_across.zcta5, #x=c(1:length(temp2$mean_ym_across.zcta5)),
          xlab="Months",ylab="Avg. number of App.s Completed",
          main = "Avg. daily completed installations per month",
          type="l", col = "blue")
    lines(y = temp$mean_ym_across.zcta5,x=c(0,1:(length(temp$mean_ym_across.zcta5)-1) ),
          xlab="Months",ylab="Avg. number of App.s Received",
          main = "Avg. daily requested installations per month",
          type="l", col="red")
    # abline(v = 137, col="black", lwd=.25, lty=2)
    legend("top", legend=c("Requests", "Completed"),
           col=c("red", "blue"), # box.lty = 0,
           lty=c(1,1), cex=0.8,
           title="Legend", text.font=3.5#, bg='lightblue'
    )
    # annotation_custom(grob = textGrob("Extra text.  Read all about it"), xmin = 2, xmax = 2, ymin = -4.5, ymax = -4.55)
    dev.off()
    
    plot.new()
    png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/daysSinceLastInstall.png")
    hist(as.numeric(as.Date(yyy$App.Complete.Date.x) - as.Date(yyy$App.Complete.Date.y)),
         xlim=c(0,500),
         ylim=c(0,30000),
         main="Time since last installation",
         xlab="Days since last installation",
         # breaks="Scott"
         breaks = seq(from=0, to=3000, by = 7.5)
    )
    dev.off()
    #log transform this variable (time since last install)
    plot.new()
    png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/log_daysSinceLastInstall.png")
    hist(log(as.numeric(as.Date(yyy$App.Complete.Date.x) - as.Date(yyy$App.Complete.Date.y))),
         xlim=c(0,8),
         main="Log time since last installation",
         xlab="Log days since last installation",
         breaks=seq(0,8,by = .32)
    )
    dev.off()
    
    
    
    #end images based on "code_makePanel.R"
    
    
    
#CALC. cox proportional hazard model; survival:::coxph;  cox.mod1 <- coxph( Surv(timeSinceLastInstall.ZCTA5, status) ~ strata(ZCTA5) + base.zip.day - 1,
    cox.mod1 <- coxph( Surv(timeSinceLastInstall.ZCTA5, status) ~ base.zip.day + - 1,
                       data = as.data.frame(install.data))
    cox.mod1.fit <- survfit(cox.mod1)
    
    png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/cox1.png")
        autoplot(cox.mod1.fit,
                 xlab="Days until adoption given one adopter in the ZCTA5",
                 ylab="S*(t)",
             main="Cox Proportional Survival Curve | Adopter in ZCTA5")
    dev.off()
    
    #we stratify the above by income class
    temp = install.data %>%
        mutate(incomeStrata = as.factor(1*(med_inc_HH >= 6.034)) ) #%>%
        # mutate(incomeStrata = factor(incomeStrata)); 
        dim(temp) #dim: 69699 x 123
    cox.mod2 <- coxph( Surv(timeSinceLastInstall.ZCTA5, status) ~  strata(incomeStrata) - 1,
                       data = as.data.frame(temp))
    cox.mod2.fit <- survfit(cox.mod2)
    png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/cox2.png")
        ggsurvplot(
            surv_fit(cox.mod2,data=temp), 
            data = temp, 
            title="Cox Proportional Survival Curve | Adopter in ZCTA5",
            # size = 1,                     # change line size
            palette = c("#E7B800", "#2E9FDF"),# custom color palettes
            # conf.int = TRUE,              # Add confidence interval
            # pval = TRUE,                  # Add p-value
            risk.table = TRUE,              # Add risk table
            risk.table.col = "strata",      # Risk table color by groups
            legend.labs = c("Lower50", "Upper50"),    # Change legend labels
            risk.table.height = 0.25, # Useful to change when you have multiple groups
            ggtheme = theme_bw(),
            legend.title = "Median Income",
            risk.table.title="People waiting to adopt"# Change ggplot2 theme
        )
    dev.off()
######### 11/8/2020 code put into code_makePanel.R (the graphic-making portion was returned to here, but this is a backup
         #I forget to change some components needed for the graphics )
    # ############.......MAKE INSTALLATION DATA THAT'LL BE THE MAJOR/FINAL DATASET FOR PAPER#############
    # # 11/8/2020, added the part from code_smallerGraphics that actually organizes the panel I need to xfer to stata
    # #Using app.received.date get average requested installations per month
    # panel.receivedDate = csi.old %>%  # complete(., App.Complete.Date=seq(from=min(.$App.Complete.Date),to=max(.$App.Complete.Date), by=1),fill=list(n=0))%>%
    #     mutate(App.Received.Date = as.Date(App.Received.Date))%>%#ensure col is dates
    #     count(App.Received.Date,Service.Zip, #create count for the zip-day#count by group of appcomplete and zcta5, label count nadopts
    #           name="n.zip.day")%>%
    #     #summarise to get n adopters that day
    #     group_by(Service.Zip, App.Received.Date) %>%
    #     summarise(n.zip.day = sum(n.zip.day)) %>%
    #     ungroup() %>%
    #     mutate(ZCTA5=as.integer(Service.Zip))%>%
    #     mutate(ym=zoo:::as.yearmon(App.Received.Date)) %>% #create the yearmonth variable for the groupings/cumsum
    #     arrange(App.Received.Date); dim(panel.receivedDate); head(panel.receivedDate)
    # 
    # temp = panel.receivedDate %>%
    #     group_by(ym,ZCTA5) %>%
    #     mutate(ym.zcta5.sum  = sum(n.zip.day,na.rm = T)) %>%
    #     ungroup() %>%
    #     group_by(ym) %>%
    #     summarise(mean_ym_across.zcta5 = mean(ym.zcta5.sum,na.rm=T))
    # #plot it and save it
    # png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/avgReqAppsMon_mostrecent.png")
    # plot(y = temp$mean_ym_across.zcta5,x=c(0,1:(length(temp$mean_ym_across.zcta5)-1) ),
    #      xlab="Months",ylab="Avg. number of App.s Received",
    #      main = "Avg. daily requested installations per month",
    #      type="l")
    # dev.off()
    # 
    # # l1=length(temp$mean_ym_across.zcta5)
    # #Using App.Complete.Date get average requested installations per month
    # panel.completeDate = csi.old %>%  # complete(., App.Complete.Date=seq(from=min(.$App.Complete.Date),to=max(.$App.Complete.Date), by=1),fill=list(n=0))%>%
    #     mutate(App.Complete.Date = as.Date(App.Complete.Date))%>%#ensure col is dates
    #     count(App.Complete.Date,Service.Zip, #create count for the zip-day#count by group of appcomplete and zcta5, label count nadopts
    #           name="n.zip.day")%>%
    #     #summarise to get n adopters that day
    #     group_by(Service.Zip, App.Complete.Date) %>%
    #     summarise(n.zip.day = sum(n.zip.day)) %>%
    #     ungroup() %>%
    #     mutate(ZCTA5=as.integer(Service.Zip))%>%
    #     # dplyr::select(-ZCTA5) %>%
    #     mutate(ym=zoo:::as.yearmon(App.Complete.Date)) %>% #create the yearmonth variable for the groupings/cumsum
    #     arrange(App.Complete.Date); dim(panel.completeDate); head(panel.completeDate)
    # 
    # #USED IN "code_smallerGraphics.R"
    # temp2 = panel.completeDate %>%
    #     group_by(ym,ZCTA5) %>%
    #     mutate(ym.zcta5.sum  = sum(n.zip.day,na.rm = T)) %>%
    #     ungroup() %>%
    #     group_by(ym) %>%
    #     summarise(mean_ym_across.zcta5 = mean(ym.zcta5.sum,na.rm=T))
    # # temp2 = temp$mean_ym_across.zcta5[1:l1]
    # 
    # #plot it and save it
    # png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/avgComAppsMon_mostrecent.png")
    # plot( temp2$mean_ym_across.zcta5, #x=c(1:length(temp2$mean_ym_across.zcta5)),
    #       xlab="Months",ylab="Avg. number of App.s Completed",
    #       main = "Avg. daily completed installations per month",
    #       type="l", col = "blue")
    # dev.off()
    # 
    # 
    # #10/6/2020
    # # #try creating avg monthly hazard rate
    # # #dim csi: 94474    35
    # # m3 = csi %>% 
    # #     mutate(lag1.rec = lag(App.Received.Date,n=1),
    # #            lag1.com = lag(App.Complete.Date,n=1))
    # 
    # #USED IN "code_smallerGraphics.R"
    # #both graphs in 1
    # plot.new()
    # png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/avg_CompReq_Mon_mostrecent.png")
    # plot( temp2$mean_ym_across.zcta5, #x=c(1:length(temp2$mean_ym_across.zcta5)),
    #       xlab="Months",ylab="Avg. number of App.s Completed",
    #       main = "Avg. daily completed installations per month",
    #       type="l", col = "blue")
    # lines(y = temp$mean_ym_across.zcta5,x=c(0,1:(length(temp$mean_ym_across.zcta5)-1) ),
    #       xlab="Months",ylab="Avg. number of App.s Received",
    #       main = "Avg. daily requested installations per month",
    #       type="l", col="red")
    # # abline(v = 137, col="black", lwd=.25, lty=2)
    # legend("top", legend=c("Requests", "Completed"),
    #        col=c("red", "blue"), # box.lty = 0,
    #        lty=c(1,1), cex=0.8,
    #        title="Legend", text.font=3.5#, bg='lightblue'
    # )
    # # annotation_custom(grob = textGrob("Extra text.  Read all about it"), xmin = 2, xmax = 2, ymin = -4.5, ymax = -4.55)
    # dev.off()
    # 
    # # get last date from non-contemporaneous (by date) member in group (date and zcta5)
    # #try 2
    # temp = csi.long%>% 
    #     rename(., Service.Zip= ZCTA5) %>%
    #     arrange(Service.Zip,App.Complete.Date) %>%
    #     mutate(App.Complete.Date = as.Date(App.Complete.Date),
    #            rowNum = row_number())
    # yyy = temp %>%
    #     left_join(temp, by = "Service.Zip") %>%
    #     # discard all the out-of-scope dates
    #     mutate(App.Complete.Date.y = ifelse(App.Complete.Date.y < App.Complete.Date.x,
    #                                         App.Complete.Date.y, NA)) %>%
    #     # we need to include row number here to preserve all rows in the original
    #     group_by(Service.Zip, App.Complete.Date.x, rowNum.x) %>% #dim: 10,691,355       35
    #     # na.rm = TRUE handles all the missing values removed in the previous mutate
    #     summarise(App.Complete.Date.y = max(App.Complete.Date.y, na.rm = TRUE), .groups = 'drop') %>% #dim: 58,957 x 5
    #     # summarise may return numeric type rather than date type - convert back
    #     mutate(App.Complete.Date.y = as.Date(App.Complete.Date.y, origin = "1970-01-01")) %>%
    #     # rename to output
    #     # dplyr:::select(.,
    #     #        App.Complete.Date = App.Complete.Date.x,
    #     #        Last.Unique.Date.In.ZCTA5 = App.Complete.Date.y) %>%
    #     ungroup(); head(yyy);dim(yyy);class(yyy); str(yyy)  #dim: 58,957 x 3
    # 
    # #make timeSinceLastZCTA5adoption
    # xx = as.numeric(yyy$App.Complete.Date.y);xx
    # filler= as.numeric(yyy$App.Complete.Date.x); filler
    # xx[which(!is.finite(yyy$App.Complete.Date.y))] = filler[which(!is.finite(xx))]; xx; length(xx)
    # xx[which(!is.finite(xx))]<-0;xx; length(xx)
    # xx[which(xx==0)]<-filler;head(xx)#integer form of the final (and correct) dates for last installed PV system 
    # 
    # yyy$App.Complete.Date.y=xx;# head(yyy); view(yyy);class(yyy);str(yyy);dim(yyy)
    # yyy %<>% mutate(App.Complete.Date.y = as.Date(
    #     App.Complete.Date.y,origin = "1970-01-01"))
    # 
    # #USED IN "code_smallerGraphics.R"
    # plot.new()
    # png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/daysSinceLastInstall.png")
    # hist(as.numeric(as.Date(yyy$App.Complete.Date.x) - as.Date(yyy$App.Complete.Date.y)),
    #      xlim=c(0,500),
    #      ylim=c(0,30000),
    #      main="Time since last installation",
    #      xlab="Days since last installation",
    #      # breaks="Scott"
    #      breaks = seq(from=0, to=3000, by = 7.5)
    # )
    # dev.off()
    # #log transform this variable (time since last install)
    # plot.new()
    # png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/log_daysSinceLastInstall.png")
    # hist(log(as.numeric(as.Date(yyy$App.Complete.Date.x) - as.Date(yyy$App.Complete.Date.y))),
    #      xlim=c(0,8),
    #      main="Log time since last installation",
    #      xlab="Log days since last installation",
    #      breaks=seq(0,8,by = .32)
    # )
    # dev.off()
    # 
    # ##Finally, we join temp and yyy by row number
    # #rename yyy$rowNum.x
    # yyy %<>% rename(rowNum = rowNum.x)
    # #join temp and yyy; results for correct rows 
    # install.data = left_join(temp, yyy, by = "rowNum"); dim(install.data)
    # # > sum(install.data$Service.Zip.x==install.data$Service.Zip.y)
    # # [1] 
    # ##Finish variable naming (App.Complete.Date.y, Service.Zip.x) 
    # #drop redundant rows (app.complete.x, rowNum, .groups) and poorly defined (time2adopt)
    # #define new variable timeSinceLastInstall.ZCTA5 (y from eqn 7 [from sbg2010])
    # install.data %<>%
    #     rename(Service.Zip = Service.Zip.x, 
    #            Last.noncontemp.Adoption.Date = App.Complete.Date.y) %>%
    #     dplyr::select(.,-App.Complete.Date.x, -Service.Zip.y, -.groups) %>%
    #     mutate(timeSinceLastInstall.ZCTA5 = as.numeric(App.Complete.Date) -
    #                as.numeric(Last.noncontemp.Adoption.Date) );dim(install.data) #dim: 56351 x 178
##########################################
{
#added 10/5/2020
    # #Get a monthly avg cost per watt
    # temp.cost = csi %>%
    #     mutate(App.Received.Date = as.Date(App.Received.Date))%>%#ensure col is dates
    #     mutate(ym=zoo:::as.yearmon(App.Received.Date)) %>% #create the yearmonth variable for the groupings/cumsum
    #     group_by(ym) %>%
    #     summarise(mon.cost.per.watt = mean(Cost.Watt,na.rm=T)); head(temp.cost)
        
# #added 10/6/2020
#     my.ts <- ts(csi.dat$Total.System.Cost, start=c(2007, 1), end=c(2017, 12), frequency=12)    
#     tsp = attributes(my.ts)$tsp
#     dates = seq(as.Date("2007-01-01"), by = "month", along = my.ts)
#     plot(my.ts, xaxt = "n", main= "Plotting outcome over time",
#          ylab="outcome", xlab="time")
#     axis(1, at = seq(tsp[1], tsp[2], along = my.ts), labels = format(dates, "%Y-%panel.receivedDate"))
#     abline(v=2012, col="blue", lty=2, lwd=2)
    
######### CRUCIAL FOR MAKING A CONTRIBUTION
    #next, a cox NONproportional hazard see https://cran.r-project.org/web/packages/coxphw/coxphw.pdf
    # temp.no.firstZcta5Installers = temp[-which(temp$timeSinceLastInstall.ZCTA5==0),];dim(temp.no.firstZcta5Installers)
    #     #dim: 68,705 x 123
    # temp.no.firstZcta5Installers %<>% 
    #     dplyr::select(., timeSinceLastInstall.ZCTA5, medbeds, Service.Zip, incomeStrata,status) %>%
    #     as.data.frame(.) %>%
    #     mutate(Service.Zip = factor(Service.Zip)); dim(temp.no.firstZcta5Installers) #dim: 68,705 x 4
    # options(na.action = "omit" )
    # cox.mod3 <- coxphw::coxphw( 
    #     Surv(timeSinceLastInstall.ZCTA5, 
    #          status) ~ medbeds+ strata(Service.Zip) - 1,
    #     template = c("ARE"), #template = c("AHR"), 
    #     robust=T,
    #     jack=F,
    #                    data = temp.no.firstZcta5Installers)
    # cox.mod3.fit <- survfit(cox.mod3)
    # png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/cox3.png")
    # ggsurvplot(
    #     surv_fit(cox.mod3,data=temp.no.firstZcta5Installers),
    #     data = temp.no.firstZcta5Installers,
    #     title="Cox Proportional Survival Curve | Adopter in ZCTA5",
    #     # size = 1,                     # change line size
    #     palette = c("#E7B800", "#2E9FDF"),# custom color palettes
    #     # conf.int = TRUE,              # Add confidence interval
    #     # pval = TRUE,                  # Add p-value
    #     risk.table = TRUE,              # Add risk table
    #     risk.table.col = "strata",      # Risk table color by groups
    #     legend.labs = c("Lower50", "Upper50"),    # Change legend labels
    #     risk.table.height = 0.25, # Useful to change when you have multiple groups
    #     ggtheme = theme_bw(),
    #     legend.title = "Median Income",
    #     risk.table.title="People waiting to adopt"# Change ggplot2 theme
    # )
    # dev.off()
    # # 
# ts.df.haz <- ts(install.data$System.Size.AC)
    # plot(main="system size over time (AC)",
    #     ylab ="time",
    #     xlab="Date",
    #     as.xts(ts.df.haz), major.format = "%Y-%panel.receivedDate")
    # 
    # plot()
}
#######################################################################################################
#empirical hazard rate (I THINK MISTAKES ARE PRESENT!!!); plot this 
    # ts.df.haz <- ts(df.haz%>%
    #                     filter(!is.nan(avg.monthly.haz.pop), #avg.monthly.haz.to.re, avg.monthly.haz.oho, avg.monthly.haz.pop
    #                            !is.na(avg.monthly.haz.pop)) %>%  #avg.monthly.haz.pop,avg.monthly.haz.oho,avg.monthly.haz.to.re
    #                     # group_by(App.Complete.Date,ZCTA5) %>% slice(1) %>% ungroup() %>%
    #                     group_by(ym) %>% slice(1) %>%
    #                     .$avg.monthly.haz.pop,
    #                 frequency = 12, start = 2001)
    # ### plot daily probability of adoption........RENAME HAZ1
    # png(filename="/Users/tdevine/Box Sync/research/solar/Analysis/surivalPlots/dayprob.png")
    # plot(main="Zip Code Empirical Hazard Rates | Adopter in ZCTA5",
    #      ylab = "Daily household probability of adoption (1E-5)",
    #      xlab="Date",
    #      as.xts(ts.df.haz)*1e5, major.format = "%Y-%panel.receivedDate")
    # zoo::plot.zoo(as.xts(ts.df.haz)*1e4, screens = 1,
    #               xlab="Date",
    #               ylab="Household probability of adoption (1E-4)", las=2,
    #               main="Monthly average of Daily Probability of Adoption | Adopter in ZCTA5")
    # grid()
    # dev.off()

###DELETED, aug 27, 8 27 2020
    # #try 1 fail
    # fn.get.noncontemp.date = function(datevec){
    #     unique.datevec=unique(datevec)
    #     
    #     return(unique.datevec[length(unique.datevec)-1])
    # }
    # temp <- csi.old %>%
    #     arrange(Service.Zip,App.Complete.Date); dim(temp) #dim: 76200, 68
    # temp$newcol
###DELETED, aug 30, 8 27 2020
    # autoplot(cox.mod2.fit,
    #              xlab="time",
    #              ylab="S*(t)",
    #              palette = c("Set2"),
    #              # legLabs = c("Lower 50% by HH median income","Upper 50% by HH median income"),
    #              # survLineSize = c(1,22),
    #              main="Cox Proportional Survival Curve|Adopter in ZCTA5 by lower/upper 50%"
    #          )+
    #          theme(text = element_text(size=9))+
    #     scale_linetype_manual(values = c('solid', 'solid'), 
    #                           labels = c('Poor', 'Rich') ) 
    #
    # autoplot(cox.mod2.fit, surv.linetype = 'blank',
    #          conf.int=FALSE, pVal=TRUE, pX=800,
    #          palette = c("Set2")
    # ) +
    #     geom_step(aes(linetype=strata) ) +
    #     scale_linetype_manual(values = c('solid', 'solid'), 
    #                           labels = c('Poor', 'Rich') ) +
    #     scale_color_manual(values = c('red', 'green'), 
    #                        labels = c('Poor', 'Rich') )
    