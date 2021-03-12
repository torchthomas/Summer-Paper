# SCRIPT TO CREATE TABLES SUMMER PAPER 2020
# AUTHOR: THOMAS DEVINE
# currdir: "C:/Users/tdevine/Desktop/2020 Summer Paper/Analysis"
# CREATED: (when I made my code neat) 8.20.2020
#edit: 8.26.2020, small changes to l29 and l27 

# TABLE 1
    # CALLED BY: code_totalcensus.R (copies before code_totalcensus_8-20_copy.R DO NOT call this script)
    # GOAL:      replicating "TABLE 1" in bg2012
    # OUTPUTDIR: setwd("/Users/tdevine/Box Sync/research/solar/Analysis/tables/")
    # OUTPUT:    "tab1.tex", a LaTeX file 
{
    #FORMAT
    # 1. data per line in table 1
    # 2. calculations per line in table 1
    
    tab1.data1 = csi %>%
        group_by(Service.Zip) %>% 
        count() %>%
        ungroup()
    tab1.data2= csi %>%  arrange(Service.Zip) %>% 
        dplyr::select(Application.Id, Service.Zip, App.Complete.Date, System.Size.AC, System.Size.DC) %>% 
        group_by(Service.Zip) %>% 
        summarise(installedmw=sum(System.Size.DC,na.rm=T)/1000)
    tab1.data3 <- data.frame(ZCTA5  = demog$ZCTA5,pop            = 100*demog$pop/1e5)
    tab1.data4 <- data.frame(ZCTA5  = demog$ZCTA5,hhsize         = demog$size_HH_tot)
    tab1.data5 <- data.frame(ZCTA5  = demog$ZCTA5,medincome      = demog$med_inc_HH)
    tab1.data6 <- data.frame(ZCTA5  = demog$ZCTA5,popmale        = 100*(demog$Male/1e5*1      / (1e5*demog$pop)))
    tab1.data7 <- data.frame(ZCTA5  = demog$ZCTA5,popwhite       = 100*(demog$whitepop  / (1e5*demog$pop)))
    tab1.data8 <- data.frame(ZCTA5  = demog$ZCTA5,collegedegrees = 100*(demog$collegedegrees / (1e5*demog$pop)))
    tab1.data9 <- data.frame(ZCTA5  = demog$ZCTA5,age2045        = 100*(demog$age2045   / (1e5*demog$pop)))
    
    
    l1 = list("Mean" = mean(tab1.data1$n),
              "Std. dev." = sd(tab1.data1$n),
              "Min" = min(tab1.data1$n),
              "Max" = max(tab1.data1$n),
              "N" = dim(tab1.data1)[1]
    )%>%as.data.frame(.);#l1
    l2 = list("Mean"      = mean(tab1.data2$installedmw),
              "Std. dev." = sd(tab1.data2$installedmw),
              "Min"       = min(tab1.data2$installedmw),
              "Max"       = max(tab1.data2$installedmw),
              "N"         = dim(tab1.data2)[1]
    )%>%as.data.frame(.);#l2
    l3 = list("Mean"      = mean(demog$pop,na.rm=T),
              "Std. dev." = sd(demog$pop,na.rm=T),
              "Min"       = demog$pop[which.min(demog$pop)],
              "Max"       = demog$pop[which.max(demog$pop)],
              "N"         = dim(!is.na(demog))[1]
    )%>%as.data.frame(.);#l3
    l4 = list("Mean"      = mean(tab1.data4$hhsize,na.rm=T),
              "Std. dev." = sd(tab1.data4$hhsize,na.rm=T),
              "Min"       = tab1.data4$hhsize[which.min(tab1.data4$hhsize)],
              "Max"       = tab1.data4$hhsize[which.max(tab1.data4$hhsize)],
              "N"         = dim(!is.na(tab1.data4))[1]
    )%>%as.data.frame(.);##l4
    l5 = list("Mean"      = mean(tab1.data5$medincome,na.rm=T),
              "Std. dev." = sd(tab1.data5$medincome,na.rm=T),
              "Min"       = tab1.data5$medincome[which.min(tab1.data5$medincome)],
              "Max"       = tab1.data5$medincome[which.max(tab1.data5$medincome)],
              "N"         = dim(!is.na(tab1.data5))[1]
    )%>%as.data.frame(.);##l5
    l6 = list("Mean"      =mean(demog$Male/(demog$Male+demog$Female)*100,na.rm=T),
              "Std. dev." = sd((demog$Male/(demog$Male+demog$Female)*100),na.rm=T),
              "Min"       = (demog$Male/(demog$Male+demog$Female)*100)[which.min(demog$Male/(demog$Male+demog$Female))],
              "Max"       = (demog$Male/(demog$Male+demog$Female)*100)[which.max(demog$Male/(demog$Male+demog$Female))],
              "N"         = dim(!is.na(tab1.data6))[1]
    )%>%as.data.frame(.);#l6
    
    l7 = list("Mean"      = mean(tab1.data7$popwhite,na.rm=T),
              "Std. dev." = sd(tab1.data7$popwhite,na.rm=T),
              "Min"       = tab1.data7$popwhite[which.min(tab1.data7$popwhite)],
              "Max"       = tab1.data7$popwhite[which.max(tab1.data7$popwhite)],
              "N"         = sum(!is.na(tab1.data7$popwhite>0))
    )%>%as.data.frame(.);##l7
    
    tempsd=sum((demog$collegedegrees/(demog$population_dec) - (sum(demog$collegedegrees,na.rm=T)/sum(demog$population_dec,na.rm=T)))^2/(
        length(demog$collegedegrees)-1),na.rm=T)
    l8 = list("Mean"      = mean(demog$collegedegrees/demog$pop/1e5*100,na.rm=T),
              "Std. dev." = tempsd,
              "Min"       = demog$collegedegrees[which.min(demog$collegedegrees)]/demog$pop[which.max(demog$collegedegrees)]/1e5*100,
              "Max"       = demog$collegedegrees[which.max(demog$collegedegrees)]/demog$pop[which.max(demog$collegedegrees)]/1e5*100,
              "N"         = sum(!is.na(demog$collegedegrees>0))
    )%>%
        as.data.frame(.);l8
    l9 = list("Mean"      = mean(tab1.data9$age2045,na.rm=T),
              "Std. dev." = sd(tab1.data9$age2045,na.rm=T),
              "Min"       = tab1.data9$age2045[which.min(tab1.data9$age2045)],
              "Max"       = tab1.data9$age2045[which.max(tab1.data9$age2045)],
              "N"         = sum(!is.na(tab1.data9$age2045>0))
    )%>%as.data.frame(.);##l9
    
    
    tab1.data10 <- data.frame(ZCTA5 = demog$ZCTA5,age65plus      = (demog$age65plus / (1e5*demog$pop)))
    tab1.data11 <- data.frame(ZCTA5 = demog$ZCTA5,workcar        = (demog$workcar   / (1e5*demog$pop)))
    tab1.data12 <- data.frame(ZCTA5 = demog$ZCTA5,carpool        = (demog$carpool   / (1e5*demog$pop)))
    tab1.data13 <- data.frame(ZCTA5 = demog$ZCTA5,pubtransit     = (demog$pubtransit/ (1e5*demog$pop)) )
    tab1.data14 <- data.frame(ZCTA5 = demog$ZCTA5,workhome       = (demog$workhome  / (1e5*demog$pop)) )
    
    tempsd=sum((demog$age65plus/(demog$population_dec) -
                    (sum(demog$age65plus,na.rm=T)/sum(demog$population_dec,na.rm=T)))^2/(
                        length(demog$age65plus)-1),na.rm=T)
    l10 = list("Mean"     = 100*(sum(demog$age65plus,na.rm=T)/sum(demog$population_dec,na.rm=T)),
               "Std. dev." = 100*tempsd,
               "Min"       = tab1.data10$age65plus[which.min(tab1.data10$age65plus)],
               "Max"       = tab1.data10$age65plus[which.max(tab1.data10$age65plus)],
               "N"         = sum(!is.na(tab1.data10$age65plus>0))
    )%>%as.data.frame(.);##l10    
    
    tempsd= sum((demog$workcar/(demog$population_dec) -
                     (sum(demog$workcar,na.rm=T)/sum(demog$population_dec,na.rm=T)))^2/(
                         length(demog$workcar)-1),na.rm=T)
    l11 = list("Mean"      = 100* mean(demog$workcar/demog$pop/1e5,na.rm=T),
               "Std. dev." = 100*tempsd,
               "Min"       = 100*demog$workcar[which.min(demog$workcar)]/demog$pop[which.min(demog$pop)]/1e5,
               "Max"       = 100*demog$workcar[which.max(demog$workcar)]/demog$pop[which.max(demog$pop)]/1e5,
               "N"         = sum(!is.na(demog$workcar>0))
    )%>%as.data.frame(.);##l11   
    tempsd= sum((demog$carpool/(demog$population_dec) -
                     (sum(demog$carpool,na.rm=T)/sum(demog$population_dec,na.rm=T)))^2/(
                         length(demog$carpool)-1),na.rm=T)
    l12 = list("Mean"      = 100*(sum(demog$carpool,na.rm=T)/sum(demog$population_dec,na.rm=T)),
               "Std. dev." = 100*tempsd,
               "Min"       = 100*demog$carpool[which.min(demog$carpool)]/demog$pop[which.min(demog$pop)]/1e5,
               "Max"       = 100*demog$carpool[which.max(demog$carpool)]/demog$pop[which.max(demog$pop)]/1e5,
               "N"         = sum(!is.na(demog$carpool>0))
    )%>%as.data.frame(.);##l12
    
    l13 = list("Mean"     = mean(demog$pubtransit/demog$pop/1e5*100,na.rm=T),
               "Std. dev." = sd(demog$pubtransit/demog$pop/1e5*100,na.rm=T),
               "Min"       = min(demog$pubtransit/demog$pop/1e5*100,na.rm=T),
               "Max"       = max(demog$pubtransit/demog$pop/1e5*100,na.rm=T),
               "N"         = sum(!is.na(demog$pubtransit>0))
    )%>%as.data.frame(.);##l13
    
    tempsd= sum((demog$workhome/(demog$population_dec) -
                     (sum(demog$workhome,na.rm=T)/sum(demog$population_dec,na.rm=T)))^2/(
                         length(demog$workhome)-1),na.rm=T)
    l14 = list("Mean"     = (sum(demog$workhome,na.rm=T)/sum(demog$population/1e5,na.rm=T)),
               "Std. dev." =100* tempsd,
               "Min"       = min((sum(demog$workhome,na.rm=T)/sum(demog$population_dec,na.rm=T)),na.rm=T),
               "Max"       = max((sum(demog$workhome,na.rm=T)/sum(demog$population_dec,na.rm=T)),na.rm=T),
               "N"         = sum(!is.na(demog$workhome/demog$population))
    )%>%
        as.data.frame(.);l14
    
    # tab1.data15 <- data.frame(ZCTA5 = demog$ZCTA5, t30mincommute  = (demog$t30mincommute / (1e5*demog$pop)))
    tempsd=sum((demog$t30mincommute/(demog$population_dec) -
                    (sum(demog$t30mincommute,na.rm=T)/sum(demog$population_dec,na.rm=T)))^2/(
                        length(demog$population_dec)-1),
               na.rm=T)
    tempt=(demog$t30mincommute); tempt[is.na(tempt)]<-0
    tempp=(demog$pop);    tempp[is.na(tempp)]<-0
    l15 = list("Mean"     =  mean(demog$t30mincommute/demog$pop,na.rm=T)/1e5,
               "Std. dev." = tempsd,
               "Min"       = 100*min(tempt/tempp,na.rm = T),
               "Max"       = 100*min(sum(demog$t30mincommute,na.rm=T)/sum(demog$pop,na.rm=T)/1e5),
               "N"         = sum(!is.na(demog$t30mincommute))
    )%>%as.data.frame(.);l15
    #l16 I don't have (drive hybrids)
    
    
    # tab1.data17 <- data.frame(ZCTA5 = demog$ZCTA5,count_OwnOcc = (demog$count_OwnOcc))
    tab1.data18 <- data.frame(ZCTA5 = demog$ZCTA5,medhomeval_ownOcc     = demog$medhomeval_ownOcc)
    tab1.data21 <- data.frame(ZCTA5 = demog$ZCTA5,homeval.0kto50k  = demog$homeval.0kto50k)
    tab1.data22 <- data.frame(ZCTA5 = demog$ZCTA5,homeval.50kto90k = demog$homeval.50kto90k)
    
    #dec, then acs
    # sum(demog$pop.house.tenure.total-demog$pop.house.tenure.rent);sum(demog$pop.housed);
    # table_contents = c("pop.house.tenure.total           = H0040001", #Total - TENURE - Occupied housing units
    #                    "pop.house.tenure.ownocc.loan     = H0040002",
    #                    "pop.house.tenure.ownocc.loanfree = H0040003",
    #                    "pop.house.tenure.rent            = H0040004",
    #                    "Male_dec                         = P0120002",
    #                    "Female_dec                       = P0120026"
    # #count owner occupied units(houses)
    # "occ.h.total=B25008_001",
    # "occ.h.ownocc=B25008_002",
    # "occ.h.not.ownocc=B25008_003",
    # #Household Type by Tenure (hbt);  Universe: Universe: Households
    # "hbt.total         =B11012_001", #includes renters 
    # "hbt.tfhmcoo       =B11012_004", #family,married,ownocc
    # "hbt.tfhofmaleoo   =B11012_008", #family,male-lead,own.occ
    # "hbt.tfhoffemaleoo =B11012_011", #family,female-lead,own.occ
    # "hbt.tnfoo         =B11012_014"  #non family, own.occ
    # mean(demog$pop.house.tenure.total-demog$pop.house.tenure.rent)/1000
    # mean(demog$pop.house.tenure.ownocc.loan +demog$pop.house.tenure.ownocc.loanfree)/1000
    # mean(demog$hbt.tnfoo+demog$hbt.tfhoffemaleoo+demog$hbt.tfhofmaleoo+
    #          demog$hbt.tfhmcoo)/1000
    temp.Ownocc = (demog$pop.house.tenure.total - demog$pop.house.tenure.rent)
    l17 = list("Mean"     = mean(temp.Ownocc,na.rm=T)/1000,
               "Std. dev." = sd(temp.Ownocc,na.rm=T)/1000,
               "Min"       = min(temp.Ownocc,na.rm=T)/1000,
               "Max"       = max(temp.Ownocc,na.rm=T)/1000,
               "N"         = sum(!is.na(temp.Ownocc),na.rm=T )
    )%>%
        as.data.frame(.);l17
    
    
    l18 = list("Mean"     = mean(demog$medhomeval_ownOcc,na.rm=T),
               "Std. dev." = sd(demog$medhomeval_ownOcc,na.rm=T),
               "Min"       = demog$medhomeval_ownOcc[which.min(demog$medhomeval_ownOcc)],
               "Max"       = demog$medhomeval_ownOcc[which.max(demog$medhomeval_ownOcc)],
               "N"         = sum(!(is.na(demog$medhomeval_ownOcc>0) ))
    )%>%as.data.frame(.);l18
    #dont have l19
    #dont have l20
    l21 = list("Mean"     = mean(demog$homeval.0kto50k/demog$homeval.totalCount,na.rm=T)*100,
               "Std. dev." = sd(demog$homeval.0kto50k/demog$homeval.totalCount,na.rm=T)*100,
               "Min"       = 100*min((tab1.data21$homeval.0kto50k/demog$homeval.totalCount),na.rm=T),
               "Max"       = 100*max((tab1.data21$homeval.0kto50k/demog$homeval.totalCount),na.rm=T),
               "N"         = sum(!is.na(demog$homeval.0kto50k>0))
    )%>%as.data.frame(.);#l21
    l22 = list("Mean"     = mean(demog$homeval.50kto90k/demog$homeval.totalCount,na.rm=T)*100,
               "Std. dev." = sd(demog$homeval.50kto90k/demog$homeval.totalCount,na.rm=T)*100,
               "Min"       = (demog$homeval.175kto400k/demog$homeval.totalCount)[which.min(demog$homeval.50kto90k/demog$homeval.totalCount)]*100,
               "Max"       = (demog$homeval.175kto400k/demog$homeval.totalCount)[which.max(demog$homeval.50kto90k/demog$homeval.totalCount)]*100,
               "N"         = sum(!is.na(demog$homeval.50kto90k>0))
    )%>%as.data.frame(.);##l22
    
    
    tab1.data23 <- data.frame(ZCTA5 = demog$ZCTA5,homeval.90kto175k  = demog$homeval.90kto175k)
    tab1.data24 <- data.frame(ZCTA5 = demog$ZCTA5,homeval.175kto400k = demog$homeval.175kto400k)
    tab1.data25 <- data.frame(ZCTA5 = demog$ZCTA5,homeval.400kplus   = demog$homeval.400kplus)
    
    l23 = list("Mean"      = mean(demog$homeval.90kto175k/demog$homeval.totalCount,na.rm=T)*100,
               "Std. dev." = sd(demog$homeval.90kto175k/demog$homeval.totalCount,na.rm=T)*100,
               "Min"       = 100*min((demog$homeval.90kto175k/demog$homeval.totalCount),na.rm=T),
               "Max"       = 100*max((demog$homeval.90kto175k/demog$homeval.totalCount),na.rm=T),
               "N"         = sum(!is.na(demog$homeval.90kto175k>0))
    )%>%as.data.frame(.);l23
    l24 = list("Mean"      = mean(demog$homeval.175kto400k/demog$homeval.totalCount,na.rm=T)*100,
               "Std. dev." = sd(demog$homeval.175kto400k/demog$homeval.totalCount,na.rm=T)*100,
               "Min"       = (demog$homeval.175kto400k/demog$homeval.totalCount)[which.min(
                   demog$homeval.175kto400k/demog$homeval.totalCount)]*100,
               "Max"       = (demog$homeval.175kto400k/demog$homeval.totalCount)[which.max(
                   demog$homeval.175kto400k/demog$homeval.totalCount)]*100,
               "N"         = sum(!is.na(demog$homeval.175kto400k>0))
    )%>%as.data.frame(.);##l24
    l25 = list("Mean"      = mean(demog$homeval.400kplus/demog$homeval.totalCount,na.rm=T)*100,
               "Std. dev." = sd(demog$homeval.400kplus/demog$homeval.totalCount,na.rm=T)*100,
               "Min"       = 100*min((demog$homeval.400kplus/demog$homeval.totalCount),na.rm=T),
               "Max"       = 100*max((demog$homeval.175kto400k/demog$homeval.totalCount),na.rm=T),
               "N"         = sum(!is.na(demog$homeval.400kplus>0))
    )%>%as.data.frame(.);l25
    l26 = list("Mean"      = mean(demog$medbeds,na.rm=T),
               "Std. dev." = sd(demog$medbeds,na.rm=T),
               "Min"       = min((demog$medbeds),na.rm=T),
               "Max"       = max(demog$medbeds,na.rm=T),
               "N"         = sum(!is.na(demog$homeval.400kplus>0))
    )%>%as.data.frame(.);l26
    l27 = list("Mean"      = mean(demog$medMort_ownW/100,na.rm=T),
               "Std. dev." = sd(demog$medMort_ownW/100,na.rm=T),
               "Min"       = min((demog$medMort_ownW/100),na.rm=T),
               "Max"       = max(demog$medMort_ownW/100,na.rm=T),
               "N"         = sum(!is.na(demog$medMort_ownW>0))
    )%>%as.data.frame(.);l27
    l28 = list("Mean"      = mean(demog$mort_agg_ownW/1000000,na.rm=T),
               "Std. dev." = sd(demog$mort_agg_ownW/1000000,na.rm=T),
               "Min"       = min((demog$mort_agg_ownW/1000000),na.rm=T),
               "Max"       = max(demog$mort_agg_ownW/1000000,na.rm=T),
               "N"         = sum(!is.na(demog$mort_agg_ownW/1000000>0))
    )%>%as.data.frame(.);l28
    l29 = list("Mean"      = mean(demog$occ.h.ownocc/1000,na.rm=T),
               "Std. dev." = sd(demog$occ.h.ownocc/1000,na.rm=T),
               "Min"       = min((demog$occ.h.ownocc/1000),na.rm=T),
               "Max"       = max(demog$occ.h.ownocc/1000,na.rm=T),
               "N"         = sum(!is.na(demog$occ.h.ownocc/1000>0))
    )%>%as.data.frame(.);l29
    tab1=(do.call(rbind, list("Zip code number of res. installations" = l1,
                              "Zip code MW of res. installations" =l2,
                              "Population (100,000s)" =l3,
                              "Household size" =l4,
                              "Median income ($10,000s)" =l5,
                              "% pop male" =l6,
                              "% pop who are white" =l7,
                              "% pop with college degree" =l8,
                              "% pop between 20 and 45" =l9,
                              "% pop over 65" =l10,
                              "% pop who drive to work" =l11,
                              "% pop who carpool to work" =l12,
                              "% pop who use public transit to work" =l13,
                              "% pop who work at home or walk to work" =l14,
                              "% pop with over a 30 min commute" =l15,
                              # l16,
                              "Number of owner-occ.d homes (1,000s)" =l17,
                              "Median value of Owner-occ.d home ($mill)" =l18,
                              # l19,
                              # l20,
                              "% homes worth $0k--90k"    = l21,
                              "% homes worth $50k--90k"   = l22,
                              "% homes worth $90k--$175k" = l23,
                              "% homes worth $175k--$400k"= l24,
                              "% homes worth $400k+"      = l25,
                              "Median number of bedrooms" = l26,
                              "Median number of Mortgaged, owner-occ.d, Houses (100s)" = l27,
                              "Aggregate Mortage ($mill)" = l28,
                              "Total pop. of owner-occ. Housing (1000s)" = l29
    )))%>%as.data.frame(.);tab1;# tab1 = forestmangr::round_df(tab1,rf="round",digits=3); tab1
    
    setwd("/Users/tdevine/Box Sync/research/solar/Analysis/tables/")
    
    xtab1 <- xtable(tab1,
                    latex.environments = "center", digits=c(0,3,2,1,2,0),
                    caption = "Summary statistics for residential installations in zip codes with at least one installation.",
                    label= "table1",
                    align = "lrrrrr"
    );
    # align(xtab1) = c("lrrrrr")
    print(xtab1,file = "tab1.tex");
}
rm(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23,l24,l25,l26,l27,l28,l29)
rm(tab1.data1  ,tab1.data2 , tab1.data3 , tab1.data4 , tab1.data5 , tab1.data6 , tab1.data7 , tab1.data8 ,tab1.data9 , tab1.data10,
   tab1.data11 ,tab1.data12 ,tab1.data13 ,tab1.data14, tab1.data15, tab1.data16,tab1.data17 ,tab1.data18 ,tab1.data19, tab1.data20,
   tab1.data21 ,tab1.data22, tab1.data23, tab1.data24 ,tab1.data25)
rm(tempp,tempsd,tempt,temp.Ownocc)