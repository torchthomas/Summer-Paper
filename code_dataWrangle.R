# SCRIPT TO PULL DATA FOR SUMMER PAPER 2020
# AUTHOR: THOMAS DEVINE
# currdir: "C:/Users/tdevine/Desktop/2020 Summer Paper/Analysis"
# CREATED: (when I made my code neat) 8.20.2020
#.............
    #Comments:     This doesn't deal with CSI data, only Demographic data
{
    # (block-level to block group-level) Pull dec2010 data with ADDITIONAL tables, 
    dec2010.new <- read_decennial(
        year = 2010,   
        state = "CA",
        geo_headers = c("ZCTA5"),
        summary_level = "block", 
        table_contents = dec.tables
    ) %>%
        .[, geoid_blkgrp := stringr::str_sub(GEOID, 6, 19)]%>% # shortening geoid to that of block groups 
        rename(population_dec = population) %>%                # rename the population variable since we duplicate the name with acs2011
        filter(population_dec > 0) %>%                         # drop blocks with no people; dim: 403,398 x 19
        as.data.table(); 
    dim(dec2010.new)
    # Like we did for dec2010 and acs2011 before pulling dec2010.new
    dec2010.new <- dec2010.new[which(dec2010.new$geoid_blkgrp %in% dec2010$geoid_blkgrp),] %>%
        dplyr::select(-NAME,-acs_NAME,-GEOCOMP,-SUMLEV,-state,-lat,-lon,-STUSAB,-GEOID);dim(dec2010.new) #dim: 23,136 x 19
    # (block group-level) BEFORE WE CAN JOIN blkgrp_zcta5 (created later) with dec2010 (here, blkgrp_zcta5) we MUST
    #                     bring dec2010.new to the block group-level
    dec.blkgrp_zcta5 <- dec2010.new %>% 
        group_by(geoid_blkgrp,ZCTA5) %>%
        mutate_if(.,is.numeric,sum) %>%
        ungroup() %>% 
        unique()%>%
        as.data.table(); dim(dec.blkgrp_zcta5) #dim: 28396 x 9
    rm(dec2010.new)
    
    
    # Create the weights corresponding to the block group-zcta5 combinations to aggregate up
    w.blkgrp_zcta5 <- dec2010 %>%              
        .[,.(geoid_blkgrp,ZCTA5,population_dec)] %>%              # variables for calculating weight
        group_by(geoid_blkgrp,ZCTA5) %>%
            mutate(pop.blkgrp_zcta5 = sum(population_dec)) %>%    # weight's numerator
            ungroup()%>%
        group_by(geoid_blkgrp) %>%                                # weight's denominator
            mutate(pop.geoid_blkgrp = sum(population_dec)) %>%
            ungroup()%>%       
        rowwise() %>% 
            mutate(w = pop.blkgrp_zcta5 / pop.geoid_blkgrp) %>%   # create weight variable
            ungroup()%>%
        dplyr::select(geoid_blkgrp,ZCTA5,w) %>%                          # the rest arranges, conforms structure, and selects unique rows
        arrange(geoid_blkgrp)%>%
        unique()%>%
        as.data.table(); dim(w.blkgrp_zcta5); #View(w.blkgrp_zcta5); dim: 28396 x 3
    
    # (blkgrp_zcta5-level) Join the acs2011 w/ w.blkgrp_zcta5, #rows should be: 28,396 obs. 
    # acs2011 blkgrps repeat because of splitting of blkgrps across zcta5s. We need only match by blkgrp to partition share of acs2011's blkgrps
    acs.blkgrp_zcta5 <- w.blkgrp_zcta5[acs2011, on = c("geoid_blkgrp")] %>% # want to remove common
        dplyr::select(-STUSAB,-state,-GEOCOMP,-SUMLEV,-GEOID); dim(acs.blkgrp_zcta5) #dim: 28,396 x 142 #join on geoid_blkgrp, then reorder
    # (blkgrp_zcta5-level) Join the acs.blkgrp_zcta5 w/ dec2010, #rows should be: 28,396 obs. 
    
    joined <- dec.blkgrp_zcta5[acs.blkgrp_zcta5, on = c("geoid_blkgrp","ZCTA5")] %>% #join on geoid_blkgrp, then reorder
        dplyr::select(.,-starts_with("i."),-NAME,-COUNTY); dim(joined) #dim: 28,396 x 148
    
    ##Next we aggregate to ZCTA5-level, ultimately merging both 
    # (blkgrp_zcta5-level) first sum counts, then avg medians
    counts.zcta5 <- joined %>%                     # joined's dimensions are 28,396 x 148
        arrange(geoid_blkgrp,ZCTA5) %>%            # might help for binding if forced to later
        dplyr::select(-starts_with(c("size_","med")) )%>% # drop median stats & other vars;
        dplyr::select(-w, everything()) %>%               # HWickham's way to move col to end
        rowwise()%>%                               # force multiply all numeric cols by a weight cell in the same row
            mutate_if(.,is.numeric, funs(w*.)) %>%     # operation call to mutate each cell
            ungroup()%>%                               # stop rowwise grouping
        dplyr::select(-w) %>%                             # drop weight col; at this point, 28,396 x 139
        group_by(ZCTA5) %>%
            mutate_if(is.numeric,funs(sum)) %>%        # important sum
            ungroup() %>%
        dplyr::select(-geoid_blkgrp) %>%                  # drop geoid_blkgrp
        unique(.) %>%                              # grab unique cols, like slice(1) or top_n(1), all NUMERIC cells are equals 
        dplyr::select(ZCTA5,everything())%>%
        arrange(ZCTA5) ; dim(counts.zcta5); #dim: 1,760 x 136
    medians.zcta5 <- joined %>%
        dplyr::select(geoid_blkgrp,ZCTA5,
               starts_with(c("size_","med")),w)%>% # Select cols that are median statistics, putting w last like before
        group_by(ZCTA5) %>% 
        summarise_if(., is.numeric, funs(mean(.)) ) %>%
        ungroup() %>%
        unique() ; dim(medians.zcta5) #1,760 x 11
    
    # (ZCTA5-level) join the counts and medians
    demog.old <- left_join(counts.zcta5,medians.zcta5, by  = "ZCTA5"); dim(demog.old)
    
    ## Rename, recalculate, and redefine the vars to match up with BG 2012
    demog <- as.data.frame(demog.old) %>%
        dplyr::select(.,-pop) %>%
        mutate( pop=population/1E5,#dup
                age0019   = M_0to4+M_5to9+M_10to14+M_15to17+M_18to19+F_0to4+F_5to9+F_10to14+F_15to17+F_18to19,
                age2045   = M_20+M_21+M_22to24+M_25to29+M_30to34+M_35to39+M_40to44+M_45to49+F_20+F_21+F_22to24+F_25to29+F_30to34+F_35to39+F_40to44+F_45to49,
                age65plus = M_65to66+M_67to69+M_70to74+M_75to79+M_85over+F_65to66+F_67to69+F_70to74+F_75to79+F_85over,
                collegedegrees = pop25f_AssocDeg +pop25f_BachDeg+pop25f_MastDeg+pop25f_ProfDeg+pop25f_DocDeg+pop25m_AssocDeg+pop25m_BachDeg+pop25m_MastDeg+pop25m_ProfDeg+pop25m_DocDeg,
                med_inc_HH = med_inc_HH/10000,#dup
                medhomeval_ownOcc = medhomeval_ownOcc/1E6,#dup
                homeval.0kto50k   =(homeval_less10k+homeval_10kto14999+homeval_15kto19999+homeval_20kto24999+homeval_25kto29999+homeval_30kto34999+homeval_35kto39999+homeval_40kto49999), 
                homeval.50kto90k  =(homeval_50kto59999+homeval_60kto69999+homeval_70kto79999+homeval_80kto89999), 
                homeval.90kto175k =(homeval_90kto99999+homeval_100kto124999+homeval_125kto149999+homeval_150kto174999), 
                homeval.175kto400k=(homeval_175kto199999+homeval_200kto249999+homeval_250kto299999+homeval_300kto399999), 
                homeval.400kplus  =(homeval_400kto499999+homeval_500kto749999+homeval_750kto999999+homeval_1millmore), 
                t30mincommute     = workers_30+workers_45+workers_60+workers_more,
                homeval.totalCount= homeval.0kto50k+homeval.50kto90k+homeval.90kto175k+homeval.175kto400k+homeval.400kplus,
                total.commuting   = workers_commuted_total,
                medbeds=medbeds,
                medmort_ownW  = medMort_ownW,
                mort_agg_ownW = mort_agg_own_wMort,
                occ.h.ownocc = occ.h.ownocc,
                pop.house.tenure.rent=pop.house.tenure.rent,
                pop.house.tenure.total=pop.house.tenure.total
                
        ) %>% 
        dplyr::select(.,-starts_with("M_"),-starts_with("F_"),-starts_with("pop25"),-starts_with("workers_"),-starts_with("homeval_")) %>%
        rowwise(.) %>%
            mutate(., workhome = sum(work_at_home,walked,na.rm=T)) %>%
            ungroup(.) %>%
        dplyr::select(-walked,-work_at_home) %>%     # dim is 1759 x 76 BEFORE this point
        .[-which(.$ZCTA5 %in% c(99999)),] %>% # 99999 is not a real ZCTA5, drop it
        as.data.table(.); dim(demog)# dim: 1,760 x 74
}