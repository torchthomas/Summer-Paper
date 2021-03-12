# SCRIPT TO DEFINE WHICH TABLES TO PULL FOR SUMMER PAPER 2020
# AUTHOR: THOMAS DEVINE
# currdir: "C:/Users/tdevine/Desktop/2020 Summer Paper/Analysis"


# Tables I want to pull are from ACS2011 5 yr estimates and dec2010 
# I used www.socialexplorer.com to look at stuff online
        # e.g., https://www.socialexplorer.com/data/ACS2011_5yr/metadata/?ds=ACS11_5yr
#ACS2011 tables
{
    acs.tables <-  c(#"temp     = B25010_002", #IGNORE THIS ONE: it's temporary for checks and I don't have to scroll to the end
        #POPULATION: total, then by male or female total, then by sex and age
        "pop = B01001_001", # total
        "Male=B01001_002","M_0to4=B01001_003","M_5to9=B01001_004","M_10to14=B01001_005","M_15to17=B01001_006","M_18to19=B01001_007","M_20=B01001_008","M_21=B01001_009","M_22to24=B01001_010","M_25to29=B01001_011","M_30to34=B01001_012","M_35to39=B01001_013","M_40to44=B01001_014","M_45to49=B01001_015","M_50to54=B01001_016","M_55to59=B01001_017","M_60to61=B01001_018","M_62to64=B01001_019","M_65to66=B01001_020","M_67to69=B01001_021","M_70to74=B01001_022","M_75to79=B01001_023","M_80to84=B01001_024","M_85over=B01001_025",
        "Female=B01001_026","F_0to4=B01001_027","F_5to9=B01001_028","F_10to14=B01001_029","F_15to17=B01001_030","F_18to19=B01001_031","F_20=B01001_032","F_21=B01001_033","F_22to24=B01001_034","F_25to29=B01001_035","F_30to34=B01001_036","F_35to39=B01001_037","F_40to44=B01001_038","F_45to49=B01001_039","F_50to54=B01001_040","F_55to59=B01001_041","F_60to61=B01001_042","F_62to64=B01001_043","F_65to66=B01001_044","F_67to69=B01001_045","F_70to74=B01001_046","F_75to79=B01001_047","F_80to84=B01001_048","F_85over=B01001_049",
        #RACE
        "whitepop=B02001_002",#WHITEPOPALONE(TOTAL)
        #COLLEGE DEGREE BY SEX (male then female)
        "pop25m_AssocDeg=B15002_014","pop25m_BachDeg=B15002_015","pop25m_MastDeg = B15002_016","pop25m_ProfDeg=B15002_017","pop25m_DocDeg=B15002_018",
        "pop25f_AssocDeg=B15002_031","pop25f_BachDeg= B15002_032","pop25f_MastDeg = B15002_033","pop25f_ProfDeg=B15002_034","pop25f_DocDeg=B15002_035",
        #INCOME
        "med_inc_HH=B19013_001", #Median HH income in the past 12 months (in 2018 inflation-adjusted dollars)
        "med_inc_family=B19113_001", #Median FAMILY income in the past 12 months (in 2018 inflation-adjusted dollars)
        #COMMUTE
        "work_at_home=B08135_002", #	Means of Transportation to Work by Time Arriving At Work From Home for Workplace Geography
        "work_at_home_imputedtraveltime=B99084_005", #	B99084.	Imputation of Travel Time to Work
        "workers_30= B08135_007","workers_45=B08135_008","workers_60=B08135_009","workers_more=B08135_010", #MORE THAN AN HOURS COMMUTE
        "carpool=B08301_004","pubtransit=B08301_010","walked = B08136_011",
        #HOME
        "medhomeval_ownOcc=B25077_001",#MEDIAN VALUE TOTAl
        "homeval_less10k=B25075_002","homeval_10kto14999=B25075_003","homeval_15kto19999=B25075_004","homeval_20kto24999=B25075_005","homeval_25kto29999=B25075_006","homeval_30kto34999=B25075_007","homeval_35kto39999=B25075_008","homeval_40kto49999=B25075_009","homeval_50kto59999=B25075_010","homeval_60kto69999=B25075_011","homeval_70kto79999=B25075_012","homeval_80kto89999=B25075_013","homeval_90kto99999=B25075_014","homeval_100kto124999=B25075_015","homeval_125kto149999=B25075_016","homeval_150kto174999=B25075_017","homeval_175kto199999=B25075_018","homeval_200kto249999=B25075_019","homeval_250kto299999=B25075_020","homeval_300kto399999=B25075_021","homeval_400kto499999=B25075_022","homeval_500kto749999=B25075_023","homeval_750kto999999=B25075_024","homeval_1millmore=B25075_025",
        #HOUSE (TOTAL) Average Household Size of Occupied Housing Units by Tenure, then Average Household Size of Occupied Housing Units by Tenure
        "size_HH_tot  = B25010_001", "size_HH_OwnOc= B25010_002",   
        #rooms COUNT
        "beds_total=B25041_001","beds_0=B25041_002","beds_1=B25041_003","beds_2=B25041_004","beds_3=B25041_005","beds_4=B25041_006","beds_5more=B25041_007",
        #rooms Median number of bedrooms
        "medbeds = B25018_001",  
        #fuel source
        "fuel_total=B25040_001","fuel_gas=B25040_002","fuel_gasother=B25040_003","fuel_electricity=B25040_004","fuel_kero_oil=B25040_005","fuel_coal=B25040_006","fuel_wood=B25040_007","fuel_solar=B25040_008","fuel_other=B25040_009","fuel_none=B25040_010",
        #MORTGAGE .......dont include: 
        # Mortgage: Status (001 is table with total), various types of loans included in total
        "mort_status=B25081_001", 
        # Mortgage:	Aggregate Value (Dollars) by Mortgage Status
        "mort_agg_val_total=B25082_001","mort_agg_val_wMort=B25082_002","mort_agg_val_woMort=B25082_003",
        # Mortgage: Median Selected Monthly Owner Costs (Dollars) by Mortgage Status
        "medmort_ownTC=B25088_001","medMort_ownW=B25088_002","medmort_ownWO=B25088_003",
        # Mortgage: Aggregate Selected Monthly Owner Costs (Dollars) by Mortgage Status
        "mort_agg_own_total_cost=B25089_001","mort_agg_own_wMort=B25089_002","mort_agg_own_woMort=B25089_003",
        #commute continued (since adding them in place complicates things)
        "workcar = B08301_002",
        "occ.h.total=B25008_001",
        "occ.h.ownocc=B25008_002",
        "occ.h.not.ownocc=B25008_003",
        #Household Type by Tenure (hbt);  Universe: Universe: Households
        "hbt.total         =B11012_001", #includes renters 
        "hbt.tfhmcoo       =B11012_004", #family,married,ownocc
        "hbt.tfhofmaleoo   =B11012_008", #family,male-lead,own.occ
        "hbt.tfhoffemaleoo =B11012_011", #family,female-lead,own.occ
        "hbt.tnfoo         =B11012_014", #non family, own.occ
        "workers_commuted_total = B08301_001"
        #deleted other tested vars in deleted section at end between "del852020" comments
    )
}
#DEC2010 tables
{dec.tables <- c("pop.house.tenure.total           = H0040001", #Total - TENURE - Occupied housing units
                 "pop.house.tenure.ownocc.loan     = H0040002",
                 "pop.house.tenure.ownocc.loanfree = H0040003",
                 "pop.house.tenure.rent            = H0040004",
                 "Male_dec                         = P0120002",
                 "Female_dec                       = P0120026")}