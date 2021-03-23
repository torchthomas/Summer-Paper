# Summer-Paper
Equivalent to my Master's Thesis. My overleaf link in my Projects section of http://thdevine.github.io/ has a much nicer and more concise portion of code, but this contains a lot more notes, highlights what I needed to delete, discussed variables I excluded from the census, and much more. 

# The code below is an abbreviated version of code_totalcensus which highlights the order of the other scripts which care source(.)d:
#Clear stuff first; set working directory

source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_makefunctions.R")

source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_tablesToPull.R")

source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_pullData.R")

source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_dataWrangle.R")

source("C:/Users/tdevine/Box Sync/research/solar/Analysis/code_filterCSI.R")

source("code_makePanel.R") 

#source("code_hazard.R") #create data for hazard analysis

#WRITE TO DTA FILE

foreign::write.dta(install.data,"c:/Users/tdevine/Box Sync/research/solar/Data_California/stata/installdata.dta")

foreign::write.dta(csi.long,"c:/Users/tdevine/Box Sync/research/solar/Data_California/stata/csilong.dta")
