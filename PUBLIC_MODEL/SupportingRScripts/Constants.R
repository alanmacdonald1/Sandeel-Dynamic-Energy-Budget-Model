

CONSTANTS <- read.csv("USER_INPUT/CONSTANTS.csv")
 


#~~Run time
# the DRUN numbers are how many days we are ahead of our start day, 2000/6/17 in model  

StartDate = 
as.Date(paste0(CONSTANTS[CONSTANTS$Parameter=="StartYear",]$value, "/",
CONSTANTS[CONSTANTS$Parameter=="StartMonth",]$value, "/",
CONSTANTS[CONSTANTS$Parameter=="StartDay",]$value))

EndDate = 
  as.Date(paste0(CONSTANTS[CONSTANTS$Parameter=="EndYear",]$value, "/",
                 CONSTANTS[CONSTANTS$Parameter=="EndMonth",]$value, "/",
                 CONSTANTS[CONSTANTS$Parameter=="EndDay",]$value))


# to get dates

TT_FEED0 <- read.csv("EnvironmentalDrivers/Temperature/TT_FEED.csv")

TT_FEED0$Date = as.Date(TT_FEED0$Date, format="%d/%m/%Y")

TT_FEED0 = subset(TT_FEED0, Date >= StartDate & Date <= EndDate)

JulianDayV = TT_FEED0$jd
rm(TT_FEED0)

#~~ Reserve dynamics constants

a =CONSTANTS[CONSTANTS$Parameter=="a",]$value 
alpha = CONSTANTS[CONSTANTS$Parameter=="LS_a",]$value 
LS_a = CONSTANTS[CONSTANTS$Parameter=="LS_a",]$value 
Length_structure_exponent = CONSTANTS[CONSTANTS$Parameter=="LS_c",]$value 
gamma = CONSTANTS[CONSTANTS$Parameter=="gam",]$value   
gam = CONSTANTS[CONSTANTS$Parameter=="gam",]$value   
b =  CONSTANTS[CONSTANTS$Parameter=="b",]$value 
Ef = CONSTANTS[CONSTANTS$Parameter=="Ef",]$value  # energy density of fat
Ep = CONSTANTS[CONSTANTS$Parameter=="Ep",]$value  # energy density of protein
ashx = CONSTANTS[CONSTANTS$Parameter=="ashx",]$value 
ashy = CONSTANTS[CONSTANTS$Parameter=="ashy",]$value 

#~~ Activity multipliers

hrs.daylight = daylength(lat = 56.25,JulianDayV)

ACT.MET = 1 + (1/24)*hrs.daylight

ACT.CONS = hrs.daylight/24

###############


#~~~~~~~~~~~ Dry to wet weight conversion factors ~~~~~~~~~~~~#
  

Rdw =   CONSTANTS[CONSTANTS$Parameter=="Rdw",]$value   
Sdw = 1/(1 - gamma)   
Gdw = Sdw  

#~~~~~~~~~~~ Energy density  ~~~~~~~~~~~~*/
  
Es = ( Ep + Ep*ashx - gam*Ep*ashy)
Eg = Es  
  
#~~~~~~~~~~~ Ingestion  ~~~~~~~~~~~~*/

maxI = 3.696*0.0004359*( 1/ (LS_a*Es)) #  maximum ingestion rate scale


#~~~~ CHRONOLOGICALS ~~~~#

NoDays  = length(JulianDayV)  #  total number of days in run



CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NoDays",
                             value =length(JulianDayV) ,
                             Ctype = "define"))
  

# Days to save
TimeToSave <- read.csv("USER_INPUT/TimeToSave.csv", sep="",header=T)


CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NumberOfDaysToSave",
                             value =nrow(TimeToSave),
                             Ctype = "define"))


