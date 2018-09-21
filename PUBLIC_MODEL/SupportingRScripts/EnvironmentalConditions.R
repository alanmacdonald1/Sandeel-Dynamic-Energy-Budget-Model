#~~ Environmental conditions
#~~~~~~~~ Surface temperature ~~~~~~~~#


TT_FEED <- read.csv("EnvironmentalDrivers/Temperature/TT_FEED.csv")

TT_FEED$Date = as.Date(TT_FEED$Date, format="%d/%m/%Y")

TT_FEED = subset(TT_FEED, Date >= StartDate & Date <= EndDate)

TT_FEED = TT_FEED$temp1

#~~~~~~~~ TIME ~~~~~~~~#


#~~~~~~~~ Deep temperature ~~~~~~~~#

TT_OV <- read.csv("EnvironmentalDrivers/Temperature/TT_OV.csv")

TT_OV$Date = as.Date(TT_OV$Date, format="%d/%m/%Y")

TT_OV = subset(TT_OV, Date >= StartDate & Date <= EndDate)

TT_OV = TT_OV$temp45


#~~~~~~~~ FOOD ~~~~~~~~#

FL <- read.csv("EnvironmentalDrivers/Food/FL.csv")

FL$Date = as.Date(FL$Date, format="%d/%m/%Y")

FL = subset(FL, Date >= StartDate & Date <= EndDate)

FL = FL$FL
FL[FL==0] <-1E-15 # c gets fussy when FL is exactly zero


FS <- read.csv("EnvironmentalDrivers/Food/FS.csv")


FS$Date = as.Date(FS$Date, format="%d/%m/%Y")

FS = subset(FS, Date >= StartDate & Date <= EndDate)

FS = FS$FS
FS[FS==0] <-1E-15 # c gets fussy when FL is exactly zero


