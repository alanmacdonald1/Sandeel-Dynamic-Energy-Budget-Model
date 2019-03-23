#############################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  SANDEEL DEB MODEL  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#############################################################################################################

# SET WORKING DIRECTORY

rm(list = ls())

if (as.data.frame(Sys.info()["sysname"]) == "Linux")
  DBdir <- ("/home/qrb12181/Dropbox") else
    DBdir <- ("/Users/Alan/Dropbox")
  
  setwd(paste0(DBdir,"/PhD_archived/DEB_MODEL"))
  
  # compile c code
  system('R CMD SHLIB ModelRun.c')
  
  #~~~~~~~~~~~ REQUIRED PACKAGES ~~~~~~~~~~#
  
  suppressMessages(library(plyr))
  suppressMessages(library(dplyr))
  suppressMessages(library(geosphere))
  suppressMessages(library(lubridate))
  suppressMessages(library(ggplot2))
  library(readr)
  library(GenSA)
  
  #~~~~~~~~~~~ MODEL FILES ~~~~~~~~~~#
  
  Parameterisation="F"
  
  # constants
  source("SupportingRScripts/Constants.R")
  
  # Food, temperature, days run
  source("SupportingRScripts/EnvironmentalConditions.R")
  
  # Initial conditions for length, weight and abundance + fitting data
  source("SupportingRScripts/SetupIndividualData.R")
  
  # convert length and weight to R,S,G for initial conditions
  source("SupportingRScripts/ConvertLengthWeightToReserveStructureGonad.R")
  
  if(Parameterisation=="T" | Parameterisation !="F")
    {
  # convert length and weight to R,S,G for initial conditions
  source("SupportingRScripts/Parameterisation.R")
  }
  
  # Compute model error...
  DEBrun= function(XXX)
  {
    {
      DEBmodel =  .C("MODEL_RUN",
                     as.double(XXX),
                     as.double(TT_FEED),
                     as.double(TT_OV),
                     as.double(ACT.MET),
                     as.double(ACT.CONS),
                     as.integer(JulianDayV),
                     as.double(ABUNDANCE_DAILY),
                     as.integer(Age),
                     as.double(FL),
                     as.double(FS),
                     as.double(DF_R),
                     as.double(DF_S),
                     as.double(DF_G),
                     as.double(Rdw),
                     as.double(Er),
                     as.integer(DF_STATE),
                     as.integer(DF_DATE_ADDED),
                     as.integer(DF_DATE_FINISH),
                     as.double(DF_LENGTH),
                     as.double(DF_WEIGHT),
                     as.integer(TimeToSave[,1]),
                     result = double(length(1)))
    }
    DEBmodel[["result"]]
  }
  
# load .so file
dyn.load("ModelRun.so")


# initial time
ptm <- proc.time()
print(paste("Started at",Sys.time()))


if(Parameterisation!="T" | Parameterisation =="F")
{
  DEBrun(X)
  source("SupportingRScripts/PlotResults.R")
  system('open Figures/AbundanceWeightLength.jpeg')
}

if(Parameterisation=="T" | Parameterisation !="F")
{
       # SANNr(XXX=X)
  }

# remove temporary files
file.remove("ModelRun.o")
file.remove("ModelRun.so")

