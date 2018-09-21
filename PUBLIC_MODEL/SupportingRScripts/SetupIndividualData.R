 

INDIVIDUAL_LENGTH_WEIGHT <-
  read.csv("SandeelData/INDIVIDUAL_LENGTH_WEIGHT.csv") %>%
  select(-SURVEY)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Which season ?
SeasonDF = data.frame(SEASON = c(rep("Spring",4),
                                 rep("Summer",4),
                                 rep("Autumn",4)),
                      Month = 1:12)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~ Go through the survey chronologicals ~~~~~#

PATH = "SandeelData/SurveyChrons/"
ChronFiles = list.files(path =PATH)

SurveyChron = NULL


for(FileNumber in 1:length(ChronFiles))
  {
  FILE= paste0(PATH,ChronFiles[FileNumber])
  Chron <- read.csv(FILE)
  
  if(grepl("Trawl", FILE)){ Chron = subset(Chron, GEARTYPE=="Pelagic")  # Only deal with pelagic trawls, not demersal
  SURVEY_INITIAL = factor("T") }
  if(grepl("Dredge", FILE)){SURVEY_INITIAL = factor("D")}
  if(grepl("Grab", FILE)){SURVEY_INITIAL = factor("G")}
  
  
  Chron$JULIANDAY =  yday(as.Date(paste0(Chron$YEAR,"-",Chron$MONTH,"-",Chron$DAY)))
  Chron = Chron %>%
    group_by(YEAR,CRUISE) %>%
    dplyr::summarize(Mean = floor(mean(unique(JULIANDAY))))%>%
    mutate(SurveyDate =Mean+ as.Date(paste0(YEAR,"-",1,"-",1)) -1)
  
  SurveyDaysFromModelStartDay =
    data_frame(
      SurveyDate = Chron$SurveyDate,
      DRUN = as.numeric(as.vector(SurveyDate - StartDate)),
      Month = month(SurveyDate),
      YEAR = year(SurveyDate),
      SURVEY_INITIAL = SURVEY_INITIAL,
      STATE = 9999)
  

  
  SurveyDaysFromModelStartDay =
    left_join(SurveyDaysFromModelStartDay,SeasonDF,by = "Month")
  
  SurveyDaysFromModelStartDay[SurveyDaysFromModelStartDay$Month<5,]$STATE = 0
  SurveyDaysFromModelStartDay[SurveyDaysFromModelStartDay$Month>8,]$STATE = 0
  
  if( sum(SurveyDaysFromModelStartDay$STATE==9999) !=0){
    SurveyDaysFromModelStartDay[SurveyDaysFromModelStartDay$STATE==9999,]$STATE =1}
  
  SurveyChron = rbind(SurveyChron,SurveyDaysFromModelStartDay)
}
rm(Chron,SeasonDF,ChronFiles,FILE,FileNumber,PATH,SURVEY_INITIAL,SurveyDaysFromModelStartDay)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


INDIVIDUAL_LENGTH_WEIGHT = join_all(
  list(INDIVIDUAL_LENGTH_WEIGHT, SurveyChron),
  by = c("YEAR", "SEASON", "SURVEY_INITIAL")
)




rm(SurveyChron)

INDIVIDUAL_LENGTH_WEIGHT$COHORT = INDIVIDUAL_LENGTH_WEIGHT$YEAR - INDIVIDUAL_LENGTH_WEIGHT$Age
INDIVIDUAL_LENGTH_WEIGHT$DF_DATE_ADDED = INDIVIDUAL_LENGTH_WEIGHT$DRUN


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# This part involves scaling abundance to total firth of forth area (Simon Greenstreet 2006,2010)

#~ Apply correction factors to abundance data 

Number.at.age <-
  read.csv("SandeelData/Number.at.age0.csv")[, -1]
names(Number.at.age) = c("RealAbundance", "Age", "YEAR", "COHORT")
Number.at.age$SURVEY_INITIAL = INDIVIDUAL_LENGTH_WEIGHT[INDIVIDUAL_LENGTH_WEIGHT$SURVEY_INITIAL=="T",]$SURVEY_INITIAL[1]
Number.at.age$COHORT = as.numeric(as.vector(substr(Number.at.age$COHORT, 5, 9)))
Number.at.age = full_join(Number.at.age, INDIVIDUAL_LENGTH_WEIGHT, by = c("Age", "YEAR", "COHORT","SURVEY_INITIAL"))
Number.at.age = Number.at.age[complete.cases(Number.at.age$LENGTH),]

ScalingFactor = Number.at.age %>%
  group_by(RealAbundance,Age,YEAR, COHORT,SEASON,
           SURVEY_INITIAL,SurveyDate,DRUN,
           STATE,DF_DATE_ADDED) %>%
  dplyr::summarize(Total = sum(ABUNDANCE)) 
ScalingFactor = ScalingFactor[complete.cases(ScalingFactor$DRUN), ]
ScalingFactor$ScalingFactor = ScalingFactor$RealAbundance/ScalingFactor$Total

                 
ScalingFactor = dplyr::left_join( INDIVIDUAL_LENGTH_WEIGHT,ScalingFactor,
                 by =c("Age","YEAR","COHORT","SEASON",
                       "SURVEY_INITIAL","SurveyDate",
                       "DRUN","STATE","DF_DATE_ADDED"))
                 
                    
#~~~~~~~~~~~~~~~~~~~ INITIAL CONDITIONS ~~~~~~~~~~~~~~~~~~~~~~~~#
  
InitialCohorts <- read.csv("USER_INPUT/InitialCohorts.csv",stringsAsFactors=FALSE, 
colClasses = c("numeric","numeric","character","character"))

InitialCohorts$SURVEY = factor(InitialCohorts$SURVEY, levels = c("D","G","T"))
InitialCohorts$SEASON = factor(InitialCohorts$SEASON, levels = c("Autumn","Spring","Summer"))


IndividualData0=NULL

for(RowNum in 1:nrow(InitialCohorts))
  {
  
  IndividualData0 = rbind(IndividualData0,
                        INDIVIDUAL_LENGTH_WEIGHT %>%
                          subset( YEAR == InitialCohorts$YEAR[RowNum] &
                                    Age == InitialCohorts$AGE[RowNum] &
                                    SURVEY_INITIAL == substr(InitialCohorts$SURVEY[RowNum],1,1) &
                                    SEASON == InitialCohorts$SEASON[RowNum] )
)
  
}
                              
rm(InitialCohorts)                    
                          
 
CohortData =IndividualData0 %>%
  distinct(COHORT,YEAR,Age,SURVEY_INITIAL,SEASON,DF_DATE_ADDED) %>%
  arrange(Age,COHORT,DF_DATE_ADDED)  
  
InitialConditions =NULL

for(CohortNum in 1:nrow(CohortData))
  {
  
  IndividualData00 = subset(IndividualData0,COHORT==CohortData$COHORT[CohortNum] &
                            YEAR == CohortData$YEAR[CohortNum] &
                            Age == CohortData$Age[CohortNum] &
                            SURVEY_INITIAL== CohortData$SURVEY_INITIAL[CohortNum] &
                            SEASON== CohortData$SEASON[CohortNum] &
                            DF_DATE_ADDED ==CohortData$DF_DATE_ADDED[CohortNum])
  
  InitialConditions0 = left_join(IndividualData00,ScalingFactor,by = c("LENGTH","WEIGHT","ABUNDANCE",
                                               "YEAR","SEASON","SURVEY_INITIAL",
                                               "Age","SurveyDate",
                                               "Month","STATE","COHORT","DF_DATE_ADDED")) 
  

  if(sum(complete.cases(InitialConditions0$ScalingFactor))==0)
  {
    InitialConditions0 = InitialConditions0 %>%
      select(LENGTH,WEIGHT,ABUNDANCE,Age,STATE,DF_DATE_ADDED)
  } else {
    InitialConditions0 = InitialConditions0 %>%
      mutate(ABUNDANCE = ScalingFactor*ABUNDANCE) %>%
      select(LENGTH,WEIGHT,ABUNDANCE,Age,STATE,DF_DATE_ADDED)
  }
  
  InitialConditions = rbind(InitialConditions,InitialConditions0)
  
}
 

DF_LENGTH = InitialConditions$LENGTH
DF_WEIGHT = InitialConditions$WEIGHT
DF_DATE_ADDED = InitialConditions$DF_DATE_ADDED
DF_DATE_FINISH = rep(length(TT_FEED), length(DF_DATE_ADDED))
ABUNDANCE_DAILY = InitialConditions$ABUNDANCE
Age = InitialConditions$Age
DF_STATE = InitialConditions$STATE


rm(InitialConditions,InitialConditions0,IndividualData00)
 
if(Parameterisation=="F" | Parameterisation !="T"){

# this is added to keep C happy - values dont matter  
  
CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NumMeanLengths",
                             value =1, # any value
                             Ctype = "define"))

CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NumAbundancePoints",
                             value =1, # any value
                             Ctype = "define"))


CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NumABUNDANCEsurveys",
                             value =1, # any value
                             Ctype = "define"))

CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NumWLsurveys",
                             value =1, # any value
                             Ctype = "define"))
}

# Parameters 
FittedParameters <- read.csv("USER_INPUT/FittedParameters.csv")
X = FittedParameters$value

if(Parameterisation!="F" | Parameterisation =="T"){
  source("SupportingRScripts/SetupParameterisation.R")
}

Number.of.fitted.parameters = length(X)

CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NoParameters",
                             value =Number.of.fitted.parameters, # Number of fitted parameters
                             Ctype = "define"))

  
# Fill remaining constants 



CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="IndNum",
                             value =length(ABUNDANCE_DAILY),    
                             Ctype = "define"))



write.table(data.frame(CONSTANTS$Ctype,
                       CONSTANTS$Parameter,
                       CONSTANTS$value),
            file ="SupportingRScripts/CONSTANTS.txt",quote=F,sep = ",",row.names = F,col.names = F)

system('python SupportingRScripts/SetupConstants.py')

file.remove("SupportingRScripts/CONSTANTS.txt")

rm(CohortData,INDIVIDUAL_LENGTH_WEIGHT,IndividualData0,
   Number.at.age,ScalingFactor,CohortNum)
 
