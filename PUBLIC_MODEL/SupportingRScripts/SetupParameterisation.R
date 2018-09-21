
#~~~~~~~~~~~~~~~~~~~~~~~~~ DATA TO FIT MODEL TO~~~~~~~~~~~~~~~~~~~~~~#


# Fit to summer trawl survey data, spring and autumn dredge survey data
# *except 2002* we fit to dredge data in summer 2002 


FittingData = INDIVIDUAL_LENGTH_WEIGHT %>%
  group_by(DRUN, YEAR, SEASON, SURVEY_INITIAL, Age, COHORT,DF_DATE_ADDED) %>%
  dplyr::summarize(
    MeanLength = sum(LENGTH * ABUNDANCE) / sum(ABUNDANCE),
    MeanWeight = sum(WEIGHT * ABUNDANCE) / sum(ABUNDANCE)
  ) %>% subset(DRUN >=0) %>%
  subset( !(SEASON =="Summer" & SURVEY_INITIAL !="T")) %>% # ONLY SUMMER TRAWLS, NO DREDGE
  subset( SURVEY_INITIAL !="G") %>%
  select(-DF_DATE_ADDED)

FittingData2 = INDIVIDUAL_LENGTH_WEIGHT %>%
  group_by(DRUN, YEAR, SEASON, SURVEY_INITIAL, Age, COHORT,DF_DATE_ADDED) %>%
  dplyr::summarize(
    MeanLength = sum(LENGTH * ABUNDANCE) / sum(ABUNDANCE),
    MeanWeight = sum(WEIGHT * ABUNDANCE) / sum(ABUNDANCE)
  ) %>% subset(DRUN >=0) %>%
  subset( SEASON =="Summer" & SURVEY_INITIAL =="D" & YEAR == 2002) %>% # DREDGE
  subset( SURVEY_INITIAL !="G") %>%
  select(-DF_DATE_ADDED)

FittingData = rbind(FittingData,FittingData2) %>%
  subset( !(SEASON =="Summer" & SURVEY_INITIAL =="T" & YEAR == 2002))



ALLFITTINGDATA = NULL
for(CohortNum in 1: nrow(CohortData))
{
  
  # For length and weight                                            
  MEAN_WL_DATE_ADDED=CohortData$DF_DATE_ADDED[CohortNum]
  
  MEAN_WL_AGE = CohortData$Age[CohortNum]
  
  DF = subset(FittingData,DRUN> CohortData$DF_DATE_ADDED[CohortNum] &
                COHORT ==CohortData$COHORT[CohortNum])
  ALLFITTINGDATA = rbind(ALLFITTINGDATA, data.frame(MEAN_WL_AGE = MEAN_WL_AGE,MEAN_WL_DATE_ADDED = rep(MEAN_WL_DATE_ADDED, nrow(DF)), DF))
  
  
}

# No environemtnal data after 2008
ALLFITTINGDATA = subset(ALLFITTINGDATA, YEAR < 2009) %>%
  arrange(MEAN_WL_AGE,MEAN_WL_DATE_ADDED,DRUN) %>%
  subset( !(MEAN_WL_AGE==0 & MEAN_WL_DATE_ADDED==0 & DRUN ==720)) # remove age 0 in 2000 and age 2 in summer survey in 2002


 
MEAN_WL_DATE_ADDED = ALLFITTINGDATA$MEAN_WL_DATE_ADDED
MEAN_WL_DRUN= ALLFITTINGDATA$DRUN
MEAN_WL_LENGTH = ALLFITTINGDATA$MeanLength
MEAN_WL_WEIGHT = ALLFITTINGDATA$MeanWeight
MEAN_WL_AGE = ALLFITTINGDATA$MEAN_WL_AGE

WL_DATE_CHECK = sort(unique(MEAN_WL_DRUN))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~ ABUNDANCE MODEL ~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


AbundanceFittingData = INDIVIDUAL_LENGTH_WEIGHT %>%
  group_by(DRUN, YEAR, SEASON, SURVEY_INITIAL, Age, COHORT,DF_DATE_ADDED) %>%
  dplyr::summarize(
    MeanLength = sum(LENGTH * ABUNDANCE) / sum(ABUNDANCE),
    MeanWeight = sum(WEIGHT * ABUNDANCE) / sum(ABUNDANCE)
  ) %>% subset(DRUN >=0) %>%
  subset( !(SEASON =="Summer" & SURVEY_INITIAL !="T")) %>% # ONLY SUMMER TRAWLS, NO DREDGE
  subset( SURVEY_INITIAL !="G") %>%
  select(-DF_DATE_ADDED)

 


ABUNDANCEFIT = NULL
for(CohortNum in 1: nrow(CohortData))
{
  
  # For abundance                                            
  ABUNDANCE_DATE_ADDED=CohortData$DF_DATE_ADDED[CohortNum]
  
  ABUNDANCE_AGE = CohortData$Age[CohortNum]
  
  DF = subset(AbundanceFittingData,DRUN>= CohortData$DF_DATE_ADDED[CohortNum] &
                COHORT ==CohortData$COHORT[CohortNum])
  ABUNDANCEFIT = rbind(ABUNDANCEFIT, data.frame(ABUNDANCE_AGE = ABUNDANCE_AGE,ABUNDANCE_DATE_ADDED = rep(ABUNDANCE_DATE_ADDED, nrow(DF)), DF))
  
  
}

ABUNDANCEFITTINGDATA = left_join(Number.at.age,ABUNDANCEFIT,by=c("Age","YEAR","COHORT","SURVEY_INITIAL","SEASON","DRUN")) %>%
  distinct(RealAbundance,Age,YEAR,COHORT,SURVEY_INITIAL,SEASON,DRUN,ABUNDANCE_AGE,ABUNDANCE_DATE_ADDED) %>%
  mutate(LogAb = log10(RealAbundance+1)) %>%
  subset(ABUNDANCE_AGE==0 & SURVEY_INITIAL=="T" ) %>% # only fit abundance to those initially age 0
  arrange(COHORT) %>%
  subset(YEAR < 2009)


ABUNDANCE_FIT = ABUNDANCEFITTINGDATA$LogAb
ABUNDANCE_DRUN = ABUNDANCEFITTINGDATA$DRUN
ABUNDANCE_AGE = ABUNDANCEFITTINGDATA$ABUNDANCE_AGE
ABUNDANCE_DATE_ADDED = ABUNDANCEFITTINGDATA$ABUNDANCE_DATE_ADDED

ABUNDANCE_DATE_CHECK = sort(unique(ABUNDANCE_DRUN))







# Fill remaining constants 


CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NumMeanLengths",
                             value =length(MEAN_WL_AGE),
                             Ctype = "define"))

CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NumAbundancePoints",
                             value =length(ABUNDANCE_AGE),
                             Ctype = "define"))


CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NumABUNDANCEsurveys",
                             value =length(ABUNDANCE_DATE_CHECK),
                             Ctype = "define"))
 
CONSTANTS = rbind(CONSTANTS,
                  data.frame(Parameter="NumWLsurveys",
                             value =length(WL_DATE_CHECK),
                             Ctype = "define"))



