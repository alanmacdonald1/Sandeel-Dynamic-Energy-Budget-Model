


FILE = "Individuals.txt"
  
d <- readLines(FILE)
  
s <- paste(d[seq(1,length(d),2)], d[seq(2,length(d),2)])
  
DF <- data.frame(read.table(header=F, stringsAsFactors=F, text=s))
names(DF) = c("JulianDay","Age","DF_DATE_ADDED","t","Dayofweek","Month","dayofmonth","timeofday",
                "year", "R","S","G","ResRatio","Weight","Length","Abundance")

DF$DateAdded = StartDate + DF$DF_DATE_ADDED
DF$Cohort = year(DF$DateAdded)  - DF$Age  
DF$Date = DF$t + StartDate
DF$DaysTracked = DF$Date- DF$DateAdded


Results =  DF %>% 
   subset(DaysTracked>=0) %>%
   subset(Age==0) %>%
   group_by(Age,DateAdded,Cohort,Date) %>%
    summarise(MeanLength = weighted.mean(Length, Abundance), # Mean length at day
              MeanWeight = weighted.mean(Weight, Abundance), # Mean weight at day
              MeanR = weighted.mean(R, Abundance), # Mean R at day
              MeanS = weighted.mean(S, Abundance), # Mean S at day
              MeanG = weighted.mean(G, Abundance), # Mean G at day
              MeanRS = weighted.mean(ResRatio, Abundance), # Mean Reserve ratio at day
              TotalCohortAbundance = log10(sum(Abundance))) %>%
  mutate(AgeDays =as.numeric(Date - DateAdded + yday(Date)), AgeYears = round(AgeDays/365,4))

rm(DF)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~ PLOT ABUNDANCE OF EACH GROUP ~~~# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

g1 = ggplot(Results)+
   facet_wrap(~ Cohort) +
  xlab("Age (years)")+
   geom_line( aes(x = AgeYears, y =TotalCohortAbundance))+
  ylab(expression(paste(log["10"], "(abundance)"))) +
  theme(panel.border = element_rect(
    colour = "black",
    fill = NA,
    size = 1
  ))
g2 = ggplot(Results)+
  facet_wrap(~ Cohort) +
  xlab("Age (years)")+
  geom_line( aes(x = AgeYears, y =MeanLength))+
  ylab("Length (cm)")+
  theme(panel.border = element_rect(
    colour = "black",
    fill = NA,
    size = 1
  ))
g3 = ggplot(Results)+
  facet_wrap(~ Cohort) +
  xlab("Age (years)")+
  geom_line( aes(x = AgeYears, y =MeanWeight))+
  ylab("Weight (g)")+
  theme(panel.border = element_rect(
    colour = "black",
    fill = NA,
    size = 1
  ))


lay <- rbind(c(1,1,2,2),
             c(1,1,3,3))
 
gs = list(ggplotGrob(g1), ggplotGrob(g2), ggplotGrob(g3))
fig = grid.arrange(grobs = gs, layout_matrix = lay)

ggsave("Figures/AbundanceWeightLength.jpeg",fig,width = 15, height = 10)


write.csv(Results[,-1],"Results/Results.csv", row.names = F)


 
 
