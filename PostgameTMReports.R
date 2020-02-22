#Postgame Analysis

library(dplyr)

Data <- read.csv("C:/Users/epmmo/Downloads/TM6.csv") #Change this!
df <- Data

date1 = unique(Data$Date)[-1] #most recent date

#pitcher whiff rates
pitchers = df%>%
  filter(Date == date1, TaggedPitchType != "Undefined", PitcherTeam == "CAL_MUS")%>%
  mutate(whiff=ifelse(PitchCall == 
                        "StrikeSwinging",1,0), swing = ifelse(
                          PitchCall %in% c("StrikeSwinging", 
                                           "FoulBall", "InPlay"),1,0))%>%
  group_by(Pitcher, TaggedPitchType)%>%
  summarise(whiffrate = sum(whiff)/sum(swing), whiffs = sum(whiff))%>%
  select(Pitcher, TaggedPitchType, whiffrate, whiffs)%>%
  arrange(desc(whiffrate))
print(pitchers, n=100)
#screenshot this

#sequence report
dt = Data%>%
  filter(Date == date1, PitcherTeam == "CAL_MUS")%>%
  arrange(Pitcher, TaggedPitchType, desc(Time))%>%
  select(Pitcher, TaggedPitchType, RelSpeed)

write.csv(dt, "C:/Users/epmmo/Downloads/Jake_Sequence_Report.csv")

dt #check to make sure it's good

table = Data%>%
  filter(PitcherTeam != "CAL_MUS", Date == date1)%>%
  group_by(Pitcher, TaggedPitchType)%>%
  summarise(AverageVelo = round(mean(RelSpeed, na.rm=T),0), 
            LowVelo = round(min(RelSpeed, na.rm=T),0), 
            TopVelo = round(max(RelSpeed, na.rm=T),0))

write.csv(table, "C:/Users/epmmo/Downloads/Teddy_Velos.csv")
