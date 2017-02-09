


#############   A    #############

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(data.table)

#load in police data
police <- fread("BPD.csv")

## PLOT 1 -- HOURLY

#generate arrest hours
police_hours <- mutate(police, arresthour = as.numeric(gsub(":","",police$ArrestTime))%/%100)

#generate age groups
police_hours$ageGroup <- "Under 20"
police_hours$ageGroup[which(police_hours$Age > 20)] <- "20 to 40"
police_hours$ageGroup[which(police_hours$Age > 40)] <- "40 to 60"
police_hours$ageGroup[which(police_hours$Age > 60)] <- "Older than 60"

#fix district unknown label
police_hours$District[which(police_hours$District == "")] <- "Unknown"

#fix sex labels
police_hours$Sex[which(police_hours$Sex == "M")] <- "Male"
police_hours$Sex[which(police_hours$Sex == "F")] <- "Female"

#fix race labels
police_hours$Race[which(police_hours$Race == "B")] <- "Black"
police_hours$Race[which(police_hours$Race == "A")] <- "Asian"
police_hours$Race[which(police_hours$Race == "I")] <- "American Indian"
police_hours$Race[which(police_hours$Race == "U")] <- "Unknown"
police_hours$Race[which(police_hours$Race == "W")] <- "White"

#generate groupings
policeGrouped <- dplyr::group_by(police_hours, arresthour, Sex, Race, District, ageGroup)
p_summarised <- dplyr::summarise(policeGrouped, arrestcount = n())

timeSeriesGroups <- c("Sex", "Race", "District", "Age Group")
timeSeriesAccessors <- c("Sex", "Race", "District", "ageGroup")

## PLOT 2 -- OFFENSE BY YEAR

#format dates correctly
police_hours$ArrestDate <- as.POSIXct(strptime(police_hours$ArrestDate, format = "%m/%d/%Y"))
police_hours$ArrestTime <- as.POSIXct(strptime(as.character(police_hours$ArrestTime), format = "%H:%M"))
police_hours$Year <- as.factor(format(police_hours$ArrestDate, "%Y"))


#pick top offenses
top.offences <- police_hours %>% group_by(IncidentOffense) %>%
  summarise(offence.count = n()) %>% arrange(desc(offence.count)) %>% top_n(10)

#omit unknown offenses
top.offences$IncidentOffense <- as.character(top.offences$IncidentOffense)
top.offences <- as.data.frame(top.offences)
top.offences$IncidentOffense[top.offences$IncidentOffense == "unknownoffense"|
                               top.offences$IncidentOffense == "oth."|
                               top.offences$IncidentOffense == "Unknown Offense"|
                               top.offences$IncidentOffense == "UNKNOWN OFFENSE"|
                               top.offences$IncidentOffense == "other"] <- NA
top.offences <- na.omit(top.offences)

#change labels on offenses
p_offences <- subset(police_hours, police_hours$IncidentOffense %in% top.offences$IncidentOffense)
offence.label <- c("Common Assault","Cut","Narcotics","Narcotics(Outside)",
                   "Seizure","Shop-Lifting","Towed Vehicle")








