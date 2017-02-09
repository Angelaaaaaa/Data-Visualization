
# ######################     M    ###################
# arrests <- read.csv("BPD.csv")
# 
# condition   <- c("COMMON ASSAULT","TRESSPASSING", "DESTRUCTION OF PROPERTY","LARCENY","ASSAULT","CDS","BURGLARY","HANDGUN VIOLATION","ROBBERY","CDS POSSESSION","ARMED ROBBERY","2ND DEGREE ASSAULT","STOLEN AUTO","DOMESTIC ASSAULT","ASSAULT OF POLICE")
# subset <- filter(arrests, arrests$ChargeDescription %in% condition)
# 
# cond <- c("Central","Eastern" ,"Northeastern", "Northern", "Northwestern", "Southeastern", "Southern", "Southwestern", "Western")
# 
# subset <- filter(subset, District %in% cond)
# 
# subset$Sex = factor(subset$Sex, labels = c("F" = "Female","M" ="Male"))
# 
# sf <- subset
# sf$ChargeDescription<- "All"
# 
# subset <- rbind(sf, subset)
# 



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


# ######### M2 ###############
# 
# arrests <- police
# #arrests$ArrestTime <- as.POSIXct(strptime(as.character(arrests$ArrestTime), format = "%H:%M"))
# 
# arrests[arrests==""] <- NA
# arrests <- na.omit(arrests)
# arrests$date <- as.Date(arrests$ArrestDate, "%m/%d/%Y")
# arrests$ArrestTime <- as.integer(arrests$ArrestTime)
# 
# 
# arrests_subset_michelle <- subset(arrests, select =  c(District, Age, ArrestTime, Race, Sex,Post,Charge, date, ChargeDescription))
# arrests_subset_michelle$District <- as.character(arrests_subset_michelle$District)
# arrests_subset_michelle$Age <- as.numeric(arrests_subset_michelle$Age)
# arrests_subset_michelle$ArrestTime <- as.numeric(arrests_subset_michelle$ArrestTime)
# arrests_subset_michelle$Race <- as.character(arrests_subset_michelle$Race)
# arrests_subset_michelle$Charge <- as.numeric(arrests_subset_michelle$Charge)
# 
# 
# 
# arrests_subset_michelle$Race = factor(arrests_subset_michelle$Race, labels = c("A" = "Asian","B" ="Black","I"= "Indian","U"= "Unknown","W"= "White"))
# arrests_subset_michelle$Race = factor(arrests_subset_michelle$Race, levels = c("Black", "White", "Asian","Indian","Unknown"))
# 
# arrests_subset_michelle$District <- factor(arrests_subset_michelle$District, levels = c("Central","Eastern" ,"Northeastern", "Northern", "Northwestern", "Southeastern", "Southern", "Southwestern", "Western")) 
# #Arrest$ArrestTime <- (Arrest$ArrestTime, labels = c("500" = "5 ", "1000" = "10 ", "1500" = "3 ", "2000" = "8 ")) 
# 

################# A ####################







