library(ggplot2)
library(ggmap)
library(networkD3)
library(sunburstR)
library(tidytext)
library(wordcloud)
library(wordcloud2) 
library(dplyr)
library(data.table)
library(magrittr)

library(shiny)
library(ggmap)
library(tidyr)
library(dplyr)
library(data.table)
library(devtools)
library(ggplot2)
library(curl)
custom.color2 <- c("#F4AC45","#FF0022","#A61C3C","#9191E9","#04471C")
zihang <- fread('https://raw.githubusercontent.com/gzhami/data_visualization/master/arrests_v3.csv')

data(stop_words)
BPD <- read.csv("BPD.csv",stringsAsFactors = FALSE)
BPD$ChargeDescription <- as.character(BPD$ChargeDescription)
BPD$Arrest <- as.numeric(BPD$Arrest)
BPD$Sex[which(BPD$Sex == "M")] <- "Male"
BPD$Sex[which(BPD$Sex == "F")] <- "Female"
BPD$Race[which(BPD$Race == "B")] <- "Black"
BPD$Race[which(BPD$Race == "W")] <- "White"
BPD$Race[which(BPD$Race == "A")] <- "Asian"
BPD$Race[which(BPD$Race == "I")] <- "Indian"
BPD$Race[which(BPD$Race == "U")] <- "Other"
BPD$District[which(BPD$District == "")] <- "Other"
BPD$date <- as.Date(BPD$ArrestDate, "%m/%d/%Y")

zihang_315_theme <- theme(
  axis.text = element_text(size = 12, family = 'mono', colour = 'black'),
  axis.title = element_text(size = 12, family = 'mono', colour = 'black'),
  legend.title = element_text(size = 10, family = 'mono', colour = 'black'),
  plot.title = element_text(size = 14,  family = 'mono', colour = 'black'),
  legend.text = element_text(size = 8, family = 'mono', colour = 'black'),
  legend.key = element_rect(fill = "white"),
  legend.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = "grey"),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white")
)
source_url("https://raw.githubusercontent.com/sventura/315-code-and-datasets/master/code/geom_mosaic.R") 


shinyServer(function(input,output,session){
  ########################1st sunburst#####################################
  output$sunburst <- renderSunburst({
    newdata <- as.data.frame(BPD$Sex)
    names(newdata) <- "V1"
    newdata$V1 <- ""
    
    if (input$sex){
      newdata$V1 <- paste(newdata$V1,BPD$Sex, sep="-")
    }
    if (input$race){
      newdata$V1 <- paste(newdata$V1,BPD$Race, sep="-")
    }
    if (input$district){
      newdata$V1 <- paste(newdata$V1,BPD$District, sep="-")
    }
    if (input$age){
      newdata$V1 <- paste(newdata$V1,BPD$Age, sep="-")
    }
    t<- as.data.frame(table(newdata$V1))
    if (as.character(input$percent) == "percent"){
      sunburst(t, percent = T, count = F)
    }
    else if (as.character(input$percent) == "percent & count"){
      sunburst(t, percent = T, count = T)
    }
    else{
      sunburst(t, percent = F, count = T)
    }
  })
  ################2nd  wordcloud##########################
  output$wordcloud <- renderWordcloud2({
    gender <- as.character(input$gender)
    s <- as.numeric(input$size)
    c <- as.character(input$color)
    basecolors <- c("lightblue", "pink", "white", "yellow", "lighgrey")


    if (c == "default"){
      if (gender == "All"){
        my_tweets <- dplyr::select(BPD,Arrest, ChargeDescription) %>%
          unnest_tokens(word, ChargeDescription) %>%
          anti_join(stop_words) %>%
          count(word)

        newdata <- my_tweets[order(my_tweets$n, decreasing = T),]
        newdata <- newdata[c(1:500),]
        newdata$n <- round(sqrt((newdata$n)))

        wordcloud2(newdata, color='random-light' ,size=s, backgroundColor="black", minRotation = -pi/2, maxRotation = pi/2, shape = "star")
      }

      else if (gender == "Male"){
        BPDm <- BPD[which(BPD$Sex == gender), ]
        my_tweets <- dplyr::select(BPDm,Arrest, ChargeDescription) %>%
          unnest_tokens(word, ChargeDescription) %>%
          anti_join(stop_words) %>%
          count(word)
        newdata <- my_tweets[order(my_tweets$n, decreasing = T),]
        newdata <- newdata[c(1:500),]
        newdata$n <- round(sqrt((newdata$n)))

        wordcloud2( newdata, color='random-light' ,size=s, backgroundColor="black", minRotation = -pi/2, maxRotation = pi/2, shape = "diamond")
      }

      else if (gender == "Female"){
        BPDf <- BPD[which(BPD$Sex == gender), ]
        my_tweets <- dplyr::select(BPDf,Arrest, ChargeDescription) %>%
          unnest_tokens(word, ChargeDescription) %>%
          anti_join(stop_words) %>%
          count(word)
        newdata <- my_tweets[order(my_tweets$n, decreasing = T),]
        newdata <- newdata[c(1:500),]
        newdata$n <- round(sqrt((newdata$n)))

        wordcloud2( newdata, color='random-light' ,size=s, backgroundColor="black", minRotation = -pi/2, maxRotation = pi/2, shape = "triangle")
      }
    }
    else {
      if (gender == "All"){
        my_tweets <- dplyr::select(BPD,Arrest, ChargeDescription, Race) %>%
          unnest_tokens(word, ChargeDescription) %>%
          anti_join(stop_words) %>%
          count(Race, word, sort = TRUE) %>%
          ungroup()

        newdata <- my_tweets[order(my_tweets$n, decreasing = T),]
        newdata <- newdata[c(1:500),]
        newdata$n <- round(sqrt((newdata$n)))
        #basecolors <- c("red", "blue", "white", "yellow", "green")
        colorlist = basecolors[ match(newdata$Race,unique(newdata$Race)) ]
        wordcloud2( newdata[,c(2,3)], color= colorlist ,size=s, backgroundColor="black", minRotation = -pi/2, maxRotation = pi/2, shape = "star")
      }

      else if (gender == "Male"){
        BPDm <- BPD[which(BPD$Sex == gender), ]
        my_tweets <- dplyr::select(BPDm,Arrest, ChargeDescription, Race) %>%
          unnest_tokens(word, ChargeDescription) %>%
          anti_join(stop_words) %>%
          count(Race, word, sort = TRUE) %>%
          ungroup()

        newdata <- my_tweets[order(my_tweets$n, decreasing = T),]
        newdata <- newdata[c(1:500),]
        newdata$n <- round(sqrt((newdata$n)))

        colorlist = basecolors[ match(newdata$Race,unique(newdata$Race)) ]
        wordcloud2( newdata[,c(2,3)], color= colorlist ,size=s, backgroundColor="black", minRotation = -pi/2, maxRotation = pi/2, shape = "diamond")
      }

      else if (gender == "Female"){
        BPDf <- BPD[which(BPD$Sex == gender), ]
        my_tweets <- dplyr::select(BPDf,Arrest, ChargeDescription, Race) %>%
          unnest_tokens(word, ChargeDescription) %>%
          anti_join(stop_words) %>%
          count(Race, word, sort = TRUE) %>%
          ungroup()

        newdata <- my_tweets[order(my_tweets$n, decreasing = T),]
        newdata <- newdata[c(1:500),]
        newdata$n <- round(sqrt((newdata$n)))
        #basecolors <- c("red", "blue", "white", "yellow", "green")
        colorlist = basecolors[ match(newdata$Race,unique(newdata$Race)) ]
        wordcloud2( newdata[,c(2,3)], color=colorlist ,size=s, backgroundColor="black", minRotation = -pi/2, maxRotation = pi/2, shape = "triangle")
      }
    }

  })

  #############3###########
  output$mosaic <- renderPlot({
    arrest_v2 <- fread('https://raw.githubusercontent.com/gzhami/data_visualization/master/arrest_v2.csv')
    x_option <- as.character(input$x_select)
    y_option <-as.character(input$y_select)
    if (x_option == "Sex") {
      if (y_option == "Race") {
        ggplot(arrest_v2, aes(x = arrest_v2$"Sex", y = arrest_v2$"Race")) + 
          geom_mosaic() + 
          mosaic_legend() + 
          labs(x = "", y = "") + 
          zihang_315_theme + 
          theme(axis.text.x=element_text(angle = 90, hjust=1)) 
      }
      else if (y_option == "Hour") {
        ggplot(arrest_v2, aes(x = arrest_v2$"Sex", y = arrest_v2$"Hour")) + 
          geom_mosaic() + 
          mosaic_legend() + 
          labs(x = "", y = "") + 
          zihang_315_theme + 
          theme(axis.text.x=element_text(angle = 90, hjust=1)) 
      }
      else if (y_option == "Weekday") {
        ggplot(arrest_v2, aes(x = arrest_v2$"Sex", y = arrest_v2$"Weekday")) + 
          geom_mosaic() + 
          mosaic_legend() + 
          labs(x = "", y = "") + 
          zihang_315_theme + 
          theme(axis.text.x=element_text(angle = 90, hjust=1)) 
      }
      else if (y_option == "Year") {
        ggplot(arrest_v2, aes(x = arrest_v2$"Sex", y = arrest_v2$"Year")) + 
          geom_mosaic() + 
          mosaic_legend() + 
          labs(x = "", y = "") + 
          zihang_315_theme + 
          theme(axis.text.x=element_text(angle = 90, hjust=1)) 
      }
      else if (y_option == "Month") {
        ggplot(arrest_v2, aes(x = arrest_v2$"Sex", y = arrest_v2$"Month")) + 
          geom_mosaic() + 
          mosaic_legend() + 
          labs(x = "", y = "") + 
          zihang_315_theme + 
          theme(axis.text.x=element_text(angle = 90, hjust=1)) 
      }
    }
    else if (x_option == "Race") {
      if (y_option == "Race") {
        ggplot(arrest_v2, aes(x = arrest_v2$"Race", y = arrest_v2$"Race")) + 
          geom_mosaic() + 
          mosaic_legend() + 
          labs(x = "", y = "") + 
          zihang_315_theme + 
          theme(axis.text.x=element_text(angle = 90, hjust=1))
      }
      else if (y_option == "Hour") {
        ggplot(arrest_v2, aes(x = arrest_v2$"Race", y = arrest_v2$"Hour")) + 
          geom_mosaic() + 
          mosaic_legend() + 
          labs(x = "", y = "") + 
          zihang_315_theme + 
          theme(axis.text.x=element_text(angle = 90, hjust=1))
      }
      else if (y_option == "Weekday") {
        ggplot(arrest_v2, aes(x = arrest_v2$"Race", y = arrest_v2$"Weekday")) + 
          geom_mosaic() + 
          mosaic_legend() + 
          labs(x = "", y = "") + 
          zihang_315_theme + 
          theme(axis.text.x=element_text(angle = 90, hjust=1))
      }
      else if (y_option == "Month") {
        ggplot(arrest_v2, aes(x = arrest_v2$"Race", y = arrest_v2$"Month")) + 
          geom_mosaic() + 
          mosaic_legend() + 
          labs(x = "", y = "") + 
          zihang_315_theme + 
          theme(axis.text.x=element_text(angle = 90, hjust=1))
      }
      else if (y_option == "Year") {
        ggplot(arrest_v2, aes(x = arrest_v2$"Race", y = arrest_v2$"Year")) + 
          geom_mosaic() + 
          mosaic_legend() + 
          labs(x = "", y = "") + 
          zihang_315_theme + 
          theme(axis.text.x=element_text(angle = 90, hjust=1))
      }
    }
  })
  ############# 4 ###########################
  output$type2 <- renderPlot({

    #PLOT 1 -- HOURLY
    myvar <- input$ColorType

    if(input$ColorType == "Sex"){
      ggplot(p_summarised) + aes(x = arresthour, y = arrestcount, fill = Sex) + geom_histogram(stat = "identity") + facet_wrap(input$FacetType, ncol = 1, scales = "free_y") + labs(x = "Arrest Hour", y = "Arrest Count", title = paste("Arrests over Time, faceted by",input$FacetType))
    }

    else if(input$ColorType == "Race"){
      ggplot(p_summarised) + aes(x = arresthour, y = arrestcount, fill = Race) + geom_histogram(stat = "identity") + facet_wrap(input$FacetType, ncol = 1, scales = "free_y") + labs(x = "Arrest Hour", y = "Arrest Count", title = paste("Arrests over Time, faceted by",input$FacetType))
    }

    else if(input$ColorType == "District"){
      ggplot(p_summarised) + aes(x = arresthour, y = arrestcount, fill = District) + geom_histogram(stat = "identity") + facet_wrap(input$FacetType, ncol = 1, scales = "free_y") + labs(x = "Arrest Hour", y = "Arrest Count", title = paste("Arrests over Time, faceted by",input$FacetType))
    }

    else if(input$ColorType == "ageGroup"){
      ggplot(p_summarised) + aes(x = arresthour, y = arrestcount, fill = ageGroup) + geom_histogram(stat = "identity") + facet_wrap(input$FacetType, ncol = 1, scales = "free_y") + labs(x = "Arrest Hour", y = "Arrest Count", title = paste("Arrests over Time, faceted by",input$FacetType))
    }

  })
  ##################### 5 ###############
  output$type3 <- renderPlotly({

    p_offences_year <- filter(p_offences, Year == input$Year)

    if(input$ColorType1 == "Sex"){
      ggplot(p_offences_year) + aes(x = IncidentOffense, fill = Sex) + geom_bar() + labs(y = "Arrest Count",  x = "Incident Offense", title = paste("Top Arrest Incident Types for Year ",toString(input$Year))) + zihang_315_theme + theme(axis.text.x = element_text(angle = 45, vjust = 0.2))
    }

    else if(input$ColorType1 == "Race"){
      ggplot(p_offences_year) + aes(x = IncidentOffense, fill = Race) + geom_bar() + labs(y = "Arrest Count",  x = "Incident Offense", title = paste("Top Arrest Incident Types for Year ",toString(input$Year))) + zihang_315_theme + theme(axis.text.x = element_text(angle = 45, vjust = 0.2))
    }

    else if(input$ColorType1 == "District"){
      ggplot(p_offences_year) + aes(x = IncidentOffense, fill = District) + geom_bar() + labs(y = "Arrest Count",  x = "Incident Offense", title = paste("Top Arrest Incident Types for Year ",toString(input$Year))) + zihang_315_theme + theme(axis.text.x = element_text(angle = 45, vjust = 0.2))
    }

    else if(input$ColorType1 == "ageGroup"){
      ggplot(p_offences_year) + aes(x = IncidentOffense, fill = ageGroup) + geom_bar() + labs(y = "Arrest Count",  x = "Incident Offense", title = paste("Top Arrest Incident Types for Year ",toString(input$Year))) + zihang_315_theme + theme(axis.text.x = element_text(angle = 45, vjust = 0.2))
    }

  })
  ####################### 6 ########################
  data <- fread('https://raw.githubusercontent.com/gzhami/data_visualization/master/data.csv')
  links <- fread('https://raw.githubusercontent.com/gzhami/data_visualization/master/links.csv')
  nodes <- fread('https://raw.githubusercontent.com/gzhami/data_visualization/master/nodes.csv')
  output$simple <- renderSimpleNetwork({
    src <- links$source
    target <- links$target
    networkData <- data.frame(src, target)
    simpleNetwork(networkData, opacity = input$opacity)
  })

  output$force <- renderForceNetwork({
    forceNetwork(Links = links, Nodes = nodes, Source = "source",
                 Target = "target", Value = "value", NodeID = input$node,
                 Group = input$group, opacity = input$opacity)

  })

  output$mytable1 <- DT::renderDataTable({
    DT::datatable(subset(data,select = c("Age", "Sex", "Race", 'ArrestDate', 'ArrestLocation', 'District', 'Hour')))
  })
  ###################### 7 ############################
  output$waterColorPlot <- renderPlot({
    if (as.character(input$checkbox) == TRUE) {
      if (as.character(input$select) == "Race") {
        if (input$zoom == 12) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 12,
               maptype = "toner") + geom_point(data = zihang,
                                               aes(x = Longitude,
                                                   y = Latitude,
                                                   color = Race))

        }
        else if (input$zoom == 13) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 13,
               maptype = "toner") + geom_point(data = zihang,
                                               aes(x = Longitude,
                                                   y = Latitude,
                                                   color = Race))
        }
        else if (input$zoom == 14) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 14,
               maptype = "toner") + geom_point(data = zihang,
                                               aes(x = Longitude,
                                                   y = Latitude,
                                                   color = Race))
        }

      }
      #### Coloring Race ####

      else if (as.character(input$select) == "Sex") {
        if (input$zoom == 12) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 12,
               maptype = "toner") + geom_point(data = zihang,
                                               aes(x = Longitude,
                                                   y = Latitude,
                                                   color = Sex))
        }
        else if (input$zoom == 13) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 13,
               maptype = "toner") + geom_point(data = zihang,
                                               aes(x = Longitude,
                                                   y = Latitude,
                                                   color = Sex))
        }
        else if (input$zoom == 14) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 14,
               maptype = "toner") + geom_point(data = zihang,
                                               aes(x = Longitude,
                                                   y = Latitude,
                                                   color = Sex))
        }

      }

      ###### Coloring Sex ########
      else if (as.character(input$select) == "Hour") {
        if (input$zoom == 12) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 12,
               maptype = "toner") + geom_point(data = zihang,
                                               aes(x = Longitude,
                                                   y = Latitude,
                                                   color = Hour))
        }
        else if (input$zoom == 13) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 13,
               maptype = "toner") + geom_point(data = zihang,
                                               aes(x = Longitude,
                                                   y = Latitude,
                                                   color = Hour))
        }
        else if (input$zoom == 14) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 14,
               maptype = "toner") + geom_point(data = zihang,
                                               aes(x = Longitude,
                                                   y = Latitude,
                                                   color = Hour))
        }
      }
      ##### Coloring Hours
    }
    else if ((as.character(input$checkbox) == FALSE)) {
      if (as.character(input$select) == "Race") {
        if (input$zoom == 12) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 12) + geom_point(data = zihang,
                                                                                        aes(x = Longitude,
                                                                                            y = Latitude,
                                                                                            color = Race))
        }
        else if (input$zoom == 13){
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 13) + geom_point(data = zihang,
                                                                                        aes(x = Longitude,
                                                                                            y = Latitude,
                                                                                            color = Race))
        }
        else if (input$zoom == 14) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 14) + geom_point(data = zihang,
                                                                                        aes(x = Longitude,
                                                                                            y = Latitude,
                                                                                            color = Race))
        }
      }
      ###### Coloring Race #####

      else if (as.character(input$select) == "Sex") {
        if (input$zoom == 12) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 12) + geom_point(data = zihang,
                                                                                        aes(x = Longitude,
                                                                                            y = Latitude,
                                                                                            color = Sex))
        }
        else if (input$zoom == 13) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 13) + geom_point(data = zihang,
                                                                                        aes(x = Longitude,
                                                                                            y = Latitude,
                                                                                            color = Sex))
        }
        else if (input$zoom == 14) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 14) + geom_point(data = zihang,
                                                                                        aes(x = Longitude,
                                                                                            y = Latitude,
                                                                                            color = Sex))
        }
      }
      ##### Coloring Sex ######

      else if (as.character(input$select) == "Hour") {
        if (input$zoom == 12) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 12) + geom_point(data = zihang,
                                                                                        aes(x = Longitude,
                                                                                            y = Latitude,
                                                                                            color = Hour))
        }

        else if (input$zoom == 13){
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 13) + geom_point(data = zihang,
                                                                                        aes(x = Longitude,
                                                                                            y = Latitude,
                                                                                            color = Hour))
        }

        else if (input$zoom == 14) {
          qmap("Baltimore", maprange = TRUE, source = "stamen", zoom = 14) + geom_point(data = zihang,
                                                                                        aes(x = Longitude,
                                                                                            y = Latitude,
                                                                                            color = Hour))
        }
      }
      ### Coloring : Hour ###
    }
    ### Checkbox : Toner is TRUE
  })
  # ####################### 8 ###################
  output$trendPlot <- renderPlotly({
    if (input$by == "All"){
      by_date <- dplyr::group_by(BPD, date)
      arrest_per_day <- dplyr::summarize(by_date, arrests = n())
      
      ggplot(arrest_per_day) +
        geom_line(aes(x = date, y = arrests), color = "navy") +
        labs(x = "Year", y = "Count", title = "Time Series for Number of Arrest Over Years") +
        zihang_315_theme
    }
    else if(input$by == "Gender"){
      by_date <- dplyr::group_by(BPD, date, Sex)
      arrest_per_day <- dplyr::summarize(by_date, arrests = n())
      
      ggplot(arrest_per_day) +
        geom_line(aes(x = date, y = arrests, color = Sex)) +
        labs(x = "Year", y = "Count", title = "Time Series for Number of Arrest Over Years") +
        zihang_315_theme
    }
    else if(input$by == "Race"){
      by_date <- dplyr::group_by(BPD, date, Race)
      arrest_per_day <- dplyr::summarize(by_date, arrests = n())
      
      ggplot(arrest_per_day) +
        geom_line(aes(x = date, y = arrests, color = Race)) +
        labs(x = "Year", y = "Count", title = "Time Series for Number of Arrest Over Years") +
        zihang_315_theme
    }
    
 })
  output$text1 <- renderText({ 
    "The time series generates a overall view of how the crime changes over time. 
    By coloring with race and gender, users could obtain a complete understanding of
    the data set"
  })
  
  output$text2 <- renderText({ 
    "Color by Race - Black: pink, White: blue, Unknown: white, Asian: yellow, Indian: green"
  })
  
})

