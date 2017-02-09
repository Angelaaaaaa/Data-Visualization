library(shinydashboard)
library(sunburstR)
library(wordcloud2)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(networkD3)

library(googleCharts)


dashboardPage(
  dashboardHeader(title = "Group name: GGplot!"),
  dashboardSidebar(
    sidebarMenu(
      #####add side bars#######
      menuItem("Sunburst Plot", tabName = "part1", icon = icon("gear")),
      menuItem("Word Cloud", tabName = "part2", icon = icon("cloud")),
      menuItem("Mosaic Plot", tabName = "part3", icon = icon("film")),
      menuItem("Facet Plot", tabName = "part4", icon = icon("bar-chart")),
      menuItem("Bar Chart", tabName = "part5", icon = icon("bar-chart")),
      menuItem("Network", tabName = "part6", icon = icon("object-group")),
      menuItem("Map", tabName = "part7", icon = icon("map-pin")),
      menuItem("Time Series", tabName = "part8", icon = icon("area-chart")),
      menuItem("Thanks", tabName = "part9", icon = icon("heart"))
    )
  ),
  
  dashboardBody(
    tabItems(
      ################1st graph tab#####################
      tabItem(
        tabName = "part1",
        titlePanel("Sunburst Analysis of BPD Arrest Data"),
        sidebarPanel(
          helpText("By selecting the display option, users can view both percentage and count"),
          selectInput(inputId = "percent",
                      label = "Percent or Count:",
                      choices = c("percent", "count", "percent & count"),
                      selected = "percent"),
          
          helpText("Check the category to display in the sunburst plot"),
          checkboxInput("age", label = "Age", value = T),
          checkboxInput("race", label = "Race", value = T),
          checkboxInput("district", label = "District", value = T),
          checkboxInput("sex", label = "Gender", value = T)
        ),
        mainPanel(
          sunburstOutput("sunburst")
        )
        
        
      ),
      ################2nd graph tab#####################
      tabItem(
        tabName = "part2",
        titlePanel("Wordcloud of Crime Charges"),
        sidebarPanel(helpText("By selecting gender, users could identify the differnce in crime type for each gender"),
                     selectInput(inputId = "gender",
                                 label = "Gender:",
                                 choices = c("Female", "Male", "All"),
                                 selected = "All"),
                     helpText("Coloring option allows users to see the common crime type for each category"),
                     selectInput(inputId = "color",
                                 label = "Color by:",
                                 choices = c("Race", "default"),
                                 selected = "default"),
                     
                     sliderInput("size",
                                 "Word Size (zoom):",
                                 min = 0.1,  max = 1.2, value = 0.3, step = 0.1)),
        mainPanel(wordcloud2Output("wordcloud",width = "100%", height = "500px"),
                  textOutput("text2"))

      ),
      ####################3#########################
      
      tabItem(tabName = "part3",
              
              titlePanel(h4("Mosaic Plot of Baltimore Arrests")),
              sidebarPanel(
                helpText("Use the dropdown menue to choose variables for correlation plot"),
                helpText("For X Axis, we can choose either Race or Sex to display."),
                selectInput("x_select",
                            label = h5("X Axis Option"),
                            choices = c("Race", "Sex"),
                            selected = "Race"),
                helpText("For Y Axis, we can choose time related variables ranging from
                         hour to year. In addition, we can choose Race to display. "),
                selectInput("y_select", 
                            label = h5("Y Axis Option"), 
                            choices = c("Hour", "Weekday", "Month", "Year", "Race"), 
                            selected = "Hour")),
              mainPanel(plotOutput("mosaic"))
              ),
      ############### 4 ##############
      tabItem(tabName = "part4",
              titlePanel("Facet plot of BPD Arrest Data"),
              sidebarPanel(
                helpText("The facet option allows users to compare distributions of different categories"),
                selectInput("FacetType", "Facet Variable", timeSeriesAccessors),
                helpText("The color option users to visualize distributions of different variables given a category"),
                selectInput("ColorType", "Color Variable", timeSeriesAccessors)
              ),
              mainPanel(
                plotOutput("type2", height = 700)
              )
      ),
      ############### 5 #################
      tabItem(tabName = "part5",
              titlePanel("Bar Plot of BPD Arrest Data"),
              sidebarPanel(
                helpText("Users could learn about crime data with respect to each year"),
                selectInput("Year", "Year:", c(2013,2014,2015,2016)),
                helpText("Users could color the plot with respect to different variables"),
                selectInput("ColorType1", "Color Variable", timeSeriesAccessors)
              ),
              mainPanel(
                plotlyOutput("type3", height = 700)
              )
      ),

      ##############6##############
      tabItem(tabName = "part6",
              titlePanel(h3("Baltimore Arrest Network")),
              sidebarLayout(
                sidebarPanel( 
                  helpText("Network Model from randomly selected 100 entries of Baltimore
                           Arrest Dataset. "),
                  sliderInput("opacity", h4("Adjust Opacity"), 0.6, min = 0.1,
                              max = 1, step = .1),
                  helpText("***Notice: Group/Label Option Only Work For Force Newtrok Plot***"),
                  helpText("Select Group Option to change coloring of node."),
                  radioButtons("group", label = h4("Group Choice"),
                               choices = list("Race" = "group1", "Time" = "group2", "Gender" = "group3"),
                               selected = "group1"),
                  helpText("Select Label Option to change labeling name of node."),
                  radioButtons("node", label = h4("Node Label Choice"),
                               choices = list("Race" = "name1", "Time" = "name2", "Gender" = "name3", "Arrests" = "name"),
                               selected = "name1")
                  ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Simple Network", simpleNetworkOutput("simple")),
                    tabPanel("Force Network", forceNetworkOutput("force"))
                  ),
                  hr(),
                  tabPanel('Data', DT::dataTableOutput('mytable1'))
                )
      )),
      ################# 7 ######################
      tabItem(tabName = "part7",
              titlePanel(("Geolocation of Baltimore Arrests")),
              sidebarPanel(
                          helpText("Geolocation Visualization of Arrests From Baltimore Data"),
                          helpText("Choose coloring option to view arrests"),
                          selectInput("select",
                                       label = h5("Color Option"),
                                       choices = c("Race", "Sex", "Hour"),
                                       selected = "Race"),
                          helpText("Adjust Zoom Value to change details of the map"), 
                           sliderInput(inputId = "zoom", label = h5("Zoom"),
                                       min = 12, max = 14, value = 12),
                          helpText("Toner is a special theme. Click the checkbox to enable Toner Theme."),
                           checkboxInput("checkbox", label = "Toner", value = FALSE)),
              mainPanel(plotOutput("waterColorPlot"))
      ),
      ###################### 8 ##################################
      tabItem(tabName = "part8",
              titlePanel("Time Series of BPD Arrest Data"),
              selectInput(inputId = "by",
                                       label = "Color the plot by:",
                                       choices = c("Race", "Gender", "All"),
                                       selected = "All"),
              textOutput("text1"),
              plotlyOutput("trendPlot")
              ),
      tabItem(tabName = "part9",
              h1(),
              h1(),
              h6("Yeah!", align = "center"),
              h6("Final Project!", align = "center"),
              h5("We are an amazing group!", align = "center"),
              h4("We love statistics and graphs!", align = "center"),
              h3("315 is amazing and interesting!", align = "center"),
              h2("We love you Sam! You are the best!", align = "center"),
              h1("Thank you for the amazing teaching!", align = "center")
              
      )
    )
  )
)