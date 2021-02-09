

ui <- dashboardPage(
    
    skin = "red",
    dashboardHeader(title = "Smart City Dashboard"),
    
    dashboardSidebar(
        sidebarMenu(
            
################################################## Introduction ##################################################           
            
            menuItem("Introduction",
                     tabName = "Introduction",icon = icon("info")),
            
            
################################################## Cycle Dashboard ##################################################  
            
            
            menuItem("Cycle Dashboard",
                     icon = icon("bicycle"),
                     menuSubItem(tabName = "Cycledashboard",
                                 sliderInput(inputId = "n_date",
                                             label = "Date:",
                                             min = as.Date("2016-06-16","%Y-%m-%d"),
                                             max = as.Date("2020-12-07","%Y-%m-%d"),
                                             #value=as.Date("2018-01-01"),
                                             timeFormat="%Y-%m-%d",
                                             value = c(as.Date("2018-06-16","%Y-%m-%d"), as.Date("2018-06-18","%Y-%m-%d")))
                     ),
                     menuSubItem("Plots", 
                                 tabName = "Plots")),
            
            
################################################## Parking Dashboard ##################################################              
            
            
            menuItem("Parking Dashboard", icon = icon("car"),
                     menuSubItem(
                         tabName = "Parking",
                         sliderInput(inputId = "p_date",
                                     label = "Date:",
                                     min = as.Date("2019-12-30","%Y-%m-%d"),
                                     max = as.Date("2020-12-06","%Y-%m-%d"),
                                     timeFormat="%Y-%m-%d",
                                     value = c(as.Date("2020-06-30","%Y-%m-%d"), as.Date("2020-07-10","%Y-%m-%d")))),
                     
                     menuSubItem("Plots", 
                                 tabName = "pplots")),
                     
                    
            
################################################## Prediction ##################################################     

        menuItem("Cycle Prediction", tabName = "pred", icon = icon('search')),
        
        menuItem("Parking Prediction", tabName = "park_pred", icon = icon('search')))),
        
    
################################################## Dashboard Body ##################################################  
    
################################################## Introduction ##################################################  
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "Introduction",
                    
                    br(),br(),
                    img(src='https://miro.medium.com/max/700/1*oFQLs6z-B7o1NHKjpteSjw.png',style="display: block ; margin-left: auto; margin-right: auto;"),
                    h6(em("source :https://medium.com/zify/the-importance-of-smart-cities-2a4f7f89a6cd"), align = "center"),
                    br(),br(),
                    h4(em("The project aims to visualise Christchurch's cycling data in recent years and parking data from seven 
                          car parks in Christchurch city centre in 2020, and to study the impact of weather 
                          (including temperature, humidity and wind speed) on people's commuting preferences, 
                          as well as the impact of weekdays or weekends. ")),
                    h3(em("Cycle Dashboard:")),
                    h4(em("The Cycle Dashboard shows each of the sensor location on the map and the number of observations for 
                          that counter or nearby counters for the selected date range and few plots that shows the effect of 
                          Weather on the count of bicycles.")),
                    h3(em("Parking Dashboard:")),
                    h4(em("The Parking Dashboard shows the location of each car park on the map and the number of observations for 
                          that car park for the selected date range and few plots that shows the effect of 
                          Weather on the usage of car parks.")),
                    h3(em("Cycle Prediction:")),
                    h4(em("This tab gives us a prediction of the  count of bicycle considering the given input, and gives us
                          a plot to show the comparison with historic data.")),
                    h3(em("Parking Prediction:")),
                    h4(em("This tab gives us a prediction of the  Percentage of Occupied Parking Slots considering the given input, and gives us
                          a plot to show the comparison with historic data.")),
                    br(),
                    h5(em("By: Varun Manjunath Sakhare & Xiaoxi Guo"), align = "right"),
     ################################################## Cycle ##################################################                    
            ),
            tabItem(tabName = "Cycledashboard",
                    
                    fluidRow(box(title = "Cycle Counters on Map",status = "primary",collapsible = TRUE,
                                 solidHeader = TRUE,width = 12, leafletOutput(outputId = "map"))),
                    h4(em("The map shows the location of each counter and the number of observations in total during the selected date range.")),
                    br(),
                    fluidRow(valueBoxOutput("infoboxnum"),valueBoxOutput("infoboxmin"),valueBoxOutput("infoboxmax")),
                   fluidRow(box(title = "Distribution of Cycle Count - Ridge",status = "primary",collapsible = TRUE,
                        solidHeader = TRUE,width = 12, plotOutput(outputId = "ridge_plot"))),
                   h4(em("This plot shows the distribution of the number of bicycles at each location for the selected date range.")),
                   br(),
                    splitLayout(
                        fluidRow(box(title = "Scatter Plot for each counter for the selected Date Range",status = "primary",collapsible = TRUE,
                                     solidHeader = TRUE,width = 12, plotOutput(outputId = "plot"))),
                        fluidRow(box(title = "Change in Bicycle Count from 2016 to 2020",status = "primary",collapsible = TRUE,
                                     solidHeader = TRUE,width = 12, img(src="Ridge.gif")))),
                   h4(em("A scatter plot to show the count of bicycles at each location for the selected date range. And a Ridge Plot that is in 
                         the form of a GIF to show the distribution for each location from 2016 to 2020")),
                   br(),
                    
                        fluidRow(box(title = "Data Table for the selected date range",status = "primary",collapsible = TRUE,
                                     solidHeader = TRUE,width = 12, dataTableOutput(outputId = "summary_table")))
                    

                    
            ),
            tabItem('Plots',
                    
                    
                    fluidRow(box(checkboxGroupInput('CycleWeek', "Select Week:", c(as.character(unique(OID$Week))),selected = 'Weekday'))),
                    
                    fluidRow(box(title = "Effect of Weather on Bicycle Count",status = "primary",collapsible = TRUE,
                                 solidHeader = TRUE,width = 12, plotOutput(outputId = "week_plot"))),
                    h4(em("Shows the distribution of cycling data across each month for different temperatures.")),
                    br(),
                    fluidRow(valueBox(uiOutput("first"), "Number of Counters in 2016", icon = icon("bicycle"),width = 3),
                             valueBox(uiOutput("second"), "Number of Counters in 2016", icon = icon("bicycle"),width = 3),
                             valueBox(uiOutput("third"), "Number of Counters in 2018", icon = icon("bicycle"),width = 3),
                             valueBox(uiOutput("fourth"), "Number of Counters in 2019", icon = icon("bicycle"),width = 3),
                             valueBox(uiOutput("fifth"), "Number of Counters in 2020", icon = icon("bicycle"),width = 3)),
                    h4(em("Each box shows the total number of counters that were active for that particular year.")),
                    br(),
                    fluidRow(box(title = "Number of Bicycles Across Each Season",status = "primary",collapsible = TRUE,
                                 solidHeader = TRUE,width = 12, plotOutput(outputId = "box_plot")))
                    
                    ),
            
            ################################################## Parking ##################################################  
            
            tabItem(tabName = "Parking",
                    fluidRow(box(title = "Parking Data on Map",status = "primary",collapsible = TRUE,
                                 solidHeader = TRUE,width = 12, leafletOutput(outputId = "pmap"))),
                    h4(em("The map shows the location of the seven car parks in Christchurch city centre and the number 
                          of observations for each of these seven car parks.")),
                    br(),
                    
                    fluidRow(box(title ="Percentage of Occupied Slots - Ridge",status = "primary",collapsible = TRUE,
                                 solidHeader = TRUE,width = 12, plotOutput(outputId = "perc_occupied"))),
                    h4(em("This plot shows the distribution of the percentage of occupied slots at each location for the selected date range.")),
                    br(),
                    fluidRow(box(title = "Data Table for the selected date range",status = "primary",collapsible = TRUE,
                                 solidHeader = TRUE,width = 12, dataTableOutput(outputId = "summary_table_park")))),
            
            tabItem('pplots',
        
                    fluidRow(box(selectInput("location", "Select Location:",
                                             c(unique(as.character(Parking$Location))),multiple = TRUE,selected = "Hereford St"))),
                    fluidRow(box(selectInput("Week", "Select Week:",
                                             c(unique(as.character(Parking$Week))),multiple = TRUE,selected = "Weekday"))),
       
                        fluidRow(box(title = "Percentage of Occupied Slot with Temperature",status = "primary",collapsible = TRUE,
                                     solidHeader = TRUE,width = 12, plotOutput(outputId = "perc_dot"))),
                    h4(em("A scatter plot to show the percentage of Occupied slots across Temperature")),
                    br(),
                        fluidRow(box(title = "Percentage of Occupied Slot with Temperature at each Hour",status = "primary",collapsible = TRUE,
                                     solidHeader = TRUE,width = 12, plotOutput(outputId = "perc_hist"))),

            
            ),

            ################################################## Prediction ##################################################              
            
            tabItem('pred',
                    box(title = 'Predictor Variables', 
                        status = 'primary', width = 12, 
                        splitLayout(
                            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                            cellWidths = c('0%', '4%','21%', '4%', '21%', '4%', '21%','4%','18%'),
                            div(),
                            selectInput( 'c_yr', 'Year', c("2021","2022")),
                            div(),
                            selectInput( 'c_mnth', 'Months', c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec")),
                            div(),
                            selectInput( 'c_weekd', 'WeekDay', c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')),
                            div(),
                            selectInput( 'c_loc', 'Location', c(unique(as.character(OID$Name))),selected = "Eco Display Sensor 1")),
                    box(status = 'primary', width = 12,
                        splitLayout(cellWidths = c('22%', '4%','21%', '4%', '21%', '4%', '21%'),
                                    sliderInput( 'c_hum', 'Humidity (%)', min = 0, max = 100, value = 30),
                                    div(),
                                    sliderInput( 'c_temp', 'Temperature (Celsius)', min = 0, max = 25, value = 15),
                                    div(),
                                    sliderInput( 'c_wind', 'Wind speed (kmph)', min = 0, max = 50, value = 10)),
                    actionButton('cal','Calculate', icon = icon('calculator')),align ="center"),
                    #Box to display the prediction results
                    box(title = 'Prediction result',
                        status = 'success', 
                        solidHeader = TRUE, 
                        width = 4, height = 260,
                        div(h5('Total Count of the Bicycle')),
                        verbatimTextOutput("value", placeholder = TRUE),
                        div(h5('Range for the Count of Bicycle:')),
                        verbatimTextOutput("range_bike", placeholder = TRUE))),
                    fluidRow(box(title = 'Comparison with Historic Data',
                                 status = 'success',
                                 width = 12,
                                 solidHeader = TRUE,
                                 plotOutput("c_ts")))),
            
            tabItem('park_pred',
                    box(title = 'Predictor Variables', 
                        status = 'primary', width = 12, 
                        splitLayout(
                            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                            cellWidths = c('0%', '4%', '19%', '4%', '19%', '4%', '19%', '4%', '15%'),
                            div(),
                            selectInput( 'p_yr', 'Year', c("2021","2022")),
                            div(),
                            selectInput( 'p_mnth', 'Months', c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec")),
                            div(),
                            selectInput('p_hr', 'Hour', c('0', '1', '2', '3', '4', '5', '6', '7', '8','9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23'),selected = '15'),
                            div(),
                            selectInput( 'p_loc', 'Location', c(unique(as.character(Parking$Location))),selected = "Hereford St"),
                            div(),
                            radioButtons( 'p_week', 'Week', c('Weekend', 'Weekday'))),
                    box(status = 'primary', width = 12,
                    splitLayout(cellWidths = c('22%', '4%','21%', '4%', '21%', '4%', '21%'),
                                    sliderInput( 'p_hum', 'Humidity (%)', min = 0, max = 100, value = 20),
                                    div(),
                                    sliderInput( 'p_temp', 'Temperature (Celsius)', min = 0, max = 25, value = 15),
                                    div(),
                                    sliderInput( 'p_wind', 'Wind speed (kmph)', min = 0, max = 50, value = 10)),
                        actionButton('parking_cal','Calculate', icon = icon('calculator')),align = "center"),
                    #Box to display the prediction results
                    box(title = 'Prediction result',
                        status = 'success', 
                        solidHeader = TRUE, 
                        width = 4, height = 200,
                        div(h5('Percentage of Occupied Slots:')),
                        verbatimTextOutput("parking_value", placeholder = TRUE))),
                    fluidRow(box(title = 'Comparison with Historic Data',
                        status = 'success',
                        width = 12,
                        solidHeader = TRUE,
                        plotOutput("p_ts")))
    )
)))

                     


