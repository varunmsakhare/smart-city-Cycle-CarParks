
shinyServer(function(input, output, session){
  
  value <- reactiveVal(0)
  ###################################################################################################  
  
  #                                             Cycle Dataset
  
  ####################################################################################################  
  
  
  data_input <- reactive ({
    OID[OID$Date >= input$n_date[1] & OID$Date <= input$n_date[2],] 
  })
  
  ################################################## Cycle Map ##################################################  
  
  output$map <- renderLeaflet({
    leaflet(data = data_input()) %>% addTiles() %>% 
      addMarkers(lng = ~Longitude, lat = ~latitude, 
                 popup = paste("<b>Eco Counter Location:</b>",data_input()$Name , '<br>', "<b> Date :</b>",data_input()$Date,'<br>'," <b>Total Cycle Count:</b>",data_input()$Number,'<br>',"<b>Temperature:</b>",data_input()$Temperature),
                 clusterOptions = markerClusterOptions())
  })
  
  ################################################## Cycle Table ##################################################  
  
  output$summary_table <- renderDataTable({
    data_input()
  })
  
  ################################################## Scatter Plot Cycle ##################################################  
  
  output$plot <- renderPlot({
    ggplot(data_input(), aes( x= Number, y = Name))+geom_point(aes(col = as.character(Year)))+xlab("Number") + ylab ("Name")+ggtitle("Total Count of Cycles in Each Location")
 
  ################################################## Num Boxes ##################################################  
       
  })
  output$infoboxnum <- renderInfoBox({
    valueBox(sum(data_input()$Number),"Total Number of Bicycles for the selected Date Range:",icon = icon("bicycle"),color = "orange")
  })
  
  output$infoboxmin <- renderInfoBox({
    valueBox(min(data_input()$Number),"Minimum Number of Bicycles per Eco counter in a day:",icon = icon("bicycle"),color = "green")
  })
  
  output$infoboxmax <- renderInfoBox({
    valueBox(max(data_input()$Number),"Maximum Number of Bicycles per Eco Counter in a day:",icon = icon("bicycle"),color = "red")
  })
  
  ################################################## Cycle Ridge Plot ##################################################  
  
  output$ridge_plot <- renderPlot({
    
    ggplot(data_input(), aes(x = Number, y = Name, fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
      scale_fill_viridis_c(name = "Number", option = "C") 
  })
  

  
  
  ################################################## Effect of Weather on Cycle ##################################################  
  
  output$week_plot <- renderPlot({
    
    OIDWeek <- OID %>% filter(Week == input$CycleWeek)
    q <- ggplot(OIDWeek,aes(Month,Number))
    q <- q + geom_point() + scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1))
    
    q <- q + geom_point(position=position_jitter(w=1,h=0),aes(color = Temperature)) 
    q <- q + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
    q + theme_bw()
    q
  })
  
  ################################################## Box Plot cycle ################################################## 
  
  output$box_plot <- renderPlot({
    
    ggplot(OID,aes(Season,Number)) + geom_boxplot(aes(color = Season)) +facet_grid(~Year) + theme_bw()
  })
  
  output$first <- renderText({
    nrow(count(first$Name))
  })
  output$second <- renderText({
    nrow(count(second$Name))
  })
  output$third <- renderText({
    nrow(count(third$Name))
  })
  output$fourth <- renderText({
    nrow(count(fourth$Name))
  })
  output$fifth <- renderText({
    nrow(count(fifth$Name))
  })
  
  
  ################################################### Prediction model for Cycle  ################################################## 
  
  #React value when using the action button
  a <- reactiveValues(result = NULL)
  
  observeEvent(input$cal, {
    #Copy of the test data without the dependent variable
    test_pred <- test_set[-2]
    
    #Dataframe for the single prediction
    values = data.frame(Months = input$c_mnth,
                        Year = input$c_yr,
                        Name = input$c_loc,
                        Temperature = input$c_temp,
                        Humidity = as.integer(input$c_hum),
                        WindSpeed = input$c_wind,
                        WeekDay = input$c_weekd)
    
    #Include the values into the new data
    test_pred <- rbind(test_pred,values)
    
    #Single preiction using the model
    a$result <-  round(predict(model_rf, 
                               newdata = test_pred[nrow(test_pred),]), 
                       digits = 0)
  })
  
  output$value <- renderText({
    #Display the prediction value
    paste(a$result)
  })
  
  output$c_ts <- renderPlot({
    
    d <- OID %>% filter(Name == input$c_loc)  %>% 
      filter(Year == "2020") %>% filter(WeekDay == input$c_weekd)
    
    ggplot(d, aes(x= Months, y=Number))+xlab("Months") +
      ylab("Count of Bicycles") + geom_point(shape=16, size=3, color = "darkgreen") +
      geom_point(data = . %>% filter(Months == input$c_mnth),aes(y=a$result), colour = "red",shape = 8,size=8)
    
  })
  
  output$range_bike <- renderText({
    #Display the range of prediction value using the MAE value
    input$cal
    isolate(sprintf('%s - %s', 
                    round(a$result - mae_rf, digits = 0),
                    round(a$result + mae_rf, digits = 0)))
  })
  
  



  ####################################################################################################  
  
  #                                       PArking Dataset
  
  ###################################################################################################  
  
  parking_input <- reactive ({
    Parking[Parking$Date >= input$p_date[1]  & Parking$Date <= input$p_date[2],] 
  })
  
  ################################################## Parking Map ##################################################  
  
  output$pmap <- renderLeaflet({
    leaflet(data = parking_input()) %>% addTiles() %>% 
      addMarkers(lng = ~Longitude, lat = ~latitude
                 , popup = paste("<b>Location :</b>",parking_input()$Location,"<br>","<b> Hour:</b>",parking_input()$Hour,"<br>","<b> Temperature in C:</b>",parking_input()$Temperature, "<br>","<b> Available Slot :</b>",parking_input()$Available,"<br>", "<b> Occupied Slot:</b>",parking_input()$Occupied),
                 clusterOptions = markerClusterOptions())
  })
  
  ################################################## Ridge plot parking ##################################################  

  output$ perc_occupied <- renderPlot({
    
    ggplot(parking_input(), aes(x = Percentage_Occupied, y = Location, fill = stat(x))) +
      geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) + 
      scale_fill_viridis_c(name = "Percentage_Occupied", option = "C") 
  })
  
  ################################################## Table Parking ##################################################  
  
  output$summary_table_park <- renderDataTable({
    parking_input()
  })

  
  ################################################## Effect of Weather on Parking Count ##################################################  
  
  output$perc_hist <- renderPlot({
    
    z <- Parking %>% filter(Location == input$location) %>% filter(Week == input$Week)%>% filter (Date >= input$p_date[1]  & Date <= input$p_date[2],)
    
    g <- ggplot(z,aes(Hour,Percentage_Occupied))
    g <- g + geom_point() + scale_x_continuous(breaks = seq(from = 0, to = 23, by = 1))
    
    g <- g + geom_point(position=position_jitter(w=0,h=0),aes(color = Temperature)) 
    g <- g + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
    g + theme_bw()
    g
    
  })
  ################################################## GG MArginal for Parking Plot ##################################################  
  
  output$perc_dot <- renderPlot({
    
    x <- Parking %>% filter(Location == input$location) %>% filter(Week == input$Week) %>% filter (Date >= input$p_date[1]  & Date <= input$p_date[2],)
    
    p <- ggplot(x , aes( x= Percentage_Occupied, y = Temperature))+geom_point(aes(col = Hour))+xlab("Percentage of Occupied Slots") + ylab ("Temperature")
    ggMarginal(p, type="histogram")
  })
  

  ################################################## Prediction model for Parking ##################################################  
  
  
  #React value when using the action button
  b <- reactiveValues(result = NULL)
  
  observeEvent(input$parking_cal, {
    #Copy of the test data without the dependent variable
    parking_test_pred <- parking_test_set[-8]
    
    #Dataframe for the single prediction
    parking_values = data.frame(Months = (input$p_mnth),
                        Hour = input$p_hr,
                        Year = input$p_yr,
                        Location = input$p_loc,
                        Temperature = input$p_temp,
                        Humidity = as.integer(input$p_hum),
                        WindSpeed = input$p_wind,
                        Week = input$p_week)
    
    #Include the values into the new data
    parking_test_pred <- rbind(parking_test_pred,parking_values)
    
    #Single preiction using the model
    b$result <-  round(predict(model_parking_rf, 
                               newdata = parking_test_pred[nrow(parking_test_pred),]), 
                       digits = 0)
  })
  
  output$parking_value <- renderText({
    #Display the prediction value
    paste((b$result),"%")
  })
  
  
  output$p_ts <- renderPlot({
    
    
    c <- Parking %>% filter(Location == input$p_loc) %>% filter(Months == input$p_mnth) %>% 
      filter(Year == "2020") %>% filter(Week == input$p_week) %>% 
      filter(WeekDay %in% c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
    
    c %>% ggplot(aes(x=Hour, y = Percentage_Occupied, group = WeekDay , color = WeekDay)) +  
      geom_point(shape=16, size=3)+scale_x_continuous(breaks = seq(from = 0, to = 23, by = 1)) +
       geom_point(data = . %>% filter(Hour == input$p_hr),aes(y=b$result), colour = "red",shape = 8,size=8)
    
  })

  
})
