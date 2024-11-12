library(shiny)
library(ggplot2)
library("rjson")
library(dplyr)
library(rmarkdown)
library(lubridate)

#site4<- read.csv('Site_4.csv')
#saveRDS(site4, 'Site4.rds')
#site4 <- readRDS('Site4.rds')
sites <- read.csv('Sites.csv')

shinyServer(function(input, output) {
  data<- reactive({ 
    data <- c() #create empty vector
    #part 1: selecting the site id

    if ('Sumburgh'  %in% input$chosen_id){
      site4<- read.csv('Site_4.csv')
      data<- rbind(data, site4)
      
    }else{ data<- data}
    if ('Dyce'  %in% input$chosen_id){
      site161<- read.csv('Site_161.csv')
      data<- rbind(data, site161)
      
    }else {data<- data}
    if ('Tain Range'  %in% input$chosen_id){
      site79<- read.csv('Site_79.csv')
      data<- rbind(data, site79)
      
    }else {data<- data}
    if ('Leuchars'  %in% input$chosen_id){
      site235<- read.csv('Site_235.csv')
      data<- rbind(data, site235)
      
    }else {data<- data}
    if ('Boulmer'  %in% input$chosen_id){
      site315<- read.csv('Site_315.csv')
      data<- rbind(data, site315)
      
    }else {data<- data}
    if ('Waddington'  %in% input$chosen_id){
      site384<- read.csv('Site_384.csv')
      data<- rbind(data, site384)
      
    }else {data<- data}
    if ('Marham'  %in% input$chosen_id){
      site409<- read.csv('Site_409.csv')
      data<- rbind(data, site409)
      
    }else {data<- data}
    if ('Benson'  %in% input$chosen_id){
      site613<- read.csv('Site_613.csv')
      data<- rbind(data, site613)
      
    }else {data<- data}
    if ('Shawbury'  %in% input$chosen_id){
      site643<- read.csv('Site_643.csv')
      data<- rbind(data, site643)
      
    }else {data<- data}
    if ('Heathrow'  %in% input$chosen_id){
      site708<- read.csv('Site_708.csv')
      data<- rbind(data, site708)
      
    }else {data<- data}
    if ('Northolt'  %in% input$chosen_id){
      site709<- read.csv('Site_709.csv')
      data<- rbind(data, site709)
      
    }else {data<- data}
    if ('Hurn'  %in% input$chosen_id){
      site842<- read.csv('Site_842.csv')
      data<- rbind(data, site842)
      
    }else {data<- data}
    if ('Machrihanish'  %in% input$chosen_id){
      site908<- read.csv('Site_908.csv')
      data<- rbind(data, site908)
      
    }else {data<- data}
    if ('Abbotsinch'  %in% input$chosen_id){
      site971<- read.csv('Site_971.csv')
      data<- rbind(data, site971)
      
    }else {data<- data}
    if ('Blackpool'  %in% input$chosen_id){
      site1090<- read.csv('Site_1090.csv')
      data<- rbind(data, site1090)
      
    }else {data<- data}
    if ('Ringway'  %in% input$chosen_id){
      site1135<- read.csv('Site_1135.csv')
      data<- rbind(data, site1135)
      
    }else {data<- data}
    if ('Hawarden Airport'  %in% input$chosen_id){
      site1144<- read.csv('Site_1144.csv')
      data<- rbind(data, site1144)
      
    }else {data<- data}
    if ('Pembrey Sands'  %in% input$chosen_id){
      site1226<- read.csv('Site_1226.csv')
      data<- rbind(data, site1226)
      
    }else {data<- data}
    if ('Yeovilton'  %in% input$chosen_id){
      site1302<- read.csv('Site_1302.csv')
      data<- rbind(data, site1302)
      
    }else {data<- data}
    if ('Aldergrove'  %in% input$chosen_id){
      site1450<- read.csv('Site_1450.csv')
      data<- rbind(data, site1450)
      
    }else {data<- data}
    data<- merge(data, sites, by.x = "Site", by.y="Site_ID")
    data<- data%>% mutate(week=wday(data$ob_time, label=FALSE))
    data<- data%>%mutate(hour_in_week=((week-1)*24+hour))
    
    
  }) 

    #part 2: select the aggregation
  plotdata<- reactive({
    if (input$aggregation=='rawdata'){
      plotdata<- data()
    }
    else if (input$aggregation=='daily'){
      plotdata<- data() %>% group_by(day,month, Site_Name) %>% 
        summarise_at(vars(c(wind_speed, air_temperature, rltv_hum, visibility, week)),
                     mean, na.rm=TRUE)
    }
    else if (input$aggregation=='monthly'){
      plotdata<- data() %>% group_by(month, Site_Name) %>% 
        summarise_at(vars(c(wind_speed, air_temperature, rltv_hum, visibility)),
                     mean, na.rm=TRUE)
    }
    else if (input$aggregation=='dailymax'){
      plotdata<- data() %>% group_by(day,month, Site_Name) %>% 
        summarise_at(vars(c(wind_speed, air_temperature, rltv_hum, visibility, week)),
                     max, na.rm=TRUE)
    }    
    else if (input$aggregation=='dailymin'){
      plotdata<- data() %>% group_by(day,month, Site_Name) %>% 
        summarise_at(vars(c(wind_speed, air_temperature, rltv_hum, visibility, week)),
                     min, na.rm=TRUE)
    }

  })
  
  tablesummaries<- reactive({
    data() %>% group_by(month, day, Site_Name)%>%
      summarise_at(vars(c(wind_speed, air_temperature, rltv_hum, visibility)),
                   mean, na.rm=TRUE) %>% filter(month==11 & day>23)
  })
  
  #part 3: select the y  axis
  y <- reactive({
    input$weathervariable
  })
  #y axis labels
  y_label <- reactive({
    req(input$weathervariable)
    if(input$weathervariable == "wind_speed"){
      y_label <- "Wind Speed"
    } else if(input$weathervariable == "air_temperature"){
      y_label <- "Air Temperature"
    } else if(input$weathervariable == "visibility"){
      y_label <- "Visibility"
    }else if(input$weathervariable == "rltv_hum"){
      y_label <- "Relative Humidity"
    }
  })
  
  #part 4: select the x axis 
  x<- reactive({
    if (input$aggregation=='rawdata'){
      if (input$xaxis=='calendartime')
        x<- 'ob_time'
      else if (input$xaxis=='dayofweek')
        x<- 'hour_in_week'
      else if (input$xaxis=='hourinday')
        x<- 'hour'
    }
    else if (input$aggregation=='daily'){
      x<- 'week'
    }
    else if (input$aggregation=='monthly'){
      x<- 'month'
    }
    else if (input$aggregation=='dailymax'){
      x<- 'week'
    }
    else if (input$aggregation=='dailymin'){
      x<- 'week'
    }
  })
  
  #x axis label
  x_label <- reactive({
    req(input$aggregation)
    if (input$aggregation=='rawdata'){
      if (input$xaxis=='calendartime')
        x_label<- 'date'
      else if (input$xaxis=='dayofweek')
        x_label<- 'hour of week(1-168)'
      else if (input$xaxis=='hourinday')
        x_label<- 'hour in day(1-24)'
    }else if(input$aggregation == "daily" ){
      x_label <- "Day of the week(1-7)"
    } else if(input$aggregation == "monthly"){
      x_label <- "Month(1-12)"
    }else if(input$aggregation == "dailymax"){
      x_label <- "Day of the week(1-7)"
    }else if(input$aggregation == "dailymin"){
      x_label <- "Day of the week(1-7)"
    }
  })
  
  #hutton criteria: 
  final_data <- reactive({
    
    #hutton criteria : calc if temp is over 10 over two days
    practice_hutton<- data()
    practice_hutton <- na.omit(practice_hutton)
    practice_hutton<-practice_hutton %>% group_by( Site_Name, month, day) %>% 
      summarise_at(vars(c(wind_speed, air_temperature, rltv_hum, visibility)),
                   min, na.rm=TRUE)
    value <- c()
    
    for (row in 1:nrow(practice_hutton)){
      air_temperature <- practice_hutton[row, "air_temperature"]
      if (air_temperature>=10)
        value <- rbind(value, 'Danger')
      else { value <- rbind(value, 'OK')}
    }
    value
    no_data<- rbind(c('No Data'))
    
    #remove last element in vector v: 
    value_1<- value[-length(value)]

    #add a plain vector hence shifting the data down once
    Temp_day_before<-append(no_data, value_1)
    #two days before:
    value_2<- Temp_day_before[-length(Temp_day_before)]
    
    Temp_twodays_before<-append(no_data, value_2)
    #add to dataframe:
    dataframe<- cbind(practice_hutton, Temp_day_before, Temp_twodays_before)
    
    #test if temp is greater than 10 on both days:
    dataframe$is_temp_over_ten<- ifelse(Temp_day_before == 'Danger' & Temp_twodays_before == 'Danger', 'Danger', 'OK')
    
    #CALCULATE IF HUMIDITY IS OVER 90%
    practice_hutton2<- data()
    practice_hutton2 <- na.omit(practice_hutton2)
    humid_value <- c()
    
    for (row in 1:nrow(practice_hutton2)){
      rltv_hum <- practice_hutton2[row, "rltv_hum"]
      if (rltv_hum>=90)
        humid_value <- rbind(humid_value, 'Danger')
      else { humid_value <- rbind(humid_value, 'OK')}
    }
    humid_value
    practice_hutton2<- cbind(practice_hutton2, humid_value)
    #is there at least 6 hours of this humidity in each day
    practice_hutton2_count<- practice_hutton2 %>% group_by(Site_Name, month, day)%>% summarise(count=sum(humid_value=='Danger'))
    humid_value<- practice_hutton2_count$count
    
    no_data<- rbind(c('No Data'))
    #remove last element in vector v: 
    humid_value1<- humid_value[-length(humid_value)]
    humid_value1<-strtoi(humid_value1)
    #add a plain vector hence shifting the data down once
    humid_day_before<-append(no_data, humid_value1)
    #two days before:
    humid_value2<- humid_day_before[-length(humid_day_before)]
    humid_value2<- strtoi(humid_value2)
    
    humid_twodays_before<-append(no_data, humid_value2)
    humid_day_before<- strtoi(humid_day_before)
    humid_twodays_before<- strtoi(humid_twodays_before)
    #add to dataframe:
    dataframe2<- cbind(practice_hutton2_count, humid_day_before, humid_twodays_before)
    colnames(dataframe2) 
    #test if humid is greater than 90 on both days:
    
    dataframe2$is_humid_over_ninety<- ifelse(humid_day_before >= 6 & humid_twodays_before >= 6, 'Danger', 'OK')
    
    #combine both dataframes together
    final_data<- merge(dataframe2, dataframe)
    final_data$Potato_Blight<- ifelse(final_data$is_humid_over_ninety =='Danger' & final_data$is_temp_over_ten =='Danger', 'Warning', 'Safe')
    final_data<- select(final_data, day, month, Site_Name, wind_speed, air_temperature, rltv_hum, visibility, Potato_Blight)

    if (input$huttonmonth == 'jan')
      final_data<- final_data%>% filter(month==1)
    else if (input$huttonmonth == 'feb')
      final_data<- final_data%>% filter(month==2)
    else if (input$huttonmonth == 'march')
      final_data<- final_data%>% filter(month==3)
    else if (input$huttonmonth == 'apr')
      final_data<- final_data%>% filter(month==4)
    else if (input$huttonmonth == 'may')
      final_data<- final_data%>% filter(month==5)
    else if (input$huttonmonth == 'jun')
      final_data<- final_data%>% filter(month==6)
    else if (input$huttonmonth == 'jul')
      final_data<- final_data%>% filter(month==7)
    else if (input$huttonmonth == 'aug')
      final_data<- final_data%>% filter(month==8)
    else if (input$huttonmonth == 'sep')
      final_data<- final_data%>% filter(month==9)
    else if (input$huttonmonth == 'oct')
      final_data<- final_data%>% filter(month==10)
    else if (input$huttonmonth == 'nov')
      final_data<- final_data%>% filter(month==11)
    
  }) 

  
 
   #create dataframe for map
  sites <- read.csv('Sites.csv')
  site_selection<- reactive({
  map <- data() %>% group_by(Site)%>% 
    summarise_at(vars(c(wind_speed, air_temperature, rltv_hum, visibility)),
                 mean, na.rm=TRUE)
  site_selection<- sites%>% filter(Site_ID %in% map$Site)
})
  
  
  #plots 
  lineplot<- reactive({
    if (input$aggregation=='rawdata'){
      if(input$xaxis=='calendartime'){
        lineplot<- ggplot(plotdata())+
          geom_line(aes_string(x(), y(), colour='Site_Name', group=1))+
          ylab(y_label())+
          xlab(x_label())+
          labs(color = "Site Name")}
    }
    lineplot
  })
  
  output$plotline <- renderPlot({
    lineplot()
  })
  
  scatterplot<- reactive({
    if (input$aggregation=='rawdata'){
      if(input$xaxis=='dayofweek')
        scatterplot<- ggplot(plotdata())+
          geom_point(aes_string(x(), y(), colour='Site_Name' ))+
          ylab(y_label())+
          xlab(x_label())+
          labs(color = "Site Name")
      if(input$xaxis=='hourinday')
        scatterplot<- ggplot(plotdata())+
          geom_point(aes_string(x(), y(), colour='Site_Name' ))+
          ylab(y_label())+
          xlab(x_label())+
          labs(color = "Site Name")
    }
    else{
      scatterplot<- ggplot(plotdata())+
        geom_point(aes_string(x(), y(), colour='Site_Name' ))+
        ylab(y_label())+
        xlab(x_label())+
        labs(color = "Site Name")
      }
    scatterplot
  })
  output$plotscatter <- renderPlot({
    scatterplot()
  })
  
  #table with summaries output
  output$meantables <- renderTable({
    tablesummaries() 
    
  })

  #plot the map
  output$mapplot<- renderPlot({
  maps::map("world","UK")
  points(site_selection()$Longitude, site_selection()$Latitude, pch=16, col="red")
  
  })
  library(tinytex)
  #download report
  output$report <- downloadHandler(
    filename = "report.docx",
    content = function(file) {
      render("report.Rmd", output_format="word_document",
             output_file=file, params=list(table= tablesummaries(), 
                                           line= lineplot(), 
                                           scatter = scatterplot() ))
    })
  #download as csv
  output$download <- downloadHandler(
    filename = function(){"table.csv"}, 
    content = function(fname){
      write.csv(tablesummaries(), fname)
    })
  #huttontable
  output$huttontable <- renderDataTable({
    final_data() 
    
  })
  
}) 

