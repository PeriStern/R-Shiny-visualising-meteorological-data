sites <- read.csv('Sites.csv')


library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Meteorological Data accross the UK"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          #select weather station(up to 5 choices)
          selectizeInput("chosen_id","Site ID",
                      sites[ ,'Site_Name'], multiple = TRUE, options = list(maxItems = 5)),
          #select weather variable - one at a time
            selectInput('weathervariable', 'Weather Variable', 
                        c('Wind Speed' = 'wind_speed', 
                          'Air Temperature' = 'air_temperature',
                          'Relative Humidity' = 'rltv_hum',
                          'Visibility' = 'visibility')),
          radioButtons('aggregation', 'Choose the aggregation',
                       c('Raw hourly data (no aggregation)' = 'rawdata',
                         'Daily Averages' = 'daily',
                         'Monthly Averages' = 'monthly',
                         'Daily Maxima' = 'dailymax',
                         'Daily Minima' = 'dailymin')),
          conditionalPanel( "condition = input.aggregation=='rawdata'",
          radioButtons('xaxis', 'Choose the measure of time',
                       c('Calendar time' = 'calendartime',
                         'Hour within the week ' = 'dayofweek',
                         'Hour in the day' = 'hourinday'))),
          downloadButton("report", "Generate report"),
          downloadButton('download',"Download table as CSV"),
          radioButtons('huttonmonth', 'Pick a month to test for a potato blight',
                       c('Janurary' = 'jan',
                         'Feburary ' = 'feb',
                         'March' = 'march',
                         'April' = 'apr',
                         'May ' = 'may',
                         'June' = 'jun',
                         'July ' = 'jul',
                         'August' = 'aug',
                         'September' = 'sep',
                         'October ' = 'oct',
                         'November' = 'nov'))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plotscatter"),
            plotOutput("plotline"), 
            tableOutput('meantables'),
            plotOutput("mapplot"),
            dataTableOutput('huttontable')
        )
    )
))
