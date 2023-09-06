library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(periscope)
library(tidyverse)
library(leaflet)
library(htmltools)

df <- read_rds("month_weather_station.RDS") %>%
  mutate(dayte=make_date(year,month)) %>%
  mutate(month2=as.character(month(dayte, label = TRUE, abbr = FALSE)))
  
df_station <- df %>%
  group_by(name) %>%
  slice_head() %>%
  select(name:latitude) %>%
  mutate(mcolor=ifelse(name=="Aberporth", "red", "blue"))


buttonoptions <- c("Max temperature (C)"="tmaxdegc",
                   "Min temperature (C)"="tmindegc",
                   "Average no. frost days"="afrostdays",
                   "Rain mm"="rainmm",
                   "Hours of Sun"="sunhours")




# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "UK Met Office historic weather station", titleWidth = 400),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
  fluidRow(column(8,
    plotOutput(outputId = "station_plot")),
    column(4, leafletOutput("stationmap"))),

   fluidRow(
        column(8,
               wellPanel(sliderInput("daterange", "Date range", min = min(df$year), 
                           max = max(df$year), value =  c(min(df$year), max(df$year))
                          , width="100%", sep=""))), 
        (column(4, wellPanel(selectInput("station", "Weather station",
                                             choices=df_station$name))))
        ),
                 
                 
  fluidRow(
    column(8, wellPanel(sliderTextInput("datatype", "Month",
                                        choices= month.name, grid=TRUE))),
    column(3, wellPanel(radioButtons("weathvar", "Weather variable",
                                              choices= buttonoptions))),
    column(1, downloadButton("d1", "Plot data"), downloadButton("d2", "All data"),
           downloadButton("d3", "Plot"))
   
))

)
                
    
# Define server logic required to draw a histogram
server <- function(input, output) {

micon <- awesomeIcons(markerColor = df_station$mcolor)
      
output$stationmap <- renderLeaflet(leaflet(df_station) %>%
                                     addTiles() %>%
                                     setView(lng = -4, lat = 55.5, zoom = 5) %>%
                                     addAwesomeMarkers(layerId=~name, label=~htmlEscape(name), icon=micon))

observeEvent(input$stationmap_marker_click, {
  click <- input$stationmap_marker_click
  updateSelectInput(inputId="station", 
                    selected =  click$id)
})

    
observeEvent(list(input$station, input$weathvar, input$datatype), {
    df1 <- df %>%
      select(name, year, xs=input$weathvar, month2) %>%
      filter(name==input$station & !is.na(xs) & month2==input$datatype & between(year, min(year), input$daterange[2]))
    updateSliderInput(inputId = "daterange", min = min(df1$year), 
                      max = max(df1$year), value =  c(min(df1$year), max(df1$year)))
  })  

observeEvent(input$station, {
  df_station2 <- df_station %>%
    mutate(mcolor=ifelse(name==input$station, "red", "blue"))
  micon2 <- awesomeIcons(markerColor = df_station2$mcolor)         
  leafletProxy("stationmap") %>%
    clearMarkers() %>%
    addAwesomeMarkers(layerId=~name, label=~htmlEscape(name), icon=micon2, data=df_station2)})

df2 <-  reactive({df %>%
  select(name, year, xs=input$weathvar, month2) %>%
  filter(name==input$station & !is.na(xs)  & month2==input$datatype & between(year, input$daterange[1], input$daterange[2]))})

 plotInput <- function () { 
   ggplot(df2(), aes(x=factor(year), y=xs)) +
     geom_point() +
     geom_path(group=1, colour="darkgreen") +
     labs(x=input$datatype, y=names((buttonoptions[which(buttonoptions == input$weathvar)]))) +
     scale_x_discrete(expand = expansion(mult = .01)) +
     theme(axis.text.x=element_text(angle=60, hjust=1))}

 output$station_plot <- renderPlot({
   plotInput()
 })
 
 
 
 output$d1 <- downloadHandler(
   filename = function() {
     paste0(input$station, input$weathvar, input$datatype, ".tsv")
   },
   content = function(file) {
     vroom::vroom_write(df2(), file)
   }
 )
 
 output$d2 <- downloadHandler(
   filename = function() {
     paste0("all_stations_variables_months", ".tsv")
   },
   content = function(file) {
     vroom::vroom_write(df, file)
   }
 )
 
 output$d3 <- downloadHandler(
   filename = function() {paste0(input$station, input$weathvar, input$datatype, '.png')},
   content = function(file) {
     ggsave(file, plot = plotInput(), device = "png")
   }
 )
 
}


# Run the application 
shinyApp(ui = ui, server = server)
