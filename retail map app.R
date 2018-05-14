#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(civis)
library(leaflet)
library(tigris)
library(tidyverse)
library(ggplot2)

# Pull in Dunkin Data
cluster.data<-read_civis("dunkin.dunkin_store_clusters")
index.data<-read_civis("dunkin.dunkin_stores_indexed")
starbucks.stores<-read_civis("dunkin.starbucks_stores_with_geo", stringsAsFactors = FALSE)
mcdonalds.stores<-read_civis("dunkin.mcdonalds_stores_with_geo", stringsAsFactors = FALSE)

combined.data<-merge(x = cluster.data, y = index.data, by = "address", all.y = FALSE, all.x = FALSE)

data<-combined.data
cluster.choices <- data.frame(
  var = levels(data$clustername),
  num = 1:length(levels(data$clustername))
)
index.choices <- data.frame(
  var = names(data),
  num = 1:length(names(data))
)

dunkinIcon <- makeIcon(
  iconUrl = "https://dunkinanytime.coca-cola.com/content/dam/dunkin/logo-favicon/dunkin-donuts-logo.png",
  iconWidth = 49, iconHeight = 65,
  iconAnchorX = 0, iconAnchorY = 0
)
starbucksIcon <- makeIcon(
  iconUrl = "https://waterfordlakestowncenter.com/images/default-source/store-logos/store-logos/starbucks.tmb-t-400x400.png?Status=Master&sfvrsn=3322078_7",
  iconWidth = 38, iconHeight = 38,
  iconAnchorX = 0, iconAnchorY = 0
)
mcdonaldsIcon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/3/36/McDonald%27s_Golden_Arches.svg/1200px-McDonald%27s_Golden_Arches.svg.png",
  iconWidth = 38, iconHeight = 38,
  iconAnchorX = 0, iconAnchorY = 0
)

# List of choices for selectInput
cluster.list <- as.vector(cluster.choices$var)
index.list <- as.vector(index.choices$var)
# Name it
names(cluster.list) <- cluster.choices$var

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Dunkin Retail Map"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("cluster",
                     label = h3("Store Cluster:"),
                     choices = c("All", cluster.list),
                     #selected = NULL,
                     multiple = FALSE)
      ,
      selectInput("Variable1",
                  label = h3("First Variable to Display:"),
                  choices = c(index.list),
                  #selected = NULL,
                  multiple = FALSE)
      ,
      selectInput("Variable2",
                  label = h3("Second Variable to Display:"),
                  choices = c(index.list),
                  #selected = NULL,
                  multiple = FALSE)
      ,
      selectInput("Variable3",
                  label = h3("Third Variable to Display:"),
                  choices = c(index.list),
                  #selected = NULL,
                  multiple = FALSE)      
      ),
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("map", height="600px"),
         textOutput("text")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$map <- renderLeaflet({
      # generate bins based on input$bins from ui.R
      #x    <- data[data$clustername == input$cluster, ] 
  
    
        
     us.leaflet<-leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(-98.35, 39.7,zoom = 4)
   })
   
   observeEvent(input$cluster,{
     if (input$cluster == "All"){
       x <- data
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[which(data$clustername %in% input$cluster),]
     }
     
     us.leaflet<-leafletProxy("map") %>% addProviderTiles("CartoDB.Positron")  %>% clearMarkers() %>%
       addMarkers(lng=x$civis_longitude, lat = x$civis_latitude, icon = dunkinIcon,
                  popup = paste(sep = "<br/>",
                                paste0("<b>Address: ",x$address,"</b>"),
                                paste0(input$Variable1, ": ",round(x[ , names(x) %in% input$Variable1]*100)),
                                paste0(input$Variable2, ": ",round(x[ , names(x) %in% input$Variable2]*100)),
                                paste0(input$Variable3, ": ",round(x[ , names(x) %in% input$Variable3]*100)))) %>%
       addMarkers(lng=starbucks.stores$civis_longitude, lat = starbucks.stores$civis_latitude, icon = starbucksIcon)  %>%
       addMarkers(lng=mcdonalds.stores$civis_longitude, lat = mcdonalds.stores$civis_latitude, icon = mcdonaldsIcon)
     print(us.leaflet)
     
   })

   observeEvent(input$Variable1,{
     if (input$cluster == "All"){
       x <- data
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[which(data$clustername %in% input$cluster),]
     }
     us.leaflet<-leafletProxy("map") %>% addProviderTiles("CartoDB.Positron")  %>% clearMarkers() %>%
       addMarkers(lng=x$civis_longitude, lat = x$civis_latitude, icon = dunkinIcon,
                  popup = paste(sep = "<br/>",
                                paste0("<b>Address: ",x$address,"</b>"),
                                paste0(input$Variable1, ": ",round(x[ , names(x) %in% input$Variable1]*100)),
                                paste0(input$Variable2, ": ",round(x[ , names(x) %in% input$Variable2]*100)),
                                paste0(input$Variable3, ": ",round(x[ , names(x) %in% input$Variable3]*100)))) %>%
       addMarkers(lng=starbucks.stores$civis_longitude, lat = starbucks.stores$civis_latitude, icon = starbucksIcon)  %>%
       addMarkers(lng=mcdonalds.stores$civis_longitude, lat = mcdonalds.stores$civis_latitude, icon = mcdonaldsIcon)
     print(us.leaflet)
     
   })
   
   observeEvent(input$Variable2,{
     if (input$cluster == "All"){
       x <- data
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[which(data$clustername %in% input$cluster),]
     }
     us.leaflet<-leafletProxy("map") %>% addProviderTiles("CartoDB.Positron")  %>% clearMarkers() %>%
       addMarkers(lng=x$civis_longitude, lat = x$civis_latitude, icon = dunkinIcon,
                  popup = paste(sep = "<br/>",
                                paste0("<b>Address: ",x$address,"</b>"),
                                paste0(input$Variable1, ": ",round(x[ , names(x) %in% input$Variable1]*100)),
                                paste0(input$Variable2, ": ",round(x[ , names(x) %in% input$Variable2]*100)),
                                paste0(input$Variable3, ": ",round(x[ , names(x) %in% input$Variable3]*100)))) %>%
       addMarkers(lng=starbucks.stores$civis_longitude, lat = starbucks.stores$civis_latitude, icon = starbucksIcon)  %>%
       addMarkers(lng=mcdonalds.stores$civis_longitude, lat = mcdonalds.stores$civis_latitude, icon = mcdonaldsIcon)
     print(us.leaflet)
     
   })
   
   observeEvent(input$Variable3,{
     if (input$cluster == "All"){
       x <- data
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[which(data$clustername %in% input$cluster),]
     }
     us.leaflet<-leafletProxy("map") %>% addProviderTiles("CartoDB.Positron")  %>% clearMarkers() %>%
       addMarkers(lng=x$civis_longitude, lat = x$civis_latitude, icon = dunkinIcon,
                  popup = paste(sep = "<br/>",
                                paste0("<b>Address: ",x$address,"</b>"),
                                paste0(input$Variable1, ": ",round(x[ , names(x) %in% input$Variable1]*100)),
                                paste0(input$Variable2, ": ",round(x[ , names(x) %in% input$Variable2]*100)),
                                paste0(input$Variable3, ": ",round(x[ , names(x) %in% input$Variable3]*100)))) %>%
       addMarkers(lng=starbucks.stores$civis_longitude, lat = starbucks.stores$civis_latitude, icon = starbucksIcon)  %>%
       addMarkers(lng=mcdonalds.stores$civis_longitude, lat = mcdonalds.stores$civis_latitude, icon = mcdonaldsIcon)
     print(us.leaflet)
     
   })
   
   output$text<- renderText({
     paste("You selected:", input$cluster)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

