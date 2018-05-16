#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages('future')
install.packages('leaflet')
install.packages('tidyverse')
install.packages('ggplot2')
system('sudo apt-get update && apt-get install -y libudunits2-dev libgdal-dev libproj-dev')
install.packages('tigris') 

check.vars<-c("avg_coalesced_commercial_age"
              ,"avg_age_pr_18_34_commercial"
              ,"avg_race_white_commercial"
              ,"avg_ts_married"
              ,"avg_children_in_hh"
              ,"avg_education_collegegrad"
              ,"avg_head_hh_salary_amt"
              ,"avg_travel_domestic_foreign"
              ,"avg_hh_has_credit_card"
              ,"avg_online_is_online"
              ,"avg_foodatlas12_fast_food_restaurants"
              ,"avg_foodatlas12_full_service_restaurants"
              ,"avg_last_6_months_dunkin_donuts"
              ,"avg_last_6_months_mcdonalds"
              ,"avg_last_6_months_starbucks"
              ,"total_count"
              ,"avg_coalesced_commercial_age_index"
              ,"avg_age_pr_18_34_commercial_index"
              ,"avg_race_white_commercial_index"
              ,"avg_ts_married_index"
              ,"avg_children_in_hh_index"
              ,"avg_education_collegegrad_index"
              ,"avg_head_hh_salary_amt_index"
              ,"avg_travel_domestic_foreign_index"
              ,"avg_hh_has_credit_card_index"
              ,"avg_online_is_online_index"
              ,"avg_foodatlas12_fast_food_restaurants_index"
              ,"avg_foodatlas12_full_service_restaurants_index"
              ,"avg_last_6_months_dunkin_donuts_index"
              ,"avg_last_6_months_mcdonalds_index"
              ,"avg_last_6_months_starbucks_index"
              ,"total_count_index")
              
library(shiny)
#library(future)
library(civis)
library(leaflet)
library(tigris)
library(tidyverse)
library(ggplot2)

#plan(multisession)

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
index.choices <- check.vars


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

# List of choices for selectInput and checkboxGroupInput
cluster.list <- as.vector(cluster.choices$var)
index.list <- as.vector(index.choices)
# Name it
names(cluster.list) <- cluster.choices$var

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Dunkin Retail Map"),
   
   fluidRow(
   # Left sidebars 
   column(2,
      selectInput("cluster",
                     label = h3("Store Cluster:"),
                     choices = c("All", cluster.list),
                     #selected = NULL,
                     multiple = FALSE)
      ,
      sliderInput("mcdonalds_in_one_mile", "Mcdonald's Stores Within One Mile:",
                  min = 0, max = max(data$mcdonalds_in_one_mile),
                  value = c(0,max(data$mcdonalds_in_one_mile)))
      ,
      sliderInput("mcdonalds_in_five_miles", "McDonald's Stores Within Five Miles:",
                  min = 0, max = max(data$mcdonalds_in_five_miles),
                  value = c(0,max(data$mcdonalds_in_five_miles)))
      ,
      sliderInput("starbucks_in_one_mile", "Starbucks Stores Within One Mile:",
                  min = 0, max = max(data$starbucks_in_one_mile),
                  value = c(0,max(data$starbucks_in_one_mile)))
      ,
      sliderInput("starbucks_in_five_miles", "Starbucks Stores Within Five Miles:",
                  min = 0, max = max(data$starbucks_in_five_miles),
                  value = c(0,max(data$starbucks_in_five_miles)))
      ,
      style="height: 1000px; overflow-x: scroll; overflow-y: scroll")
   ,
      # Show a plot of the generated distribution
   column(8,
        leafletOutput("map", height="600px"),
         plotOutput("dunkinPlot", height = "400px")
      ),
   column(2,
          checkboxGroupInput("variables",
                      label = h3("Variables to Display:"),
                      choices = c(index.list))
          ,
          style="height: 1000px; overflow-x: scroll; overflow-y: scroll"
   )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
   output$map <- renderLeaflet({
      # generate bins based on input$bins from ui.R
      #x    <- data[data$clustername == input$cluster, ] 
  
    
        
     us.leaflet<-leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(-98.35, 39.7,zoom = 4) %>%
                addMarkers(lng=starbucks.stores$civis_longitude, lat = starbucks.stores$civis_latitude, icon = starbucksIcon)  %>%
                addMarkers(lng=mcdonalds.stores$civis_longitude, lat = mcdonalds.stores$civis_latitude, icon = mcdonaldsIcon) 
     print(us.leaflet)
   })
   
  session$allowReconnect("force")
}

# Run the application 
shinyApp(ui = ui, server = server)
