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

data<-cluster.data
choices <- data.frame(
  var = levels(data$clustername),
  num = 1:length(levels(data$clustername))
)
# List of choices for selectInput
cluster.list <- as.vector(choices$var)
# Name it
names(cluster.list) <- choices$var

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
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         textOutput("text")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      #x    <- data[data$clustername == input$cluster, ] 
     colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
     
      if (input$cluster == "All"){
        x <- data
        colour <- factor(x$clustername)
      } else{
        #hold <- reactive(data[which(data$cluster %in% input$cluster),])
        #x <- hold()
        x<-data[which(data$clustername %in% input$cluster),]
        index<- match(input$cluster, cluster.list)
        colors<- colors[index]
      }
        
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      x.min <- 0
      x.max <- max(data$starbucks_in_five_miles)
      y.min <- 0
      y.max <- max(data$mcdonalds_in_five_miles)
      
      # draw the histogram with the specified number of bins
      cluster_graph <- ggplot(x, aes(x = starbucks_in_five_miles, y = mcdonalds_in_five_miles)) + xlim(x.min, x.max) + ylim(y.min, y.max)
      cluster_graph <- cluster_graph + geom_point(aes(colour = factor(x$clustername)))
      
      cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
      cluster_graph <- cluster_graph + xlab("Starbucks in Five Miles")
      cluster_graph <- cluster_graph + ylab("McDonald's in Five Miles")
      title <- paste("k-means Solution with", "7", sep=" ")
      title <- paste(title, "Clusters", sep=" ")
      cluster_graph <- cluster_graph + ggtitle(title)
      print(cluster_graph)
   })
   
   output$text<- renderText({
     paste("You selected:", input$cluster)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

