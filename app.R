#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(tidyverse)
require(rpart.plot)
require(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sitio Shiny Ciencia de Datos\nArbol de Decision"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        NULL,

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
        set.seed(123)
        date_fruit <- read.csv("http://201.161.90.102:8080/citibike-tripdata.csv")
        
        # Dividir dataset en 75% entrenamiento y 25% prueba
        date_fruit_split <- sample(
          c(
            rep(0, (0.75 * nrow(date_fruit))), 
            rep(1, (0.25 * nrow(date_fruit)))
          )
        )
        
        date_fruit_train = date_fruit[date_fruit_split==0,]
        date_fruit_test = date_fruit[date_fruit_split==1,]
        
        date_fruit_tree <- rpart(start_lat ~ end_lat + end_lng, data = date_fruit_train)
        rpart.plot(date_fruit_tree)
        
      
       
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
