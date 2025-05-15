library(shiny)
library(tidyverse)
library(viridis)
faithful <- as_tibble(datasets::faithful[, 2])


# Define UI for application that draws a histogram

test <- read_csv("Unemployment_by_state.csv")
test <- test |>
  mutate(across(-1,as.numeric))
u3 <- test |>
  pivot_longer(cols = -1,
               names_to = "date",
               values_to = "unemployment")
u3$date <- str_replace(u3$date," "," 01 ")
u3$date <- mdy(u3$date)

ui <- fluidPage(
  
  # Application title
  titlePanel("Unemployment Rate"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "bins",
                  label = "Number of States:",
                  min = 1,
                  max = 11,
                  value = 5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    v_colors <- viridis(input$bins, 
                        alpha = 1, 
                        begin = 0, 
                        end = 1, 
                        direction = 1, option = "B")
    
    ggplot(u3, aes(x = year(date), y = after_stat(density))) +
      geom_histogram(bins = input$bins,
                     fill = v_colors) +
      labs(x = 'State',
           y = "Density",
           title = 'Histogram of Unemployment Density Per Year') +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5,
                                      size = rel(1.5),
                                      face = 'bold',
                                      margin = 
                                        margin(0,0,30,0)),
            axis.title.x = element_text(size = rel(1.5),
                                        margin = 
                                          margin(30,0,0,0)),
            axis.title.y = element_text(size = rel(1.5),
                                        margin = 
                                          margin(0,30,0,0)) ,
            axis.text.x = element_text(size = rel(1.5)),
            axis.text.y = element_text(size = rel(1.5)) )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)