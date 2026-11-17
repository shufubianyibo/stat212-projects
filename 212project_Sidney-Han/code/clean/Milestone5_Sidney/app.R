#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
bestseller <- read_csv("../../../data/clean/full_bestseller.csv")
clean <- bestseller %>% 
  group_by(title) %>%
  summarize(
    best_ranking = min(rank),
    max_weeks_on_list = max(weeks_on_list),
    publisher = first(publisher),
    author = first(author),
    .groups = "drop"
  )


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Top Bestseller Runs"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("bins",              #this stopped working midway through and I have not been able to fix it so I removed it for now.
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30),
          
            pickerInput(
              "pub",
              "Select options below:",
              bestseller$publisher,
              options = list(`actions-box` = TRUE),
              multiple = TRUE
            ),
  
            
          pickerInput(
            "auth",
            "Select options below:",
            bestseller$author,
            options = list(`actions-box` = TRUE),
            multiple = TRUE
          ),
          
          sliderInput(
            "rank",
            "Select top rank",
            min = 1,
            max = 15,
            value = 1
          )
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
      x    <- faithful[, 2] 
      bin <- seq(min(x), max(x), length.out = input$bins + 1)

        #create histogram with filtering in regards to dropdowns/sliders
        clean %>% 
          filter(max_weeks_on_list > 1) %>% 
          filter(publisher %in% input$pub) %>% 
          filter(author %in% input$auth) %>% 
          filter(best_ranking == input$rank) %>% 
          ggplot(aes(x = max_weeks_on_list)) +
          geom_histogram(binwidth = 10, fill = "darkred") +
          labs(x = "Maximum # of Weeks on List", y = "# of Books")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
