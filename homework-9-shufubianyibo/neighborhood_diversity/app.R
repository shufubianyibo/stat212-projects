#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(sf)
library(tidyverse)

data_by_dist <- read_rds("data/diverse_data_by_dist.rds")
data_by_year <- read_csv("data/diverse_data_by_year.csv")
metro_names <- data_by_dist %>% pull(metro_name) %>% unique()
race_lookup <- c(
  aian = "American Indian",
  asian = "Asian",
  black = "Black",
  hispanic = "Hispanic",
  two_or_more = "Multiracial",
  white = "White")

# Define UI for application that draws a histogram
ui <- navbarPage(

    # Application title
    titlePanel("Diverse Data"),
    
    tabPanel("Explore metros",
             sidebarLayout(
               sidebarPanel(
                 width = 3, 
                 selectInput('city', 'Choose a metropolitan area', choices = metro_names, selected = metro_names[1]), # Default to the first city
                 sliderInput("span",
                             "Span parameter:",
                             min = 0.1, 
                             max = 0.9, 
                             value = 0.3, 
                             step = 0.05), 
                 hr(), 
                 p("Diversity is measured using the entropy index based on race/ethnicity categories from the 2020 US Census.")
               ),
               mainPanel(
                 width = 9, 
                 fluidRow( 
                   column(12,
                          h4("Diversity Score vs. Distance from City Center"),
                          plotlyOutput("scatterPlot", height = "400px") 
                   )
                 ),
                 hr(), 
                 fluidRow( 
                   column(6, 
                          h4("Map of diversity scores (2020 US Census)"),
                          plotOutput("mapPlot", height = "450px")
                   ),
                   column(6, 
                          h4("Overall Race Distribution"),
                          plotOutput("barPlot", height = "450px")
                   )
                 )
               )
             )
    ),
    
    tabPanel("Compare over time",
             h3("Comparison Over Time (Content to be added)"),
             p("This section will eventually allow comparisons across different census years.")
    ),
    
    tabPanel("About",
             h3("About This Application")
    )
)
    

    sidebarLayout(
        sidebarPanel(
            selectInput('city', 'Choose City', metro_names),
            sliderInput("span",
                        "Span:",
                        min = 0,
                        max = 1,
                        value = 0.7)
        ),

        mainPanel(
           plotlyOutput("scatterPlot"),
           plotOutput("mapPlot"),
           plotOutput("barPlot")
        )
    )



server <- function(input, output, session) {

    output$scatterPlot <- renderPlotly({
     # browser()
      p <- data_by_dist%>%
          filter(metro_name == input$city) %>%
          ggplot(aes(x = distmiles, y = entropy, key = tract_id,
                     text = paste("Tract ID:", tract_id,
                                  "<br>Diversity Score:", round(entropy, 3),
                                  "<br>Distance (miles):", round(distmiles, 2)))) +
            geom_point() + 
            geom_smooth(aes(key = NULL, text = NULL), 
                        se=FALSE, span = input$span, color = "blue", method = "loess")+
            labs(
              title = paste("Diversity Score vs. Distance for", input$city),
              x = "Distance from city hall (miles)",
              y = "Diversity score (entropy)"
            ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
      ggplotly(p, source = "plotly_scatterplot", tooltip = "text") %>%
        event_register("plotly_selected") %>%
        layout(dragmode = "select", 
               hoverlabel = list(bgcolor = "white", font = list(size=12)))
    })
    
    output$mapPlot <- renderPlot({
      by_city<- data_by_dist%>%
        filter(metro_name == input$city)
      ed <- event_data("plotly_selected", source = "plotly_scatterplot")
      #browser()
      if (is.null(ed) | length(ed) == 0) {
        p_map <- data_by_dist%>%
          filter(metro_name == input$city) %>%
          ggplot() +
          geom_sf(aes(fill = entropy), size = 0.1) + 
          labs(
            title = paste("Map of Diversity Scores in", input$city), 
            x = NULL, 
            y = NULL
          ) +
          theme_minimal() +
          theme(
            axis.text = element_blank(), 
            axis.ticks = element_blank(), 
            panel.grid = element_blank(), 
            plot.title = element_text(hjust = 0.5) 
          )} else{
            if (nrow(ed) > 0) {
              selected_tract_ids <- ed$key
              selected_tracts_sf <- by_city %>%
                filter(tract_id %in% selected_tract_ids)
              zoomed_bbox <- st_bbox(selected_tracts_sf)
              if (nrow(selected_tracts_sf) > 0) {
                p_map <- ggplot() +
                  geom_sf(data = by_city, aes(fill = entropy), color = "white", size = 0.05) +
                  geom_sf(data = selected_tracts_sf, aes(fill = entropy), color = "red", size = 1)  + 
                  coord_sf(
                    xlim = c(zoomed_bbox["xmin"], zoomed_bbox["xmax"]),
                    ylim = c(zoomed_bbox["ymin"], zoomed_bbox["ymax"]),
                    expand = FALSE
                  ) +
                  labs(x = NULL, y = NULL) +
                  theme_minimal() +
                  theme(
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.grid = element_blank(),
                    plot.caption = element_text(size = 8, hjust = 1)
                  )
              }
             }
          }
      print(p_map)
    })
    
    output$barPlot <- renderPlot({
      by_city<- data_by_dist%>%
        filter(metro_name == input$city)
      required_race_cols <- names(race_lookup)
      actual_cols <- names(st_drop_geometry(by_city)) 
      missing_cols <- setdiff(required_race_cols, actual_cols)
      race_summary <- by_city%>%
        st_drop_geometry() %>% 
        select(all_of(required_race_cols)) %>% 
        summarise(across(everything(), sum, na.rm = TRUE)) %>% 
        pivot_longer(cols = everything(), names_to = "Race_Code", values_to = "Count") %>% 
        mutate(Race = recode(Race_Code, !!!race_lookup, .default = Race_Code))
      race_summary %>%
        ggplot(aes(x = Race, y = Count)) +
        geom_col() + 
        scale_y_continuous(labels = scales::comma) + 
        labs(
          title = paste("Overall Race Distribution in", input$city), 
          x = NULL, 
          y = "Count"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 30, hjust = 1), 
          plot.title = element_text(hjust = 0.5) 
        )
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
