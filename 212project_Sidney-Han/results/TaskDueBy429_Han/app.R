#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud2)

review_data_raw <-  read_csv("../../data/clean/primarycleaned_reviews_data.csv")


review_data <- review_data_raw %>%
  filter(!is.na(stars), !is.na(review_text)) %>% 
  mutate(stars = as.character(round(as.numeric(stars)))) 

data("stop_words")
other_stop_words <- tibble(
  word = c("book", "page", "pages", "read", "books", "story", "stories", "review", "reviews", "written", "reading", "writing") 
)
all_stop_words <- bind_rows(stop_words, other_stop_words) %>%
  distinct()

unique_ratings <- review_data %>%
  distinct(stars) %>%
  pull(stars) %>%
  sort()

# --- UI Definition ---
ui <- fluidPage(
  titlePanel("Book Review Word Clouds by Rating"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("rating_select", "Select Star Rating:",
                  choices = c("--- Select ---" = "", unique_ratings), # Add a placeholder
                  selected = ""), 
      hr(),
      h4("Click a word below for details:"),
      uiOutput("word_buttons_ui") ,
      hr(),
      h4("Distribution of Ratings (Click bars to select/deselect)"),
      plotOutput("rating_distribution_plot", height = "250px", click = "plot_click")
    ),
    
    mainPanel(
      h3(textOutput("wordcloud_title", inline = TRUE)),
      wordcloud2Output("word_cloud_plot", height = "500px"), 
      hr(),
      h4("Word Details:"),
      verbatimTextOutput("word_details_output") 
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  selected_ratings_rv <- reactiveVal(character(0))
  selected_word <- reactiveVal(NULL)
  observeEvent(input$rating_select, {
    req(input$rating_select != "")
    current_selection <- selected_ratings_rv()
    new_selection <- input$rating_select
    if (!is.null(input$rating_select) && input$rating_select != "" &&
        !identical(selected_ratings_rv(), input$rating_select)) { 
      selected_ratings_rv(input$rating_select)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_click, {
    clicked_index <- round(input$plot_click$x)
    if (clicked_index >= 1 && clicked_index <= length(unique_ratings)) {
      clicked_star <- unique_ratings[clicked_index]
      current_selection <- selected_ratings_rv()
      if (clicked_star %in% current_selection) {
        new_selection <- current_selection[current_selection != clicked_star] # Deselect
      } else {
        new_selection <- c(current_selection, clicked_star) # Select
      }
      
      new_selection <- unique(sort(new_selection))
      selected_ratings_rv(new_selection)
      selected_word(NULL)
    }
  })
  
  observe({
    current_sel <- selected_ratings_rv() 
    if (length(current_sel) == 1) {
      if (!identical(input$rating_select, current_sel)) {
        updateSelectInput(session, "rating_select", selected = current_sel)
      }
    } else {
      if (!identical(input$rating_select, "")) {
        updateSelectInput(session, "rating_select", selected = "")
      }
    }
  })
  
  rating_word_data <- reactive({
    selected_r <- selected_ratings_rv()
    req(length(selected_r) > 0)
    rating_specific_data <- review_data %>%
      filter(stars %in% selected_r)
    req(nrow(rating_specific_data) > 0)
    rating_words <- rating_specific_data %>%
      unnest_tokens(word, review_text) %>%
      anti_join(all_stop_words, by = "word") %>%
      filter(!str_detect(word, "^\\d+$")) %>% 
      filter(nchar(word) > 2) %>%             
      count(word, sort = TRUE) %>%
      filter(n >= 5) 
    
    list(words = rating_words, reviews = rating_specific_data)
  })
  
  
  
  output$word_cloud_plot <- renderWordcloud2({
    selected_r <- selected_ratings_rv()
    if(length(selected_r) == 0) return(NULL)
    
    word_data_list <- rating_word_data()
    word_data <- rating_word_data()$words
    req(nrow(word_data) > 0)
    
    word_data_for_cloud <- word_data %>% select(word, freq = n) %>% head(150) 
    
    wordcloud2(word_data_for_cloud, size = 0.7, shape = 'circle',
               color='random-dark', backgroundColor = "white")
  })
  
  wordcloud_title_reactive <- reactive({
    selected_r <- selected_ratings_rv()
    if (length(selected_r) == 0) {
      "Select Rating(s) to Generate Word Cloud"
    } else {
      paste("Word Cloud for", paste(selected_r, collapse=", "), "Star Reviews")
    }
  })
  output$wordcloud_title <- renderText({
    wordcloud_title_reactive() 
  })
  
  output$word_buttons_ui <- renderUI({
    selected_r <- selected_ratings_rv()
    if(length(selected_r) == 0) return(NULL)
    word_data_list <- rating_word_data()
    word_data <- rating_word_data()$words
    req(nrow(word_data) > 0) 
    
    top_words <- word_data %>% head(20) 
    
    buttons <- lapply(top_words$word, function(w) {
      button_id <- paste0("word_btn_", gsub("[^A-Za-z0-9]", "_", w))
      actionLink(inputId = button_id, label = w, style = "margin-right: 10px; display: inline-block;")
    })
    
    fluidRow(buttons)
  })
  observe({
    word_data <- rating_word_data()$words
    if(is.null(word_data) || nrow(word_data) == 0) return()
    
    top_words <- word_data %>% head(20)
    
    lapply(top_words$word, function(w) {
      button_id <- paste0("word_btn_", gsub("[^A-Za-z0-9]", "_", w))
      observeEvent(input[[button_id]], {
        selected_word(w)
      }, ignoreInit = TRUE)
    })
  })
  
  
  output$word_details_output <- renderText({
    clicked_word <- selected_word() 
    req(clicked_word) 
    current_data <- rating_word_data()
    word_freqs <- current_data$words
    specific_reviews <- current_data$reviews
    selected_r_text <- paste(selected_ratings_rv(), collapse=", ")
    
    word_info <- word_freqs %>% filter(word == clicked_word)
    
    if (nrow(word_info) > 0) {
      freq <- word_info$n[1]
      
      example_review_df <- specific_reviews %>%
        filter(str_detect(review_text, regex(paste0("\\b", clicked_word, "\\b"), ignore_case = TRUE))) %>%
        slice(1)
      
      example_review_text <- if(nrow(example_review_df) > 0) {
        paste(strwrap(example_review_df$review_text[1], width = 70), collapse = "\n")
      } else {
        paste0(" (No example found containing this exact word for ", selected_r_text, "-star reviews)")
      }
      
      paste0("Word: '", clicked_word, "'\n",
             # Clarify frequency is across selected ratings
             "Frequency in ", selected_r_text, "-star reviews: ", freq, "\n\n",
             "Example Review (from selected ratings):\n",
             example_review_text
      )
    } else {
      paste("Details for '", clicked_word, "' not found for ", selected_r_text, "-star reviews.")
    }
  })
  
  output$rating_distribution_plot <- renderPlot({
    library(ggplot2)
    selected_r <- selected_ratings_rv()
    plot_data <- review_data %>%
      mutate(stars_factor = factor(stars, levels = unique_ratings))
    ggplot(data = plot_data, aes(x = stars_factor)) +
      geom_bar(aes(fill = ifelse(stars_factor %in% selected_r, "Selected", "Other")), show.legend = FALSE) +

      scale_fill_manual(values = c("Selected" = "darkorange", "Other" = "steelblue")) +
      labs(
        x = "Star Rating",
        y = "Number of Reviews"
      ) +
      scale_x_discrete(drop = FALSE) +
      theme_minimal(base_size = 12)
  
  })

  
} 

# --- Run the App ---
shinyApp(ui = ui, server = server)