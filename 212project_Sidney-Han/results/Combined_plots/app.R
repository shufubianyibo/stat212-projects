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
library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud2)
library(ggplot2)

bestseller <- read_csv("../../data/clean/full_bestseller.csv")
clean <- bestseller %>% 
  group_by(title) %>%
  summarize(
    best_ranking = min(rank),
    max_weeks_on_list = max(weeks_on_list),
    publisher = first(publisher),
    author = first(author),
    .groups = "drop"
  )

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
ui <- navbarPage(
  
  # Application title
  "Understanding Literary Success: Investigating Factors Leading to Book Success",
  
  tabPanel("Introduction",
           h4("By Han Sun and Sidney Langford"),
           h3("About This Project"),
           p("Around 2.2 million books are published each year worldwide. With this many new titles to choose from, 
           how can one accurately predict which books will be a success? And how does one define success in literature?
           With factors including sales, critical acclaim, and reviews from everyday readers, many factors can highlight
           a book's success."),
           
           p("To better understand how these factors contribute to book success we looked into different metrics of book success: 
           Library Thing, a book review website in which users can rate and review books, and the NYT Bestseller List. The Library 
           Thing data provides insight into the readers' mindset and what is important to them about a book in regards to its rating (on a scale of 1-5) while 
           the NYT Bestseller List shows how books were ranked over time (as dictated by sales) and allows us to explore trends within books, 
           authors, publishers, and more. These two sources allow us to interact with the two most important sides of book success: the consumers
           and the publishers."),
           
           p("Click through the tabs to explore the data. In the 'Review Word Clouds' tab, explore in detail the most commonly used words in book
             reviews for each star rating, make sure to click on the words for extra information! In the 'Top Bestseller Runs' tab, filter by pubisher, author,
             and top ranking to explore trends in the number of books with the longest and shortest runs on the NYT Bestseller List!")
           
  ),
  
  tabPanel("Review Word Clouds",
           fluidPage(
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
  ),
  
  tabPanel("Top Bestseller Runs",
           fluidPage(
             
             # Application title
             titlePanel("Top Bestseller Runs"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("bins",          
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 
                 # Dropdown input for publishers
                 pickerInput(
                   "pub",
                   "Select Publisher(s) below:",
                   sort(unique(bestseller$publisher)),
                   options = list(`actions-box` = TRUE),
                   multiple = TRUE
                 ),
                 
                 # Dropdown input for authors
                 pickerInput(
                   "auth",
                   "Select Author(s) below:",
                   sort(unique(bestseller$author)),
                   options = list(`actions-box` = TRUE),
                   multiple = TRUE
                 ),
                
                 # Dropdown input for top rank 
                 pickerInput(
                   "rank",
                   "Select Top Rank(s) Below:",
                   c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                   options = list(`actions-box` = TRUE),
                   multiple = TRUE
                 ),
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot")
               )
             )
           )
  )
)


# --- Server Logic ---
server <- function(input, output, session) {
  
  # --- Server Logic for Review Word Clouds (Page 2) ---
  selected_ratings_rv <- reactiveVal(character(0)) 
  selected_word <- reactiveVal(NULL) 
  
  # Observer for dropdown selection
  observeEvent(input$rating_select, {
    if (!is.null(input$rating_select) && input$rating_select != "" &&
        !identical(selected_ratings_rv(), input$rating_select)) {
      selected_ratings_rv(input$rating_select)
      selected_word(NULL) 
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE) 
  
  # Observer for plot click
  observeEvent(input$plot_click, {
    clicked_index <- round(input$plot_click$x)
    if (!is.na(clicked_index) && clicked_index >= 1 && clicked_index <= length(unique_ratings)) {
      clicked_star <- unique_ratings[clicked_index]
      current_selection <- selected_ratings_rv()
      
      if (clicked_star %in% current_selection) {
        new_selection <- current_selection[current_selection != clicked_star] # Deselect
      } else {
        new_selection <- c(current_selection, clicked_star) # Select
      }
      
      new_selection <- unique(sort(new_selection))
      selected_ratings_rv(new_selection)
      selected_word(NULL) # Reset word selection when plot click changes rating selection
    }
  })
  
  # Observer to synchronize dropdown with reactive value
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
  
  # Reactive expression for word data based on selected ratings
  rating_word_data <- reactive({
    selected_r <- selected_ratings_rv()
    req(length(selected_r) > 0)
    
    rating_specific_data <- review_data %>%
      filter(stars %in% selected_r)
    
    req(nrow(rating_specific_data) > 0)
    
    # Process words
    rating_words <- rating_specific_data %>%
      unnest_tokens(word, review_text) %>%
      anti_join(all_stop_words, by = "word") %>%
      filter(!str_detect(word, "^\\d+$")) %>% 
      filter(nchar(word) > 2) %>% 
      count(word, sort = TRUE) %>%
      filter(n >= 5)
    
    list(words = rating_words, reviews = rating_specific_data)
  })
  
  # Render the word cloud
  output$word_cloud_plot <- renderWordcloud2({
    selected_r <- selected_ratings_rv()
    if(length(selected_r) == 0) return(NULL)
    
    word_data_list <- rating_word_data()
    word_data <- word_data_list$words
    
    req(nrow(word_data) > 0)
    
    word_data_for_cloud <- word_data %>% select(word, freq = n) %>% head(150) # Limit cloud size
    
    wordcloud2(word_data_for_cloud, size = 0.7, shape = 'circle',
               color='random-dark', backgroundColor = "white")
  })
  
  # Generate title for the word cloud
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
  
  # Generate actionLinks/buttons for top words
  output$word_buttons_ui <- renderUI({
    selected_r <- selected_ratings_rv()
    if(length(selected_r) == 0) return(NULL)
    
    word_data_list <- rating_word_data()
    word_data <- word_data_list$words
    
    req(nrow(word_data) > 0)
    
    top_words <- word_data %>% head(20) 
    buttons <- lapply(top_words$word, function(w) {
      button_id <- paste0("word_btn_", gsub("[^A-Za-z0-9]", "_", w))
      actionLink(inputId = button_id, label = w, style = "margin-right: 10px; margin-bottom: 5px; display: inline-block;")
    })
    
    fluidRow(buttons)
  })
  
  # Observer to handle clicks on the dynamically created word buttons
  observe({
    word_data_list <- rating_word_data()
    req(length(selected_ratings_rv()) > 0, !is.null(word_data_list))
    
    word_data <- word_data_list$words
    if(is.null(word_data) || nrow(word_data) == 0) return()
    
    top_words <- word_data %>% head(20)
    
    # Create observers for each potential button
    lapply(top_words$word, function(w) {
      button_id <- paste0("word_btn_", gsub("[^A-Za-z0-9]", "_", w))
      observeEvent(input[[button_id]], {
        selected_word(w) 
      }, ignoreInit = TRUE) 
    })
  })
  
  
  # Render the details for the selected word
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
        slice_sample(n = 1)
      
      example_review_text <- if(nrow(example_review_df) > 0) {
        paste(strwrap(example_review_df$review_text[1], width = 70), collapse = "\n")
      } else {
        paste0(" (No example review found containing the exact word '", clicked_word, "' for ", selected_r_text, "-star reviews)")
      }
      
      # Format the output text
      paste0("Word: '", clicked_word, "'\n",
             "Frequency in ", selected_r_text, "-star reviews: ", freq, "\n\n",
             "Random Example Review (from ", selected_r_text,"-star selection):\n",
             "---------------------------------------------------\n",
             example_review_text,
             "\n---------------------------------------------------"
      )
    } else {
      paste("Details for '", clicked_word, "' not found for ", selected_r_text, "-star reviews.")
    }
  })
  
  
  # Render the rating distribution plot
  output$rating_distribution_plot <- renderPlot({
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
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  }, res = 96) 
  
  
  # --- Server Logic for Bestseller Analysis (Page 3) ---
  output$distPlot <- renderPlot({
    x    <- faithful[, 2] 
    bin <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #create histogram with filtering in regards to dropdowns/sliders
    clean %>% 
      filter(max_weeks_on_list > 1) %>% 
      filter(publisher %in% input$pub) %>% 
      filter(author %in% input$auth) %>% 
      filter(best_ranking == as.numeric(input$rank)) %>% 
      ggplot(aes(x = max_weeks_on_list)) +
      geom_histogram(binwidth = input$bins, fill = "darkred") +
      labs(x = "Maximum # of Weeks on List", y = "# of Books")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
