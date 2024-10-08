
# Loading in necessary libraries we will need
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(shiny)
library(ggplot2)

# Dataset
airline_data = read.csv('/Users/hassanmarji/Desktop/airline_review.csv')

# We went through the 'ratings' and 'header' column, and created a new 'comments' column based on that to categorize the ratings
airline_data = airline_data %>%
  mutate(comments = case_when(
    rating >= 1 & rating <= 3 ~ "disgrace",
    rating >= 4 & rating <= 6 ~ "average",
    rating >= 7 & rating <= 8 ~ "good",
    rating >= 9 & rating <= 10 ~ "great"
  ))

# We also decided to split the 'route' column. This was relatively simple as it was mainly split by the word 'to' and some had to be split using 'via' as well
airline_data = airline_data %>%
  separate(route, into = c("from", "to"), sep = " to ", extra = "drop", fill = "right") %>%
  separate(to, into = c("to", NA), sep = " via", extra = "drop")

# Converting the 'date' and 'date_flown' columns to a date format 
# We also filtered rows with missing dates. We did this because we use this in one of our visuals, and it would not work if we had missing/NA values.
airline_data = airline_data %>%
  mutate(date = ymd(date),
         date_flown = ymd(date_flown)) %>%
  filter(!is.na(date) & !is.na(date_flown))

# The 'aircraft' column needed some cleaning. For example, some planes were labeled as Boeing 777, B777, and 777. These are the same thing, so we had to label them as the same thing.
# Some aircrafts also have different types. Ex: B777-300ER or B777-200, etc. These are all part of the same B777 'family' so we put them all under that.

airline_data <- airline_data %>%
  mutate(aircraft = str_split(aircraft, "and", simplify = TRUE)[,1]) %>%
  mutate(aircraft = str_remove_all(aircraft, "(?i)Boeing|Airbus|B0eing|Finnair|Embraer|neo|Neo| neo|E|Various|Boieng")) %>%
  mutate(aircraft = str_split(aircraft, "[-,/, ,&,]", simplify = TRUE)[,1]) %>%
  mutate(aircraft = str_trim(aircraft)) %>%
  # Add 'B' in front of certain aircraft models
  mutate(aircraft = case_when(
    str_detect(aircraft, "^777") ~ paste0("B", aircraft),
    str_detect(aircraft, "^767") ~ paste0("B", aircraft),
    str_detect(aircraft, "^787") ~ paste0("B", aircraft),
    aircraft == "Dramlinr" ~ "Dreamlnr",   
    aircraft == "" ~ "other",               
    aircraft == "170" ~ "E170",           
    aircraft == "Saab" ~ "SAAB",           
    aircraft == "190" ~ "MB190",            
    TRUE ~ aircraft                         
  ))


# We noticed some entries in the 'from' and 'to' columns basically meant the same location, so we 'grouped' them all into one.
airline_data = airline_data %>%
  mutate(
    from = case_when(
      from %in% c("Heathrow", "London Heathrow") ~ "London",
      TRUE ~ from  
    ),
    to = case_when(
      to %in% c("Heathrow", "London Heathrow") ~ "London",
      TRUE ~ to  
    )
  )


# UI
ui = fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      conditionalPanel(
        condition = "input.tab_selected == 'Ratings Distribution'",
        selectInput('traveller_type', 'Select Traveller Type:', choices = c('All', unique(airline_data$traveller_type))),
        selectInput('seat_type', 'Select Seat Type:', choices = c('All', unique(airline_data$seat_type))),
        selectInput('color_choice', 'Choose Bar Color:', choices = c('blue1', 'orange2', 'green3', 'thistle1')),
        dateRangeInput('date_range', 'Select Date Range:', 
                       start = ifelse(is.na(min(airline_data$date)), default_start, min(airline_data$date)), 
                       end = ifelse(is.na(max(airline_data$date)), default_end, max(airline_data$date)))
      ),
      
      conditionalPanel(
        condition = "input.tab_selected == 'Sentiment Analysis'",
        selectInput('traveller_type_sentiment', 'Select Traveller Type:', choices = c('All', unique(airline_data$traveller_type))),
        selectInput('seat_type_sentiment', 'Select Seat Type:', choices = c('All', unique(airline_data$seat_type))),
        dateRangeInput('date_range_sentiment', 'Select Date Range:', 
                       start = ifelse(is.na(min(airline_data$date)), default_start, min(airline_data$date)), 
                       end = ifelse(is.na(max(airline_data$date)), default_end, max(airline_data$date)))
      ),
      
      conditionalPanel(
        condition = "input.tab_selected == 'Ratings Over Time'",
        selectInput('traveller_type_time', 'Select Traveller Type:', choices = c('All', unique(airline_data$traveller_type))),
        selectInput('seat_type_time', 'Select Seat Type:', choices = c('All', unique(airline_data$seat_type)))
        # Time filter is fixed to Yearly, so no need for additional input here
      ),
      
      conditionalPanel(
        condition = "input.tab_selected == 'Ratings by Aircraft Model'",
        selectInput('seat_type_aircraft', 'Select Seat Type:', choices = c('All', unique(airline_data$seat_type))),
        selectInput('traveller_type_aircraft', 'Select Traveller Type:', choices = c('All', unique(airline_data$traveller_type))),
        dateRangeInput('date_range_aircraft', 'Select Date Flown:', start = min(airline_data$date_flown), end = max(airline_data$date_flown))
      ),
      
      conditionalPanel(
        condition = "input.tab_selected == 'Route Performance'",
        selectInput('from', 'From:', choices = c('All', unique(airline_data$from))),
        uiOutput('to_filter'), # Dynamically generated based on 'from'
        selectInput('traveller_type_route', 'Select Traveller Type:', choices = c('All', unique(airline_data$traveller_type))),
        selectInput('aircraft_type_route', 'Select Aircraft Type:', choices = c('All', unique(airline_data$aircraft)))
      )
      
    ),
    
    mainPanel(
      
      # Tabs for different visualizations
      tabsetPanel(id = 'tab_selected',
                  tabPanel('Ratings Distribution', plotOutput('ratings_dist')),
                  tabPanel('Sentiment Analysis', plotOutput('sentiment_analysis')),
                  tabPanel('Ratings Over Time', plotOutput('ratings_over_time')),
                  tabPanel('Ratings by Aircraft Model', plotOutput('ratings_by_aircraft')),
                  tabPanel('Route Performance', plotOutput('route_performance'))
      )
      
    )
  )
)

# Server
server = function(input, output, session){
  
  
  minimal_theme = theme_minimal() + 
    theme(
      axis.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  
  # We set a fixed count limit because we wanted to user to actually the incresae/decrease in the graphs/
  fixed_count_limit <- 500  
  
  # Plot 1: Overall Ratings Distribution
  output$ratings_dist = renderPlot({
    airline_data %>%
      filter(
        (traveller_type == input$traveller_type | input$traveller_type == 'All'),
        (seat_type == input$seat_type | input$seat_type == 'All'),
        date >= input$date_range[1] & date <= input$date_range[2]
      ) %>%
      ggplot(aes(x = factor(rating))) + 
      geom_bar(fill = input$color_choice) +
      labs(title = 'Distribution of Ratings', x = 'Rating (1 = Worst, 10 = Best)', y = 'Number of Reviews') +
      scale_x_discrete(labels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10')) +
      scale_y_continuous(limits = c(0, fixed_count_limit)) +  # Fixed y-axis
      minimal_theme
  })
  
  # Plot 2: Sentiment Analysis by Rating Category
  output$sentiment_analysis = renderPlot({
    airline_data %>%
      filter(
        (traveller_type == input$traveller_type_sentiment | input$traveller_type_sentiment == 'All'),
        (seat_type == input$seat_type_sentiment | input$seat_type_sentiment == 'All'),
        date >= input$date_range_sentiment[1] & date <= input$date_range_sentiment[2]
      ) %>%
      ggplot(aes(x = comments, fill = comments)) + 
      geom_bar() +
      labs(title = 'Sentiment Analysis by Rating Category', x = 'Sentiment', y = 'Number of Reviews') +
      scale_fill_manual(values = c('disgrace' = 'red', 'average' = 'orange', 'good' = 'yellow', 'great' = 'lightblue1')) +
      scale_y_continuous(limits = c(0, fixed_count_limit)) +  # Fixed y-axis
      minimal_theme +
      theme(legend.position = 'none')
  })
  
  # Fixed limits for ratings 
  fixed_rating_limit = c(0, 10)
  
  # Plot 3: Ratings Over Time (We chose to do this over a yearly basis to demonstrate actual change from year to year)
  output$ratings_over_time = renderPlot({
    time_data = airline_data %>%
      mutate(year = year(date)) %>%
      filter(
        (traveller_type == input$traveller_type_time | input$traveller_type_time == 'All'),
        (seat_type == input$seat_type_time | input$seat_type_time == 'All')
      )
    
    time_data %>%
      group_by(year) %>%
      summarise(avg_rating = mean(rating)) %>%
      ggplot(aes(x = year, y = avg_rating)) +
      geom_line(color = 'darkorange') +
      labs(title = 'Average Rating Over Time (Yearly)', x = 'Year', y = 'Average Rating') +
      scale_x_continuous(breaks = unique(time_data$year)) +
      scale_y_continuous(limits = fixed_rating_limit) + 
      minimal_theme
  })
  
  # Plot 4: Ratings by Aircraft Model
  output$ratings_by_aircraft = renderPlot({
    airline_data %>%
      filter(
        (seat_type == input$seat_type_aircraft | input$seat_type_aircraft == 'All'),
        (traveller_type == input$traveller_type_aircraft | input$traveller_type_aircraft == 'All'),
        date_flown >= input$date_range_aircraft[1] & date_flown <= input$date_range_aircraft[2]
      ) %>%
      group_by(aircraft) %>%
      summarise(avg_rating = mean(rating)) %>%
      ggplot(aes(x = reorder(aircraft, avg_rating), y = avg_rating)) +
      geom_bar(stat = 'identity', fill = 'steelblue') +
      labs(title = 'Average Ratings by Aircraft Model', x = 'Aircraft Model', y = 'Average Rating') +
      scale_y_continuous(limits = c(0, 10)) +  
      minimal_theme
  })
  
  # We wanted the 'to' filter to change to the countries where the airline flies when the 'from' is selected.
  output$to_filter = renderUI({
    req(input$from)
    available_to = unique(airline_data$to[airline_data$from == input$from])
    selectInput('to', 'To:', choices = c('All', available_to))
  })
  
  # Route Performance 
  output$route_performance = renderPlot({
    filtered_data = airline_data %>%
      filter(
        (from == input$from | input$from == 'All'),
        (to == input$to | input$to == 'All'),
        (traveller_type == input$traveller_type_route | input$traveller_type_route == 'All'),
        (aircraft == input$aircraft_type_route | input$aircraft_type_route == 'All')
      )
    
    # Grouped bar chart for average ratings by route chosen by the user.
    filtered_data %>%
      group_by(from, to) %>%
      summarise(avg_rating = mean(rating)) %>%
      ggplot(aes(x = from, y = avg_rating, fill = to)) +
      geom_bar(stat = 'identity', position = "dodge") +
      labs(title = 'Route Performance (Average Rating)', x = 'From', y = 'Average Rating') +
      scale_y_continuous(limits = c(0, 10)) +
      minimal_theme +
      theme(legend.position = 'bottom')
  })
}

# Run the Shiny app
shinyApp(ui, server)
    









