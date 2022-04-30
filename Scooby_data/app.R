#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)
library(RColorBrewer)

# Get and read in data (code provided by creator for the TidyTuesday challenge)
# Or read in the data manually
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

# Pivot longer to condense caught_[character] in order to see who caught the
# culprit in that episode
scoobydoo_tidy <- scoobydoo %>% 
  pivot_longer(cols = caught_fred:caught_scooby,
               names_to = "caught_by",
               values_to = "were_caught") %>% 
  filter(!is.na(were_caught))

# remove rows where the character does not catch the culprit
scoobydoo_tidy <- subset(scoobydoo_tidy, were_caught==TRUE)

# replace value of caught_by with only the name of the character
scoobydoo_tidy <- scoobydoo_tidy %>% 
  mutate(caught_by = str_match(caught_by, pattern = "fred|daphnie|velma|shaggy|scooby"))

scoobydoo_tidy <- scoobydoo_tidy %>% 
  mutate(date_aired = year(date_aired))
  
# create a list of character names from data
character_options <- scoobydoo_tidy %>% 
  filter(!is.na(caught_by)) %>% 
  distinct(caught_by)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Scooby-Doo TV Series Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Select tab to show what data to display:"),
            h4("- Distribution: How Often a Character From the Mystery Inc. has Caught the Culprit per TV Series"),
            h4("- Motives: Motives of the Crimes Throughout the Whole Franchise"),
            h4("- Engagement: Top Most Engaged Episodes by July 2021")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Distribution",
                               selectInput("character_to_highlight",
                                           "Choose character to highlight:",
                                           choices = character_options),
                               plotOutput("distPlot",
                                          height = "800px")),
                      tabPanel("Motives",
                               plotOutput("bar_plot",
                                                     height = "800px")),
                      tabPanel("Engagement",
                               sliderInput("num_episodes",
                                           "Select the number of episodes to display:",
                                           min = 1,
                                           max = 50,
                                           value = 10),
                               plotOutput("engage_bar_plot",
                                          height = "800px"))
            
          )
           
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Who has caught the culprit the most per series? How has the frequency of this character catching
    # the culprit change over the year as new series are created?
    output$distPlot <- renderPlot({
       all_series <-  scoobydoo_tidy %>% 
          filter(format %in% c("TV Series", "TV Series (segmented)")) %>%
          group_by(series_name) %>%
          count(caught_by) %>%
          mutate(percentage = round(n/sum(n) * 100, digits = 0))
      
        highlighted_character <- all_series %>% 
          filter(caught_by == input$character_to_highlight)
        
        year_order <-  scoobydoo_tidy %>% 
          filter(format %in% c("TV Series", "TV Series (segmented)")) %>%
          group_by(series_name, date_aired) %>%
          count(caught_by)
      
        all_series %>% 
          ggplot(aes(x = series_name,
                     y = percentage,
                     group = caught_by),
                 srt = 35) +
          geom_line(color = "grey") +
          geom_line(data = highlighted_character,
                    color = "red") +
          geom_point(color = "grey")+
          geom_point(data = highlighted_character,
                     color = "red") +
          geom_label(data = highlighted_character,
                     color = "red",
                     aes(label = percentage)) +
          scale_x_discrete(name = "Series Name (in order of year)", 
                             limits=c("Scooby Doo, Where Are You!","The New Scooby-Doo Movies",
                                      "The Scooby-Doo Show", "Scooby-Doo and Scrappy-Doo (first series)",
                                      "Scooby-Doo and Scrappy-Doo (second series)",
                                      "The New Scooby and Scrappy Doo Show", "The New Scooby-Doo Mysteries",
                                      "The 13 Ghosts of Scooby-Doo", "A Pup Named Scooby-Doo", 
                                      "What's New Scooby-Doo?",
                                      "Scooby-Doo Mystery Incorporated", "Warner Home Video",
                                      "Be Cool, Scooby-Doo!", "Lego", "Scooby-Doo and Guess Who?")) +
          theme(axis.text.x = element_text(angle = 35, hjust=.75)) +
          labs(y = "Percentage of episodes character from the group caught the culprit",
               title = "How Often a Character From the Mystery Inc. has Caught the Culprit per TV Series") +
          theme(text = element_text(size = 15))  
    })
    
    # Which motive was common throughout the franchise?
    output$bar_plot <- renderPlot({
      
      scoobydoo_tidy %>% 
        filter(motive != "NULL") %>% 
        group_by(motive) %>% 
        count(motive) %>% 
        ggplot(aes(x = n,
                   y = reorder(motive, n))) +
        geom_col()  +
        geom_label(aes(label = n)) +
        labs(x = "count",
             y = "Motives",
             title = "Motives of the Crimes Throughout the Whole Franchise") +
        theme(text = element_text(size = 20))
    })
    
    
    # What were the top ten most engaged episodes in the franchise up 'til July 2021? 
    output$engage_bar_plot <- renderPlot({
      engagement_tidy <- scoobydoo %>%
        filter(engagement != "NULL") %>%
        filter(format %in% c("TV Series", "TV Series (segmented)")) %>%
        mutate(engagement = as.numeric(engagement))
      
      engagement_tidy %>% 
        top_n(as.numeric(input$num_episodes), engagement) %>% 
        ggplot(aes(x = engagement,
                   y = reorder(title, engagement),
                   fill = series_name)) +
        geom_col()  +
        geom_label(aes(label = engagement)) +
        labs(x = "Number of IMDB Reviews",
             y = "Epsiode Title",
             title = "Top Most Engaged Episodes by July 2021") +
        theme(text = element_text(size = 20)) +
        scale_fill_hue(c=45, l=40)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
