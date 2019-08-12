# import libraries
library(tidyverse)
library(rvest)
library(tidytext)
library(shiny)
library(shinythemes)

# import profanity list
url = 'https://www.cs.cmu.edu/~biglou/resources/bad-words.txt'
profanity_list = read.delim(url, header = F, stringsAsFactors = F)
profanity_list = profanity_list %>% as_data_frame() %>% select(word = V1)

# define counter function
ProfanityCount = function(url) {
  # read page
  page = read_html(url)
  lyrics = page %>% html_nodes('.lyrics p') %>% html_text()
  
  # 'tidy' text
  tidy_lyrics = lyrics %>%
    as_data_frame() %>%
    unnest_tokens(word, value)
  
  # count profanity
  df = left_join(
    x = tidy_lyrics %>%
      filter(word %in% profanity_list$word) %>%
      group_by(word) %>% count(sort = T),
    y = profanity_list,
    by = 'word'
  )
  df = df %>% select(Word = word, Count = n)
  return(df)
}

# build shiny app
ui <- fluidPage(theme = shinytheme("superhero"),
                tags$h1(
                  tags$style(HTML("
                                  @import url('//fonts.googleapis.com/css?family=Roboto|Cabin:400,700');
                                  
                                  h1 {
                                  font-weight: 500;
                                  line-height: 1.1;
                                  color: #ffa200;
                                  }
                                  
                                  "))
                  ),
                h1("Lyrics Profanity Analyzer"),
                h4('Quickly measure profanity in any song, based on lyrics from genius.com.'),
                mainPanel(
                  
                  # input field
                  textInput("user_text", label = "Enter a song URL from genius.com", width = '500px', placeholder = "https://genius.com/Eminem-rap-god-lyrics"),
                  
                  # submit button
                  actionButton("submit", label = "Submit"),
                  
                  # display text output
                  br(),
                  br(),
                  tableOutput("table")
                )
                  )

server <- function(input, output) {
  
  # reactive expression
  url_reactive <- eventReactive(input$submit, {
    input$user_text
  })
  
  output$table <- renderTable({
    ProfanityCount(url_reactive())
  }, striped = F, hover = TRUE, bordered = TRUE, spacing = 'xs', align = 'l', width = '200px')
}

shinyApp(ui = ui, server = server)
