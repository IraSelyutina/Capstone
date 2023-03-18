library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Word Predictor(based on Swiftkey source data)"),
  
  # Sidebar ####    
  sidebarLayout(
    
    sidebarPanel(
      
      # Text input
      textInput("text", label = ('Please enter some text for prediction next word'), value = ''),
      
      # Number of words slider input
      sliderInput('slider',
                  'Maximum number of predicted words',
                  min = 0,  max = 10,  value = 3
      ),
      radioButtons("dist", "Type of n-gram for prediction:",
                   c("Cumulative" = "cum",
                     "Trigram" = "tri",
                     "Bigram" = "bi",
                     "Unigram" = "uni"
                   ))
    ),
    
    # Mainpanel ####
    
    mainPanel(
      
      wellPanel(
        textOutput("text1"),
      ),
      
      
      dataTableOutput('table'),
      h3("Wordcloud of most probable next words"),
      plotOutput('wordcloud')
    )
  ) 
)
)
