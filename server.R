source('prediction.R')

library(shiny)
library(wordcloud)
library(ggplot2)

# Define application ####

shinyServer(function(input, output) {
  
  prediction =  reactive({
    
    inputText = input$text
    n_count=stri_count_words(inputText)
    if (n_count>=2)
    {
      input1 =  word(inputText,n_count)
      input2 =  word(inputText,n_count-1)
    }
    else if (n_count==1)
    {
      input1 =  word(inputText,n_count)
      input2 =  ""
    }
    else
    {
      input1 =  ""
      input2 =  ""
    }
    nSuggestion = input$slider
    
    if(input$dist == "cum")
    {prediction = fun.predict(input1, input2, n = nSuggestion)}
    else if (input$dist == "tri")
    {prediction = fun.predict3(input1, input2, n = nSuggestion)} 
    else if (input$dist == "bi")
    {prediction = fun.predict2(input1, input2, n = nSuggestion)}
    else if (input$dist == "uni")
    {prediction = fun.predict1(input1, input2, n = nSuggestion)}
  })
  
  
  output$text1<-renderText({if(input$dist == "cum"){paste("Prediction based on Tri/Bi/Uni-grams combination")}
    else if(input$dist == "tri"){paste("Prediction based on Tri-gram")}
    else if(input$dist == "bi"){paste("Prediction based on Bi-gram")}
    else if(input$dist == "uni"){paste("Prediction based on Uni-gram")}
  })
  
  output$table = renderDataTable(prediction())
  
  
  wordcloud_rep = repeatable(wordcloud)
  output$wordcloud = renderPlot(
    wordcloud_rep(
      na.omit(prediction()$Nextword),
      na.omit(prediction()$freq),
      colors = brewer.pal(8, 'Dark2'),
      scale=c(4, 0.5),
      max.words = 300
    )
  )
  
})
