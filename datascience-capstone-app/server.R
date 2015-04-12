library(shiny)
print('Initializing Shiny App')

print('Loading logic file')
source('logic.R')
print('Finished loading logic file')

print('Moving into reactive state')
shinyServer(function(input, output, session) {
  
  output$text_next_word <- renderText({
    PredictNextWord(
      input$text_leading
    )
  })
  
})
