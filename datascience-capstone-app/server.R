library(shiny)
print('Initializing Shiny App')

print('Loading logic file')
source('logic.R')
print('Finished loading logic file')

print('Moving into reactive state')
shinyServer(function(input, output, session) {
  
  output$text_next_word <- renderText({
    progress <- shiny::Progress$new()
    progress$set(message = "Computing next word", value = 0)
    on.exit(progress$close())
    updateProgress <- function(detail) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 3
      progress$set(value = value, detail = detail)
    }

    PredictNextWord(
      input$text_leading
      ,updateProgress
    )
  })
  
})
