library(shiny)

fluidPage(
  titlePanel('Data Science Specialization - Capstone Final Project')
  ,sidebarLayout(
    sidebarPanel(
      helpText(
        'This webapp demonstrates a simple text prediction engine.'
        ,'Type in a few words below and see a helpful prediction to the right.'
      )
      ,textInput(
        "text_leading"
        ,'Leading text to predict from'
        ,value = 'I love'
      )
    )
    ,mainPanel(
      helpText('A helpful prediction of the next word is:')
      ,textOutput('text_next_word')
    )
  )
)
