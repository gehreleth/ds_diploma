if(!exists("m", mode="list")) source("build_model.R")

function(session, input, output) {
  completions <- reactive( {predict.next.word(m, input$text, num.possibilities = 4)$token })

  output$completions <- renderUI({
    tagList(
      actionButton("completion1", width = 200, label = { completions()[1] }),
      actionButton("completion2", width = 200, label = { completions()[2] }),
      actionButton("completion3", width = 200, label = { completions()[3] }),
      actionButton("completion4", width = 200, label = { completions()[4] })
    )
  })
  
  observeEvent(input$completion1, {
    updateTextInput(session, "text", value=paste(input$text, completions()[1], sep = ' '))
  })
  
  observeEvent(input$completion2, {
    updateTextInput(session, "text", value=paste(input$text, completions()[2], sep = ' '))
  })  
  
  observeEvent(input$completion3, {
    updateTextInput(session, "text", value=paste(input$text, completions()[3], sep = ' '))
  })  

  observeEvent(input$completion4, {
    updateTextInput(session, "text", value=paste(input$text, completions()[4], sep = ' '))
  })  
}