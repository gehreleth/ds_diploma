if(!exists("m", mode="list")) source("build_model.R")

function(session, input, output) {
  completions <- reactive( {predict.next.word(m, input$text, num.possibilities = 4)$token })

  output$completions <- renderUI({
    completions <- completions()
    if (length(completions) > 0) {
      buttonCount <- min(4, length(completions))
      mapply(function(arg) {
        tagList(actionButton(paste("completion", arg, sep=''), width = 200, label = { completions[arg] }))
      }, arg = 1:buttonCount)
    }
  })
  
  observeEvent(input$completion1, {
    updateTextInput(session, "text", value=apply.completion(input$text, completions()[1]))
  })
  
  observeEvent(input$completion2, {
    updateTextInput(session, "text", value=apply.completion(input$text, completions()[2]))
  })  
  
  observeEvent(input$completion3, {
    updateTextInput(session, "text", value=apply.completion(input$text, completions()[3]))
  })  

  observeEvent(input$completion4, {
    updateTextInput(session, "text", value=apply.completion(input$text, completions()[4]))
  })  
}