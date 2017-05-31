if(!exists("m", mode="list")) source("build_model.R")

num.possibilities <- 8

function(session, input, output) {
  completions <- reactive( {predict.next.word(m, input$text, num.possibilities = num.possibilities)$token })
  output$completions <- renderUI({
    completions <- completions()
    if (length(completions) > 0) {
      buttonCount <- min(num.possibilities, length(completions))
      mapply(function(arg) {
        tagList(actionButton(paste("completion", arg, sep=''), width = 100, label = { completions[arg] }))
      }, arg = 1:buttonCount)
    }
  })
  
  mapply(function(arg) {
    observeEvent(input[[paste("completion", arg, sep='')]], {
      updateTextInput(session, "text", value=apply.completion(input$text, completions()[arg]))
    })
  }, arg = 1:num.possibilities)
}