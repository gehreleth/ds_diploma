fluidPage(
  title = 'Autocompletion proof-of-concept',
  titlePanel("Autocompletion proof-of-concept"),
  
  fluidRow(uiOutput("completions")),
  
  fluidRow(
    textAreaInput("text", 
                  label = h4("Enter text here"), 
                  value = "")
  )
)
