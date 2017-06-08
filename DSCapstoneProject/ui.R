library(shinythemes)
library(shinyjs)

fluidPage(
  useShinyjs(),
  theme = shinytheme("cerulean"),
  title = 'Z Word Predictor',
  div(
    id = "loading_page",
    h1("Loading model ...")
  ),
  hidden(
    div(
      id = "main_content",
      titlePanel("Z Word Predictor"),
      sidebarPanel(
        h4("Possible continuations"),
        tableOutput('continuations')
      ),
      mainPanel(
        fluidRow(
          column(12,
                 plotOutput("wordcloud"))
        ),
        fluidRow(
          column(12,
                 radioButtons("dict", "Dictionary type:",
                              c("Blogs" = "blogs",
                                "News" = "news",
                                "Twitter" = "twitter")))
        ),
        fluidRow(
          column(12,
                 textAreaInput("text",
                               label = h4("Enter text here"),
                               width = 500,
                               value = ""))
        )
      )
      
    )
  )
)
