library(shinythemes)

fluidPage(
  theme = shinytheme("cerulean"),
  title = 'Autocompletion proof-of-concept',
  titlePanel("Autocompletion proof-of-concept"),
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
