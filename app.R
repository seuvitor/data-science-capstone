library(shiny)

source("word-prediction.R")

ui <- fluidPage(
  titlePanel("Smart Typing Machine"),
  tags$script('
      Shiny.addCustomMessageHandler("refocus",
        function(e_id) {
          document.getElementById(e_id).focus();
        });'),
  sidebarLayout(
    sidebarPanel(
      h4("Documentation"),
      p("Welcome to the Smart Typing Machine!"),
      p("This Shiny app was built for the Coursera/Johns Hopkins Data Science Capstone to showcase",
        "the habilities learned in the program."),
      p("You can ", strong("paste some text"), " in the box and hit enter, or you can also ",
        strong("just start typing"), " at will, and at all moments the app will show five words ",
        "that might be the next word you want to type, so that you can easily select it."),
      p("Under the hood, the app uses a Stupid Backoff type algorithm for prediction, based on a ",
        "n-gram probabilistic model that was built from the Coursera/SwiftKey dataset provided ",
        "by the course, containing large amounts of raw text from tweets, blog posts and news ",
        "articles. "),
      p("Enjoy!"),
      div(
        actionButton("reset", "Click here to start again!"), align="center"
      )
    ),
    mainPanel(
      h3(
        textOutput("caption", container = div),
        selectizeInput("predictionInput",
                       label = NULL,
                       choices = "",
                       multiple = FALSE,
                       options = list(create = TRUE))
      )
    )
  )
)

server <- function(input, output, session) {
  fullSentence <- reactiveVal("")
  output$caption <- renderText(fullSentence())
  
  # Join old text with the new selected word
  observe({
    selectedWord <- input$predictionInput
    if (trimws(selectedWord) != "") {
      newSentence <- trimws(paste(isolate(fullSentence()), selectedWord))
      fullSentence(newSentence)
    }
  })

  # Make a new prediction when the text changes
  observe({
    print("will predict and update the select")
    predictedWords <- predictNextWord(fullSentence())$feature
    updateSelectInput(session, "predictionInput",
                      choices = predictedWords,
                      selected = ""
    )
  })
  
  # Keep the input box focused
  observe({
    session$sendCustomMessage("refocus",list("predictionInput-selectized"))
    invalidateLater(100)
  })
  
  # Reset the text
  observeEvent(input$reset, {
    fullSentence("")
  })
  
}

shinyApp(ui = ui, server = server)