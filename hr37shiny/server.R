library(shiny)

# code that runs once when app is launched
# (getting ready for visitors)
source("helpers.R")

# this unnamed function runs every time
# the app is visited
shinyServer(function(input, output) {
    # code that runs once per visit goes here
    getInput <- reactive({
      out <- list()
      out$model <- as.character(input$ass)
      out$ci    <- as.numeric(input$ci)
      out$p     <- as.character(input$pay) 
      out$vote  <- as.character(input$vote)
      return(out)
    })  
    output$thePlot <- renderPlot({ 
      # code that runs when visitors changes
      # an input widget goes here
      input$goButton
      isolate(params <- getInput())
      getBigPicture(model=params$model,
                    ci=params$ci,
                    p=params$p,
                    vote=params$vote)
    })
})
