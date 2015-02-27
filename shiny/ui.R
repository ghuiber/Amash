library(shiny)
cilist        <- list(.8,.9,.95,.99)
names(cilist) <- paste(unlist(cilist)*100,'%')
# my class project
shinyUI(fluidPage(
    titlePanel(h3("How to win friends & influence people in Congress")),
    sidebarLayout(
        sidebarPanel(h4("A guide for the numerate politician-shopper"),
                     p("Say you're a security and defense interest and want to defeat the Amash amendment."), 
                     radioButtons("ass", label = h4("Your assumptions:"),
                                  choices = list("Baseline difference by party, but same response to funding." = "Model 1", 
                                                 "Both baseline and response to funding are different." = "Model 2"),
                                  selected = "Model 1"),                     
                     radioButtons("pay", label = h4("Funding type:"),
                                  choices = list("Minimum: clear the 50% chance of a vote in your favor, on average." = "min", 
                                                 "Safe: clear the 50% chance of a vote in your favor with a confidence level chosen below." = "safe"),
                                  selected = "min"),  
                     radioButtons("ci", label = h4("Confidence level:"), 
                                  choices = cilist,
                                  selected = cilist[[1]]),
                     actionButton("goButton","Go")),
        mainPanel(
            plotOutput("thePlot")
            #textOutput("theText")
        )
    )
))
