library(shiny)
cilist        <- list(.8,.9,.95,.99)
names(cilist) <- paste(unlist(cilist)*100,'%')
# my class project
shinyUI(fluidPage(
    titlePanel(h3("How to Win Friends & Influence People in Congress")),
    sidebarLayout(
        sidebarPanel(h4("A guide for the numerate politician-shopper"),
                     p("Say you're a security and defense interest and want to defeat the Amash amendment."), 
                     radioButtons("ass", label = h4("Your assumptions:"),
                                  choices = list("Baseline difference by party, but same response to funding." = "Model 1", 
                                                 "Both baseline and response to funding are different." = "Model 2"),
                                  selected = "Model 1"),                     
                     radioButtons("pay", label = h4("How much:"),
                                  choices = list("Minimum: the median of the probability range of a vote in your favor is above 50%." = "min", 
                                                 "Safe: the bottom of the probability range of a vote in your favor is above 50% with a confidence level chosen below." = "safe"),
                                  selected = "min"),  
                     radioButtons("ci", label = h4("Confidence level:"), 
                                  choices = cilist,
                                  selected = cilist[[1]]),
                     actionButton("goButton","Go")),
        mainPanel(
            plotOutput("thePlot")
        )
    )
))
