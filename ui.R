# Load libraries
# Load libraries
library(shiny)


shinyUI(
  pageWithSidebar(
    headerPanel("Predictive Text Modeling"),
    # Select inputs
    sidebarPanel(
      helpText("Navigate through the tabs displayed in the main-frame, in-order to have a look at the word prediction and exploratory data analysis"),
      textInput("userInput", "Type the word(s) in the given space:", value = "Good"),
      sliderInput("xPlot", "Enter the word limit for the word cloud:",
                  min = 0, max = 50,
                  value = 30),
      submitButton("Predict"),
      
      br()                
    ),
    
    
    mainPanel(
      # use tab format to display outputs
      tabsetPanel(
        
        # tab 1        
        tabPanel("About App",
                 withMathJax(),
                 br(),
                 includeMarkdown("include.Rmd")),
        
        # tab 2        
        tabPanel("Word Prediction",
                 mainPanel(
                  #  helpText("The top 3 word predictions are shown below (Hint: Biggest font size corresponds to highest probability):"),
                  # h1(textOutput("top1"), align = "center", style = "color:orange"),
                  # h2(textOutput("top2"), align = "center", style = "color:green"),
                  # h3(textOutput("top3"), align = "center", style = "color:blue"),
                  #  br(),
                   helpText("The top 10 word predictions and their corresponding rank are shown below (Hint: Rank 1 has the highest probability):"),
                   br(),
                   br(),
                   dataTableOutput("top11")
                 )),
        
        #tab 3
        tabPanel("Word Cloud",
                 mainPanel(
                   p("The following is a word cloud of the top 50 predicted words based on your partial sentence input.  The size and color of the words are relative to there probability of occurrence."),
                   plotOutput(outputId = "plot1")))
      )
    )
  )
)