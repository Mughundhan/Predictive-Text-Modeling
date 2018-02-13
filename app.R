# Load libraries
library(shiny)
library(ggplot2)
library(scales)
library(tidyr) # assists in cleaning & preparing data
library(dplyr) # assists in data manipulation, transformation, & summarization
library(stringr)
library(wordcloud)

ui <- fluidPage(
shinyUI(
  pageWithSidebar(
    headerPanel("Predictive Text Modeling"),
    # Select inputs
    sidebarPanel(
      textInput("userInput", "Type the word(s) in the given space:", value = "Good"),
      
      submitButton("Predict"),
      
      br()                
    ),
    
    
    mainPanel(
      # use tab format to display outputs
      tabsetPanel(
        
        # tab 1        
        tabPanel("App Info",
                 withMathJax(),
                 includeMarkdown("include.Rmd")),
        
        # tab 2        
        tabPanel("Word Prediction",
                 mainPanel(
                   p("The following are the top 6 predicted words based on your partial sentence input.  This list is rank ordered from top to bottom representing highest to lowest probability.  The word in ", span(strong("blue"), style = "color:blue"), "represents the highest probable word to follow your input."),
                   h1(textOutput("top1"), align = "center", style = "color:blue"),
                   h2(textOutput("top2"), align = "center"),
                   h3(textOutput("top3"), align = "center"),
                   h4(textOutput("top4"), align = "center"),
                   h5(textOutput("top5"), align = "center"),
                   h6(textOutput("top6"), align = "center"))),
        
        #tab 3
        tabPanel("Word Cloud",
                 mainPanel(
                   p("The following is a word cloud of the top 50 predicted words based on your partial sentence input.  The size and color of the words are relative to there probability of occurrence."),
                   plotOutput(outputId = "plot1")))
      )
    )
  )
)
)


server <- function(input, output) {
  
final.df <- readRDS("final.rds")
top.unigram <- readRDS("top.unigram.rds")

shinyServer(function(input, output) {
  textDisplay <- reactive ({
    
    # user input
    user.input <- input$userInput
    user.input <- tolower(user.input)
    user.input <- as.vector(strsplit(user.input," ")[[1]])
    user.input <- str_replace_all(user.input, "[^[:alpha:]]", "")
    user.input <- grep('.', user.input, value=TRUE)
    user.input <- paste(user.input, sep=" ", collapse=" ")
    
    # get length of user input
    i <- length(strsplit(user.input," ")[[1]])
    
    if (i > 4){
      sub.input <- seq(i-3, i)
      user.input <- as.vector(strsplit(user.input," ")[[1]])[sub.input]
      i <- length(user.input)
    }
    
    # input for search
    full.input <- paste(user.input, sep=" ", collapse= " ")
    
    n <- i+1
    
    # subset based on length of user input
    source.df <- final.df %>%
      filter(count == n) %>%
      filter(key == full.input)
    
    # if search yields no results look for n-1 gram
    if(nrow(source.df) == 0 & i > 1){
      l = n
      l = l-1
      full.input2 <- strsplit(full.input, " ")[[1]][2:i]
      full.input2 <- paste(full.input2, sep=" ", collapse=" ")
      source.df <- final.df %>%
        filter(count == l) %>%
        filter(key == full.input2)
    }
    
    if(nrow(source.df) == 0 & i > 2){
      l = n
      l = l-2
      full.input3 <- strsplit(full.input, " ")[[1]][3:i]
      full.input3 <- paste(full.input3, sep=" ", collapse=" ")
      source.df <- final.df %>%
        filter(count == l) %>%
        filter(key == full.input3)
    }
    
    if(nrow(source.df) == 0 & i > 3){
      l = n
      l = l-3
      full.input4 <- strsplit(full.input, " ")[[1]][4:i]
      full.input4 <- paste(full.input4, sep=" ", collapse=" ")
      source.df <- final.df %>%
        filter(count == l) %>%
        filter(key == full.input4)
    }
    
    
    ###### MISSPELLINGS or APPROXIMATES########
    # find approximate of initial input
    if(nrow(source.df) == 0 & i > 3){
      
      sub.df <- final.df %>%
        filter(count == 5)
      full.input5 <- strsplit(full.input, " ")[[1]][c(1,3:i)]
      start <- paste("^",full.input5[1], sep="", collapse="")
      end <- paste(full.input5[3],"\\b", sep="", collapse="")
      middle <- paste("\\w+", full.input5[2], sep=" ", collapse="")
      term1 <- paste(start, middle, end, sep=" ", collapse="")
      
      full.input5 <- strsplit(full.input, " ")[[1]][c(1:2,i)]
      start <- paste("^",full.input5[1], sep="", collapse="")
      end <- paste(full.input5[3],"\\b", sep="", collapse="")
      middle <- paste(full.input5[2], "\\w+", sep=" ", collapse="")
      term2 <- paste(start, middle, end, sep=" ", collapse="")
      
      term <- paste(term1, "|", term2, sep="", collapse=" ")
      
      source.df <- sub.df[grep(term, sub.df$key),]
    }
    
    if(nrow(source.df) == 0 & i > 2){
      
      sub.df <- final.df %>%
        filter(count == 4)
      full.input5 <- strsplit(full.input, " ")[[1]][c(1,i)]
      start <- paste("^",full.input5[1], sep="", collapse="")
      end <- paste(full.input5[2],"\\b", sep="", collapse="")
      end <- paste("\\w+", end, sep=" ", collapse="")
      term <- paste(start, end, sep=" ", collapse="")
      
      source.df <- sub.df[grep(term, sub.df$key),]
    }
    
    source.df <- ungroup(source.df)
    source.df <- rbind(source.df[,2:3], top.unigram[,1:2])
    source.df$predict[1:10]
  })
  
  output$top1 <- renderText({
    textDisplay()[1]
  })
  
  output$top2 <- renderText({
    textDisplay()[2]
  })
  
  output$top3 <- renderText({
    textDisplay()[3]
  })
  
  output$top4 <- renderText({
    textDisplay()[4]
  })
  
  output$top5 <- renderText({
    textDisplay()[5]
  })
  
  output$top6 <- renderText({
    textDisplay()[6]
  })
  
  output$plot1 <- renderPlot({
    # user input
    user.input <- input$userInput
    user.input <- tolower(user.input)
    user.input <- as.vector(strsplit(user.input," ")[[1]])
    user.input <- str_replace_all(user.input, "[^[:alpha:]]", "")
    user.input <- grep('.', user.input, value=TRUE)
    user.input <- paste(user.input, sep=" ", collapse=" ")
    
    # get length of user input
    i <- length(strsplit(user.input," ")[[1]])
    
    if (i > 4){
      sub.input <- seq(i-3, i)
      user.input <- as.vector(strsplit(user.input," ")[[1]])[sub.input]
      i <- length(user.input)
    }
    
    # input for search
    full.input <- paste(user.input, sep=" ", collapse= " ")
    
    n <- i+1
    
    # subset based on length of user input
    source.df <- final.df %>%
      filter(count == n) %>%
      filter(key == full.input)
    
    # if search yields no results look for n-1 gram
    if(nrow(source.df) == 0 & i > 1){
      l = n
      l = l-1
      full.input2 <- strsplit(full.input, " ")[[1]][2:i]
      full.input2 <- paste(full.input2, sep=" ", collapse=" ")
      source.df <- final.df %>%
        filter(count == l) %>%
        filter(key == full.input2)
    }
    
    if(nrow(source.df) == 0 & i > 2){
      l = n
      l = l-2
      full.input3 <- strsplit(full.input, " ")[[1]][3:i]
      full.input3 <- paste(full.input3, sep=" ", collapse=" ")
      source.df <- final.df %>%
        filter(count == l) %>%
        filter(key == full.input3)
    }
    
    if(nrow(source.df) == 0 & i > 3){
      l = n
      l = l-3
      full.input4 <- strsplit(full.input, " ")[[1]][4:i]
      full.input4 <- paste(full.input4, sep=" ", collapse=" ")
      source.df <- final.df %>%
        filter(count == l) %>%
        filter(key == full.input4)
    }
    
    
    ###### MISSPELLINGS or APPROXIMATES########
    # find approximate of initial input
    if(nrow(source.df) == 0 & i > 3){
      
      sub.df <- final.df %>%
        filter(count == 5)
      full.input5 <- strsplit(full.input, " ")[[1]][c(1,3:i)]
      start <- paste("^",full.input5[1], sep="", collapse="")
      end <- paste(full.input5[3],"\\b", sep="", collapse="")
      middle <- paste("\\w+", full.input5[2], sep=" ", collapse="")
      term1 <- paste(start, middle, end, sep=" ", collapse="")
      
      full.input5 <- strsplit(full.input, " ")[[1]][c(1:2,i)]
      start <- paste("^",full.input5[1], sep="", collapse="")
      end <- paste(full.input5[3],"\\b", sep="", collapse="")
      middle <- paste(full.input5[2], "\\w+", sep=" ", collapse="")
      term2 <- paste(start, middle, end, sep=" ", collapse="")
      
      term <- paste(term1, "|", term2, sep="", collapse=" ")
      
      source.df <- sub.df[grep(term, sub.df$key),]
    }
    
    if(nrow(source.df) == 0 & i > 2){
      
      sub.df <- final.df %>%
        filter(count == 4)
      full.input5 <- strsplit(full.input, " ")[[1]][c(1,i)]
      start <- paste("^",full.input5[1], sep="", collapse="")
      end <- paste(full.input5[2],"\\b", sep="", collapse="")
      end <- paste("\\w+", end, sep=" ", collapse="")
      term <- paste(start, end, sep=" ", collapse="")
      
      source.df <- sub.df[grep(term, sub.df$key),]
    }
    
    top.unigram <- top.unigram %>% 
      mutate(probability = .2*probability) %>%
      select(predict, probability)
    
    source.df <- ungroup(source.df) %>% 
      mutate(probability = total/sum(total)) %>%
      select(predict, probability)
    
    source.df <- rbind(source.df, top.unigram)
    
    words <- source.df$predict
    freq <- source.df$probability
    
    wordcloud(words, freq, scale = c(7,2), colors=brewer.pal(8, "Dark2"),
              fixed.asp = FALSE, rot.per = 0, max.words = 50)
  })
  
})
}