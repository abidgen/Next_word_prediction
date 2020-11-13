
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(shinyWidgets)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(navbarPage( theme = shinytheme('superhero'),
                    title = "Next word prediction widget",
                    tabPanel("App",
                             mainPanel(
                                 
                                 
                                 
                                 textInput(inputId = 'text', label = h4("Enter text for prediction"),
                                           value = 'How are you',
                                           width = '100%'),
                                 actionButton('reset', label ="Reset" ),
                                 h5('Note: Wait till the app is done loading 
                                    required datasets. Text in the below 
                                    suggestion box will indicate that the app is ready and functional.'),
                                 
                                 br(),
                                 
                                 h4('Suggestions:'),
                                 verbatimTextOutput("next_word"),
                                 br(),
                                 hr(),
                                 br(),
                                 dataTableOutput("output_dataset")
                                 
                                 
                                 
                             )
                    ),
                    tabPanel("Documentation",
                             mainPanel(
                                 h3("Next word prediction widget"),
                                 h4("This app predicts next word of a continuous 
                                    sentence based on previous words."),
                                 h4("This suggestion model uses Katz's 
                                    back-off model where it uses the last three 
                                    words to predict the next word. If no word 
                                    match is found in the n-gram dataset, 
                                    it will use last two words and then last 
                                    one word until it finds a suitable match. 
                                    If no match is found, it will suggest most 
                                    frequent words."),
                                 
                                 h4("The n-gram dataset was created using 'twitter',
                                    'news', and ' blog' dataset provided by 
                                    coursera-Data science captone course page. 
                                    Five percent data from all datasets were used
                                    to build corpora and n-gram frequency tables.
                                    Here, we generated unigrams to quadgrams,
                                    then used 'Kneser-Ney smoothing' techniques 
                                    to determine the probability of each word in 
                                    the sample corpora in respect to their preceding words."),
                                 
                                 
                                 br(),
                                 hr(),
                                 br(),
                                 
                                 h3("Intructions"),
                                 
                                 tags$li(tags$span("After initializing the app, it will take 
                                    some to load the n-gram probability data. When the 
                                    'Suggestion' box and the suggestion table has 
                                    been populated, that will indicate that the 
                                    app is ready.")),
                                 tags$li(tags$span("User needs enter some text in the 'Enter text
                                    for prediction' box and the app will automatically suggest 
                                    top 5 probable next word based on the 
                                    previous words provided. ")),
                                 
                                 tags$li(tags$span("A table of all the probable words 
                                    with their probability scores will be 
                                    generated at the bottom section.")),
                                 
                                 tags$li(tags$span("'Reset' button will clear all text already 
                                    provided and start a new session."))

                             )
                    )
                    
))
