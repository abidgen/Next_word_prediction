

library(quanteda)
library(dplyr)
library(data.table)
library(shiny)


source('prediction_function.R')




shinyServer(function(input, output, session) {
    
    #session$onSessionEnded(stopApp)
    
    word_table <- reactive({
        validate(
            need(input$text, 'Please enter some text.')
        )
        word_table<- next_word_table_prediction(input$text);
    })
    
    next_word<- reactive({
        validate(
            need(input$text, 'Please enter some text.')
        )
        next_table<- next_word_prediction(input$text)
        
    })
    
    output$next_word <- renderPrint({
        ds <- next_word()
        cat( 
            paste( ds, collapse=', ' )
        )
    })
    
    
    output$output_dataset <- renderDataTable({
        word_table()
    })
    
    
    observeEvent(
        input$reset,{
            updateTextInput(session,'text', value = '' )
        }
    )
    
})

