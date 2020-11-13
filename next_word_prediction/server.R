
install_load <- function (package1, ...)  {   
    packages <- c(package1, ...)
    for(package in packages){
        if(package %in% rownames(installed.packages()))
            do.call('library', list(package))
        else {
            install.packages(package)
            do.call("library", list(package))
        }
    } 
}


req_libraries <- c('quanteda','dplyr','data.table','shiny')



lapply(req_libraries, install_load); rm(req_libraries)


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

