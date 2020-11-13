



unigram_final<- readRDS('./combined.5unigram_final.rds')
bigram_final<- readRDS('./combined.5bigram_final.rds')
trigram_final<- readRDS('./combined.5trigram_final.rds')
quadgram_final<- readRDS('./combined.5quadgram_final.rds')



next_word_table_prediction<- function(input){
    
    
    tokenize_input<- function(input){
        
        #sentences <- tokens(input, what='sentence')[[1]]
        
        word_tokens <- tokens(
            x= input,#sentences[length(sentences)],
            remove_numbers = TRUE,
            remove_punct = TRUE,
            remove_symbols=TRUE,
            remove_separators = TRUE,
            split_hyphens = TRUE,
            remove_url = TRUE
        )
        #word_tokens<- tokens_remove(word_tokens, profanity_list)
        #word_tokens<- tokens_select(word_tokens, grady_lower, padding = TRUE)
        
        
        return(tolower(word_tokens[[1]]))
    }
    
    
    words<-tokenize_input(input)
    
    
    if (length(words)>=3){
        word1_word2_word3 <- paste(tail(words,3), collapse = ' ')
        word1_word2 <- paste(tail(words,2), collapse = ' ')
        word1 <- tail(words,1)
        if (is.finite(quadgram_final[word1_word2_word3]$pkn[1])){
            word_table<- head(quadgram_final[word1_word2_word3], 100)
            
            #next_word<- word_table$prediction
        }else if((is.finite(trigram_final[word1_word2]$pkn[1]))){
            word_table<- head(trigram_final[word1_word2], 100)
            
            #next_word<- word_table$prediction
        }else if((is.finite(bigram_final[word1]$pkn[1]))){
            word_table<- head(bigram_final[word1], 100)
            #next_word<- word_table$prediction
        }else{
            word_table<- head(unigram_final, 100)
            
            #next_word<- word_table$prediction
        }

    } else if (length(words)==2){
        word1_word2 <- paste(tail(words,2), collapse = ' ')
        word1 <- tail(words,1)
        if((is.finite(trigram_final[word1_word2]$pkn[1]))){
            word_table<- head(trigram_final[word1_word2], 100)
            
            next_word<- word_table$prediction
        }else if((is.finite(bigram_final[word1]$pkn[1]))){
            word_table<- head(bigram_final[word1], 100)
            
            #next_word<- word_table$prediction
        }else{
            word_table<- head(unigram_final, 100)
            
            #next_word<- word_table$prediction
        }
        
        
    } else if(length(words)==1){
        word1 <- tail(words,1)
        if((is.finite(bigram_final[word1]$pkn[1]))){
            word_table<- head(bigram_final[word1], 100)
            
            #next_word<- word_table$prediction
        }else{
            word_table<- head(unigram_final, 100)
            
            #next_word<- word_table$prediction
        }

    }
    
    word_table<- word_table %>% select('input', 'prediction', 'pkn')
    colnames(word_table) <- c( 'prediction_base', 'prediction', 'probability')

    
    
    return(word_table)
 
    
}



next_word_prediction<- function(input){
    word_table <- next_word_table_prediction(input)
    next_word<- head(word_table$prediction,5)
    prediction <- paste( next_word, collapse=', ' )
    return(prediction)
}





