

## load required  libraries

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


req_libraries <- c('quanteda','dplyr','data.table',"qdapDictionaries")



lapply(req_libraries, install_load); rm(req_libraries)


max_ngram <- 4

sample_percent <- 5
min_frequency_count <- 4


# GET DATA --------------------------------------------------------------------

# download and unzip data file that contains 3 language corpora
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
dest_file <- "Coursera-SwiftKey.zip"

if(!(file.exists(dest_file) & file.info(dest_file)$size==574661177)){
    download.file(url,dest_file)
    unzip(dest_file)
}

rm(url, dest_file)

# set paths to English-language corpora and working files
file_dir <- "final/en_US"
twit_path <- paste(file_dir, "en_US.twitter.txt", sep = "/")
news_path <- paste(file_dir, "en_US.news.txt", sep = "/")
blogs_path <- paste(file_dir, "en_US.blogs.txt", sep = "/")

path_list<- c(twit_path,news_path,blogs_path)


set.seed(1169)

# helper function to create  sample files
sample_corpus <- function(file_dir=NULL,path_list =NULL) {
    
    combined_path <- paste(file_dir, "combined.txt", sep = "/")
    replacement <- sprintf("\\.%dsample\\.txt", sample_percent)
    combined_sample_path <- gsub("\\.txt", replacement, combined_path)
    
    if (!file.exists(combined_sample_path)) {
        
        combined_sample <- c()
        for (i in path_list){
            replacement <- sprintf("\\.%dsample\\.txt", sample_percent)
            sample_path <- gsub("\\.txt", replacement, i)
            
            if (!file.exists(sample_path) & !is.null(i)) {
                if (!file.exists(i))
                    stop(sprintf("File not found: %s", i))
                
                con <- file(i, "r")
                read_text <- readLines(con, skipNul = T)
                close(con)
                
                num_lines <- length(read_text)
                
                # sample % of file
                ratio <- sample_percent / 100
                sample_pattern <- rbinom(num_lines, 1, ratio)
                sample_text <- read_text[sample_pattern == 1]
                
                
                
                # write sample file
                con <- file(sample_path, "w")
                writeLines(sample_text, con)
                close(con)
                
                
                combined_sample <- append(combined_sample, sample_text)
            }
        }
        
        con <- file(combined_sample_path, "w")
        writeLines(combined_sample, con)
        close(con)
    }
    
    
    
    return(combined_sample_path)
}


combined_sample_path <- sample_corpus(file_dir,path_list)





# helper function to loadf text files
load_a_file <- function(path = NULL) {
    if (!is.null(path)) {
        if (!file.exists(path))
            stop(sprintf("File not found: %s", path))
        
        con <- file(path)
        text <- readLines(con)
        close(con)
        
        return(text)
    }
}

# profanity file generation
profanity <- function(){
    
    if(!file.exists("profan1.txt")){
        download.file("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en", 
                      destfile="profan1.txt", sep="")
    }
    
    if(!file.exists("profan2.txt")){
        download.file("https://raw.githubusercontent.com/RobertJGabriel/Google-profanity-words/master/list.txt", 
                      destfile="profan2.txt", sep=",")
    }
    
    
    profan1<- readLines('profan1.txt', skipNul = T)
    profan2<- readLines('profan2.txt', skipNul = T)
    profanity_list<-tolower(unique(append(profan1,profan2)))
    
    
    
    return(profanity_list)
}

profanity_list <- profanity()



## loading english dictionary
data(GradyAugmented)

grady_lower <- tolower(GradyAugmented);rm(GradyAugmented)



# helper function to make tokenization file
make_word_tokens <- function(sample_path = NULL) {
    tokens_path <- gsub("sample\\.txt", "tokens\\.rds", sample_path)
    if (!file.exists(tokens_path) & !is.null(sample_path)) {
        
        # create corpora
        text <- load_a_file(sample_path)
        corpus_data <- corpus(text )
        
        
        # if sentence based prediction is desired
        corpus_data<- corpus_reshape(
            corpus_data,
            to ="sentences",
        )
        
        
        corpus_data<- tolower(corpus_data)
        num_sentences <- length(list(corpus_data)[[1]])
        
        # tokenize words
        word_tokens <- tokens(
            x=corpus_data,
            remove_numbers = TRUE,
            remove_punct = TRUE,
            remove_symbols=TRUE,
            remove_separators = TRUE,
            split_hyphens = TRUE,
            remove_url = TRUE
        )
        # remove profanity and select words present in dictionary
        word_tokens<- tokens_remove(word_tokens, profanity_list) #padding=TRUE
        word_tokens<- tokens_select(word_tokens, grady_lower) #padding = TRUE
    
        attr(word_tokens, "num_sentences") <- num_sentences
        saveRDS(word_tokens, tokens_path)
    }
    
    return(tokens_path)
}


tokens_path <- make_word_tokens(combined_sample_path)

tt

# helper function to make dfm  file
make_dfms <- function(tokens_path = NULL, include_dfms = TRUE) {
    dfms_path <- gsub("tokens", "dfms", tokens_path)
    if (!file.exists(dfms_path) & !is.null(tokens_path)) {
        if (!file.exists(tokens_path))
            stop(sprintf("File not found: %s", tokens_path))
        
        tokens_words <- readRDS(tokens_path)
        ngrams_list <- vector("list", max_ngram)

        
        for (i in 1:max_ngram) {
            ngrams_list[[i]] <- tokens_ngrams(tokens_words, n = i, concatenator = " ")
        }
        attr(ngrams_list[[1]], "num_sentences") <-
            attr(tokens_words, "num_sentences")
        ngrams_path <- gsub("tokens", "ngrams", tokens_path)
        saveRDS(ngrams_list, ngrams_path)
        
        # make DFM's
        cat('make DFM')
        if (include_dfms) {
            dfms <- lapply(ngrams_list, dfm)
            attr(dfms, "num_sentences") <- attr(ngrams_list[[1]], "num_sentences")
            saveRDS(dfms, dfms_path)
        }
    }
    return(dfms_path)
}

dfms_path <- make_dfms(tokens_path)






# helper function to make n-gram frequency table file  
make_simple_freq_tables <- function( dfms_path = NULL) {
    simple_freqs_path <- gsub("dfms", "simple.freqs", dfms_path)
    if (!file.exists(simple_freqs_path) & !is.null(dfms_path)) {
        if (!file.exists(dfms_path))
            stop(sprintf("File not found: %s", dfms_path))
        cat('load data\n')
        dfms <- readRDS(dfms_path)
        
        
        cat('create a simple frequency table\n')
        # create a simple frequency table
        term_freqs <- vector("list", max_ngram)
        for (i in 1:max_ngram){
            term_freqs[[i]] <- data.table(textstat_frequency(dfms[[i]]))%>% select(1,2)
            
        }

        # save simple frequency tables
        cat('save simple frequency tables\n')
        attr(term_freqs, "num_sentences") <- attr(dfms, "num_sentences")
        saveRDS(term_freqs, simple_freqs_path)
    }
    
    return(simple_freqs_path)
}




simple_freqs_path <- make_simple_freq_tables( dfms_path)



# helper function to make discount_table file, which will be needed for smoothing
make_discount_table<- function(simple_freqs_path=NULL){
    discount_table_path <- gsub("simple.freqs", "Freq.discount.factor", simple_freqs_path)
    if (!file.exists(discount_table_path) & !is.null(simple_freqs_path)) {
        if (!file.exists(simple_freqs_path))
            stop(sprintf("File not found: %s", simple_freqs_path))
        freq_table <- readRDS(simple_freqs_path)
        
        # create a frequency of  frequency table
        freq_data <- data.table(freq_table[[1]]$frequency)
        for (i in 2:max_ngram){
            freq_data <- rbind(freq_data, data.table(freq_table[[i]]$frequency))
        }
        
        f_o_f <- table(freq_data)
        freq_data<-data.table(cbind(frequency=as.integer(names(f_o_f)), freq_of_freq=as.integer(f_o_f)))
        # Y component calculation
        Y <- freq_data$freq_of_freq[1]/(freq_data$freq_of_freq[1]+2*freq_data$freq_of_freq[2])
        # create Freq.discount.factor table
        discount_table<- c()
        for(i in 1:3){
            discount_table[i]<- i - (i+1)*Y*(freq_data$freq_of_freq[i+1]/freq_data$freq_of_freq[i])
        }
        
        # save Freq.discount.factor table

        saveRDS(discount_table, discount_table_path)
    }
    
    return(discount_table_path)
}


discount_table_path <- make_discount_table(simple_freqs_path)


# helper function to modify n-gram frequency table file. these modifications are needed for smoothing
make_ngram_freq_table <- function(simple_freqs_path=NULL){
    unigram_freq_path <- gsub("simple.freqs", "unigram_freq", simple_freqs_path)
    bigram_freq_path <- gsub("simple.freqs", "bigram_freq", simple_freqs_path)
    trigram_freq_path <- gsub("simple.freqs", "trigram_freq", simple_freqs_path)
    quadgram_freq_path <- gsub("simple.freqs", "quadgram_freq", simple_freqs_path)
    if (!file.exists(unigram_freq_path) & !file.exists(bigram_freq_path) &
        !file.exists(trigram_freq_path) & !file.exists(quadgram_freq_path) &
        !is.null(simple_freqs_path)) {
        if (!file.exists(simple_freqs_path))
            stop(sprintf("File not found: %s", simple_freqs_path))
        freq_table <- readRDS(simple_freqs_path)
        
        # create a frequency of  frequency table
        
        unigram<- freq_table[[1]]
        setkey(unigram, feature)

        
        saveRDS(unigram, unigram_freq_path)
        
        bigram<- freq_table[[2]][,paste0('word',1:2):=tstrsplit(feature,' ')]
        setkey(bigram, feature)

        
        saveRDS(bigram, bigram_freq_path)

        trigram<- freq_table[[3]][,paste0('word',1:3):=tstrsplit(feature,' ')]%>% 
            mutate(word1_word2 = paste(word1, word2), word2_word3=paste(word2, word3))
        setkey(trigram, feature)
        

        saveRDS(trigram, trigram_freq_path)
        
        quadgram <- freq_table[[4]][,paste0('word',1:4):=tstrsplit(feature,' ')] %>%
            mutate(word1_word2_word3 = paste(word1, word2, word3),
                   word2_word3=paste(word2, word3),
                   word2_word3_word4=paste(word2, word3, word4))
        setkey(quadgram, feature)

        saveRDS(quadgram, quadgram_freq_path)

    }
    
    return(c(unigram_freq_path, bigram_freq_path, trigram_freq_path, quadgram_freq_path))
}


ngram_freq_path <- make_ngram_freq_table(simple_freqs_path)




# # helper function to calculate unigram pkn and bigram pcont  

unigram_pkn<- function(ngram_freq_path, discount_table_path){
    cat('load data\n')
    unigram <- readRDS(ngram_freq_path[1])
    bigram <- readRDS(ngram_freq_path[2])
    trigram <- readRDS(ngram_freq_path[3])
    disc <- readRDS(discount_table_path)
    
    
    cat('calculate n_0_word\n')
    bi_word2<- bigram %>% count(word2)
    names(bi_word2) <- c('feature', 'count')
    unigram[bi_word2, on='feature', n_0_word:= i.count]
    unigram[!is.finite(unigram$n_0_word),"n_0_word"] <- 0
    
    
    cat('calculate pkn\n')
    total_bigram <- nrow(bigram)
    
    unigram$pkn <- unigram$n_0_word/total_bigram
    
    cat('calculate lambda_numerator \n')
    
    n1_table<- bigram %>% filter(frequency==1) %>% count(word1)
    names(n1_table) <- c('feature', 'count')
    unigram[n1_table, on='feature', n1:= i.count]
    unigram[!is.finite(unigram$n1),"n1"] <- 0
    
    n2_table<- bigram %>% filter(frequency==2) %>% count(word1)
    names(n2_table) <- c('feature', 'count')
    unigram[n2_table, on='feature', n2:= i.count]
    unigram[!is.finite(unigram$n2),"n2"] <- 0
    
    n3_table<- bigram %>% filter(frequency>=3) %>% count(word1)
    names(n3_table) <- c('feature', 'count')
    unigram[n3_table, on='feature', n3:= i.count]
    unigram[!is.finite(unigram$n3),"n3"] <- 0
    
    unigram$lambda_nu <- (disc[1]*unigram$n1 + disc[2]*unigram$n2 + disc[3]*unigram$n3)
    
    
    cat('calculate lambda \n')
    tri_word2<- trigram %>% count(word2)
    names(tri_word2) <- c('feature', 'count')
    unigram[tri_word2, on='feature', n_0_word_0:= i.count]
    unigram[!is.finite(unigram$n_0_word_0),"n_0_word_0"] <- 0
    
    
    unigram$lambda_bi <- unigram$lambda_nu/unigram$n_0_word_0
    unigram[!is.finite(unigram$lambda),"lambda"] <- 0
    
    cat('save RDS \n')
    saveRDS(unigram, ngram_freq_path[1])
    
    
    
}


unigram_pkn(ngram_freq_path, discount_table_path)



# # helper function to calculate bigram pkn and trigram pcont
bigram_pkn<- function(ngram_freq_path, discount_table_path){
    
    cat('load data\n')
    unigram <- readRDS(ngram_freq_path[1]) %>% select(feature, pkn,lambda_bi, n_0_word_0)
    bigram <- readRDS(ngram_freq_path[2])
    trigram <- readRDS(ngram_freq_path[3])
    quadgram <- readRDS(ngram_freq_path[4])
    disc <- readRDS(discount_table_path)
    
    
    cat('calculate n_0_word1_word2\n')
    
    tri_word2_word3<- trigram %>% count(word2_word3)
    names(tri_word2_word3) <- c('feature', 'count')
    bigram[tri_word2_word3, on='feature', n_0_word1_word2:= i.count]
    bigram[!is.finite(bigram$n_0_word1_word2),"n_0_word1_word2"] <- 0
    #total_trigram <- nrow(trigram)
    
    
    cat('import data from unigram\n')
    
    colnames(unigram)[colnames(unigram) == 'feature'] <- 'word1'
    
    bigram[unigram, on='word1', `:=`(pcont=i.pkn, lambda_bi=i.lambda_bi, n_0_word_0=i.n_0_word_0 )]
    
    
    cat('calculate discount\n')
    bigram[frequency == 1, discount := disc[1]]
    bigram[frequency == 2, discount := disc[2]]
    bigram[frequency >= 3, discount := disc[3]]
    
    
    cat('calculate bigram pkn\n')
    bigram$pkn <- (pmax((bigram$n_0_word1_word2 - bigram$discount), 0) / bigram$n_0_word_0) + (bigram$lambda_bi * bigram$pcont)
    
    cat('calculate lambda_nu\n')
    n1_table<- trigram %>% filter(frequency==1) %>% count(word1_word2)
    names(n1_table) <- c('feature', 'count')
    bigram[n1_table, on='feature', n1:= i.count]
    bigram[!is.finite(bigram$n1),"n1"] <- 0
    
    n2_table<- trigram %>% filter(frequency==2) %>% count(word1_word2)
    names(n2_table) <- c('feature', 'count')
    bigram[n2_table, on='feature', n2:= i.count]
    bigram[!is.finite(bigram$n2),"n2"] <- 0
    
    n3_table<- trigram %>% filter(frequency>=3) %>% count(word1_word2)
    names(n3_table) <- c('feature', 'count')
    bigram[n3_table, on='feature', n3:= i.count]
    bigram[!is.finite(bigram$n3),"n3"] <- 0
    
    bigram$lambda_nu <- (disc[1]*bigram$n1 + disc[2]*bigram$n2 + disc[3]*bigram$n3)
    
    
    cat('calculate lambda \n')
    
    quad_word1_word2<- quadgram %>% count(word2_word3)
    names(quad_word1_word2) <- c('feature', 'count')
    bigram[quad_word1_word2, on='feature', n_0_word1_word2_0:= i.count]
    bigram[!is.finite(bigram$n_0_word1_word2_0),"n_0_word1_word2_0"] <- 0
    
    bigram$lambda_tri <- bigram$lambda_nu/bigram$n_0_word1_word2_0
    bigram[!is.finite(bigram$lambda_tri),"lambda_tri"] <- 0
    
    
    cat('save RDS \n')
    saveRDS(bigram, ngram_freq_path[2])
    
}
bigram_pkn(ngram_freq_path, discount_table_path)


# # helper function to calculate trigram pkn and quadgram pcont
trigram_pkn<- function(ngram_freq_path, discount_table_path){
    
    cat('load data\n')
    bigram <- readRDS(ngram_freq_path[2]) %>% select(feature, pkn,lambda_tri, n_0_word1_word2_0)
    trigram <- readRDS(ngram_freq_path[3])
    quadgram <- readRDS(ngram_freq_path[4])
    disc <- readRDS(discount_table_path)
    
    
    cat('calculate n_0_word1_word2_word3\n')
    
    quad_word2_word3_word4<- quadgram %>% count(word2_word3_word4)
    names(quad_word2_word3_word4) <- c('feature', 'count')
    trigram[quad_word2_word3_word4, on='feature', n_0_word1_word2_word3:= i.count]
    rm(quad_word2_word3_word4)
    trigram[!is.finite(trigram$n_0_word1_word2_word3),"n_0_word1_word2_word3"] <- 0
    
    
    
    cat('import data from bigram\n')
    
    colnames(bigram)[colnames(bigram) == 'feature'] <- 'word1_word2'
    
    trigram[bigram, on='word1_word2', `:=`(pcont=i.pkn, lambda_tri=i.lambda_tri, n_0_word1_word2_0=i.n_0_word1_word2_0 )]
    
    
    
    cat('calculate discount\n')
    trigram[frequency == 1, discount := disc[1]]
    trigram[frequency == 2, discount := disc[2]]
    trigram[frequency >= 3, discount := disc[3]]
    
    
    
    
    cat('calculate trigram pkn\n')
    trigram$pkn <- (pmax((trigram$n_0_word1_word2_word3 - trigram$discount), 0) / trigram$n_0_word1_word2_0) + (trigram$lambda_tri * trigram$pcont)
    
    
    
    cat('calculate lambda_nu\n')
    n1_table<- quadgram %>% filter(frequency==1) %>% count(word1_word2_word3)
    names(n1_table) <- c('feature', 'count')
    trigram[n1_table, on='feature', n1:= i.count]
    trigram[!is.finite(trigram$n1),"n1"] <- 0
    
    n2_table<- quadgram %>% filter(frequency==2) %>% count(word1_word2_word3)
    names(n2_table) <- c('feature', 'count')
    trigram[n2_table, on='feature', n2:= i.count]
    trigram[!is.finite(trigram$n2),"n2"] <- 0
    
    n3_table<- quadgram %>% filter(frequency>=3) %>% count(word1_word2_word3)
    names(n3_table) <- c('feature', 'count')
    trigram[n3_table, on='feature', n3:= i.count]
    trigram[!is.finite(trigram$n3),"n3"] <- 0
    
    trigram$lambda_nu <- (disc[1]*trigram$n1 + disc[2]*trigram$n2 + disc[3]*trigram$n3)
    
    cat('calculate lambda \n')
    
    quad_word1_word2_word3<- quadgram %>% count(word1_word2_word3)
    names(quad_word1_word2_word3) <- c('feature', 'count')
    trigram[quad_word1_word2_word3, on='feature', n_word1_word2_word3_0:= i.count]
    trigram[!is.finite(trigram$n_word1_word2_word3_0),"n_word1_word2_word3_0"] <- 0
    
    trigram$lambda_quad <- trigram$lambda_nu/trigram$n_word1_word2_word3_0
    trigram[!is.finite(trigram$lambda_quad),"lambda_quad"] <- 0
    
    
    
    cat('save RDS \n')
    
    saveRDS(trigram, ngram_freq_path[3])
    
}

trigram_pkn(ngram_freq_path, discount_table_path)








# # helper function to calculate quadgram pkn
quadgram_pkn<- function(ngram_freq_path, discount_table_path){
    
    cat('load data\n')
    trigram <- readRDS(ngram_freq_path[3]) %>% select(feature, pkn,lambda_quad, n_word1_word2_word3_0)
    quadgram <- readRDS(ngram_freq_path[4])
    disc <- readRDS(discount_table_path)
    
    
    cat('import data from trigram\n')
    
    colnames(trigram)[colnames(trigram) == 'feature'] <- 'word1_word2_word3'
    
    quadgram[trigram, on='word1_word2_word3', `:=`(pcont=i.pkn, lambda_quad=i.lambda_quad, n_word1_word2_word3_0=i.n_word1_word2_word3_0 )]
    
    
    
    cat('calculate discount\n')
    quadgram[frequency == 1, discount := disc[1]]
    quadgram[frequency == 2, discount := disc[2]]
    quadgram[frequency >= 3, discount := disc[3]]
    
    
    
    cat('calculate quadgram pkn\n')
    quadgram$pkn <- (pmax((quadgram$frequency - quadgram$discount), 0) / quadgram$n_word1_word2_word3_0) + (quadgram$lambda_quad * quadgram$pcont)
    
    
    cat('save RDS \n')
    saveRDS(quadgram, ngram_freq_path[4])
    
}

quadgram_pkn(ngram_freq_path, discount_table_path)







# # helper function to create final n-gram files that will be used for shiny app
final_data_preperation<- function(file_link){
    final_output_link<- c()
    for (i in 1:length(file_link)){
        final_output_link[i] <- gsub("freq", "final", file_link[i])
    }
    
    unigram_final <- readRDS(file_link[1]) %>% select(prediction=feature,pkn )
    setkey(unigram_final, prediction)
    setorder(unigram_final, -pkn)
    saveRDS(unigram_final, final_output_link[1])
    
    bigram_final <- readRDS(file_link[2]) %>% select(term=feature, input= word1, prediction= word2 ,pkn )
    setkey(bigram_final, input)
    setorder(bigram_final, input, -pkn)
    saveRDS(bigram_final, final_output_link[2])
    
    trigram_final <- readRDS(file_link[3]) %>% select(term=feature,input= word1_word2 , prediction= word3,pkn )
    setkey(trigram_final, input)
    setorder(trigram_final, input, -pkn)
    saveRDS(trigram_final, final_output_link[3])
    
    quadgram_final <- readRDS(file_link[4]) %>% select(term=feature,input= word1_word2_word3, prediction= word4 ,pkn )
    setkey(quadgram_final, input)
    setorder(quadgram_final, input, -pkn)
    saveRDS(quadgram_final, final_output_link[4])
    
    return(final_output_link)
}


final_1percent_ngram_pkn <- final_data_preperation(ngram_freq_path)


