
# Next Word Prediction Widget

### author: "Abid Al Reza"

### date: "11/13/2020"




## Overview

This presentation was generated as a requirement for completion of the coursera course titled "Data science capstone."

The application (Auto Loan Calculator) was developed using Shiny and can be observed [here](https://abidgen.shinyapps.io/next_word_prediction/).

Source codes for the presentation (Rmd file) and application (ui.R and server.R) are hosted at [Github](https://github.com/abidgen/Coursera_data_science_capstone_jhu).



## Prediction Algorithm 

This app predicts next word of a continuous sentence based on previous words.

This suggestion model is based on Katz's back-off model where it uses the last three words to predict the next word. If no match is found in the n-gram data, it will use last two words and then last one word until it finds a suitable match. If no match is found, it will suggest most frequent words.


## Data preperation
The n-gram data was created using 'twitter', 'news', and ' blog' data provided by coursera-Data science captone course page. Five percent sample data from all data were used to build corpora and n-gram frequency tables. 

Here, we generated unigrams to quadgrams, then used 'Kneser-Ney smoothing' technique to determine the probability of each word in the sample corpora in respect to their preceding words in the sample data. 

The code for data cleaning, data preparation, ,n-gram frequency table generation, smoothing and probability calculation are hosted at [Github](https://github.com/abidgen/Coursera_data_science_capstone_jhu). 



## Instructions and App description

- After initializing the app, it will take some to load the n-gram probability data. When the 'Suggestion' box and the suggestion table has been populated, that will indicate that the app is ready.
- User needs enter some text in the 'Enter text for prediction' box and the app will automatically suggest top 5 probable next word based on the previous words provided.
- A table of all the probable words with their probability scores will be generated at the bottom section.
- 'Reset' button will clear all text already provided and start a new session.

## Link to the app

https://abidgen.shinyapps.io/next_word_prediction_Abid/
