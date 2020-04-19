############################################################################
# Coursera Data Science Capstone Project
# Author   : Myriam Ragni
# Date     : April 2020
# Filename : PredictWord.R
# Part 5   : - This script is used by the Predictive Text Product (Shiny)
#            - Cleanup of input text
#            - Search for the next word using the Katz-Backoff technique
############################################################################
# ------------------------ BEGIN FUNCTIONS ----------------------------
# CleanInput      : Takes a text as input and performs advanced cleanup
# predict_results : Takes a text as input and calls search_quadram/
#                   search_trigram/search_bigram depending on the 
#                   number of words in the input text
# search_quadgram : Called for input text >=3 - returns as dataframe
#                   top 4-grams with the highest probability; if there
#                   is no match, fallbacks to search-trigram
# search_trigram  : Called for input text =2 - returns as dataframe
#                   top 3-grams with the highest probability; if there
#                   is no match, fallbacks to search-bigram
# search_bigram   : Called for input text =1 - returns as dataframe
#                   top 2-grams with the highest probability; if there
#                   is no match, fallbacks to search-unigram
# search_unigram  : Fallback if next word cannot be found in 
#                   4/3/2 Grams dictionaries
# ---------------------------------------------------------------------

CleanInput <- function(inputtext) {
        inputtext <- tolower(inputtext)
        inputtext <- replace_word_elongation(inputtext)
        inputtext <- replace_contraction(inputtext) ### does not replace here's /hadn't/ haven't
        inputtext <- replace_email(inputtext, replacement=" ") 
        inputtext <- gsub(" [0-9]+g "," ", inputtext) 
        inputtext <- gsub(" [0-9]+kg "," ", inputtext) 
        inputtext <- gsub(" [0-9]+mg "," ", inputtext)
        inputtext <- gsub(" [0-9]+lbs "," ", inputtext)
        inputtext <- gsub(" [0-9]+$ "," ", inputtext) 
        inputtext <- gsub(" [0-9]+th "," ", inputtext)
        inputtext <- gsub(" [0-9]+nd "," ", inputtext)
        inputtext <- gsub(" [0-9]+rd "," ", inputtext)
        inputtext <- gsub(" [0-9]+st "," ", inputtext)
        inputtext <- gsub(" [0-9]+k "," ",inputtext)
        inputtext <- gsub(" [0-9]+am "," ",inputtext)
        inputtext <- gsub(" [0-9]+pm "," ",inputtext)
        inputtext <- gsub(" [0-9]+a\\.m "," ",inputtext)
        inputtext <- gsub(" [0-9]+p\\.m "," ",inputtext)
        inputtext <- gsub(" [a-zA-Z]+$ "," ",inputtext)
        inputtext <- gsub(" +h | [0-9]+h"," ",inputtext)
        inputtext <- gsub(" +m | [0-9]+m"," ",inputtext)
        inputtext <- gsub(" +s | [0-9]+s"," ",inputtext)
        inputtext <- gsub("&", " and ", inputtext)
        inputtext <- gsub("@", " at ", inputtext)
        inputtext <- gsub("[[:punct:][:blank:]]+", " ",inputtext)
        inputtext <- gsub("here's", "here is", inputtext)
        inputtext <- gsub("hadn't", "had not", inputtext)
        inputtext <- gsub("haven't", "have not", inputtext)
        inputtext <- gsub("'s", "", inputtext)
        inputtext <- gsub("yrs|yr", "years", inputtext)
        inputtext <- gsub("pls|plz", "please", inputtext)
        inputtext <- gsub(" u ", " you ", inputtext)
        inputtext <- gsub("[^\x01-\x7F]","",inputtext) # emoji's, emoticons
        inputtext <- gsub("(http|https|www)[^[:space:]]+"," ",inputtext) # URLs
        inputtext <- gsub("\\(\\w+\\)"," ",inputtext) # parenthesis and their content
        inputtext <- gsub("\\(\\w+\\)"," ",inputtext) # parenthesis and their content
        inputtext <- gsub(" [b-hj-z] ", " ", inputtext)
        inputtext <- gsub("#[[:alnum:]_]*)","", inputtext) #tag
        inputtext <- gsub("\\s+"," ", inputtext)
        return(inputtext)
}


search_quadgram <- function(input_words){
        nr_words <- length(input_words)
        predictions <- filter(quad_words_freq,
                              word_1==input_words[nr_words-2] & 
                                      word_2==input_words[nr_words-1] & 
                                      word_3==input_words[nr_words]) %>%
                select("word_1","word_2","word_3","word_4","count","prob") %>%
                arrange(desc(prob)) %>%
                top_n(10,prob) %>%
                rename(WORD_1=word_1,WORD_2=word_2,WORD_3=word_3,WORD_4=word_4,NR_WORD_STRING_IN_DICT=count,PROB_OF_OCCURRENCE=prob)
        if(is.data.frame(predictions) && nrow(predictions)==0){
                search_unigram(input_words)
        } else return(predictions)
}

search_trigram <- function(input_words){
        nr_words <- length(input_words)
        predictions <- filter(tri_words_freq,
                              word_1==input_words[nr_words-1] & 
                                      word_2==input_words[nr_words]) %>%
                select("word_1","word_2","word_3","count", "prob") %>%
                
                arrange(desc(prob)) %>%
                top_n(10,prob) %>%
                rename(WORD_1=word_1,WORD_2=word_2,WORD_3=word_3,NR_WORD_STRING_IN_DICT=count,PROB_OF_OCCURRENCE=prob)
        if(is.data.frame(predictions) && nrow(predictions)==0){
                search_unigram(input_words)
        } else return(predictions)
}

search_bigram <- function(input_words){
        nr_words <- length(input_words)
        predictions <- filter(bi_words_freq,
                              word_1==input_words[nr_words]) %>%
                select("word_1","word_2","count", "prob") %>%
                arrange(desc(prob)) %>%
                top_n(10,prob) %>% 
                rename(WORD_1=word_1,WORD_2=word_2,NR_WORD_STRING_IN_DICT=count,PROB_OF_OCCURRENCE=prob)
        if(is.data.frame(predictions) && nrow(predictions)==0){
                search_unigram(input_words)
        } else return(predictions)
}

search_unigram <- function(input_words){
        gram <<- c("No matching with 4,3,2-grams, therefore showing random 1-grams from the top 100")
        predictions <- sample_n(uni_words_freq100, size = 10)
        names(predictions) <- c("WORD_1","NR_WORD1_IN_DICT","PROB_OF_OCCURENCE")
        return(predictions)
}


predict_results <- function(input){
        nr_words <- str_count(input, boundary("word"))
        input_words <- unlist(str_split(input, boundary("word")))
        input_words <- tolower(input_words)
        
        if (nr_words>=3){
                predictions <- search_quadgram(input_words)
        } else {
               if (nr_words==2){
                        predictions <- search_trigram(input_words)
                } else {if (nr_words==1){
                       predictions <- search_bigram(input_words)
                } 
                }
        }
        return(predictions)
}


