# Capstone Project Data Science (Coursera)
# SHINY Application: Predictive Text Product
# Author: Myriam Ragni
# Date: April 2020
#
# This is the server logic of a Shiny web application.
# To run the application, click 'Run App' above. 
#
# --------------------------
# Setup the environment ----
# --------------------------
# ---- server ----
suppressWarnings(library(shiny))
suppressWarnings(library(dplyr))
suppressWarnings(library(stringr))
suppressWarnings(library(textclean))
suppressWarnings(library(DT))

# ------------------------------------------------------------------
# Loads the R script used to cleanup the input text and search  ----
# for the next word in the frequency dictionaries               ----
# Loads the frequency dictionaries                              ----
# Creates a subset of the top100 unigrams                       ----
# ------------------------------------------------------------------
source("./PredictWord.R",local=TRUE)
uni_words_freq <- readRDS(file="./dict/en_US/train_unigram_prob_tbl.Rds")
uni_words_freq100 <- uni_words_freq[order(-prob)][1:100]
bi_words_freq <- readRDS(file="./dict/en_US/train_bigram_prob_tbl.Rds")
tri_words_freq <- readRDS(file="./dict/en_US/train_trigram_prob_tbl.Rds")
quad_words_freq <- readRDS(file="./dict/en_US/train_quadgram_prob_tbl.Rds")

# -----------------------------
# Define the server logic  ----
# -----------------------------
shinyServer(function(input, output) {
    
        # ----------------------------------------------------------------
        # Returns the sentence entered by the user in the main panel  ----
        # ----------------------------------------------------------------  
        output$text <- renderText({
                paste("You entered the text: ", input$text)
        })
        
        observe({
                t1 <- Sys.time()
                textCleaned <- CleanInput(input$text)
                # --------------------------------------------------------------------------
                # Returns the text after transformation (cleanup, change to lowercases) ----
                # --------------------------------------------------------------------------  
                output$cleanedtext <- renderText({
                        paste("Text after cleanup: ", textCleaned)
                })
                
                if(textCleaned !=""){
                # ----------------------------------
                # Runs the prediction algorithm ----
                # ----------------------------------
                predictions <- predict_results(textCleaned)
                t2 <- Sys.time()
                
                # ----------------------------------------------------------------------------
                # Returns a concatenation of the initial text and the predicted next word ----
                # (Predicted next word = ???? if not found in 4,3,2 Grams)                ----
                # ----------------------------------------------------------------------------  
                output$predictedtext <- renderText({
                        if (ncol(predictions)=="3"){ ####backoff to unigram
                                predictedtext <- c("????")   
                        } else predictedtext <- predictions[1,(ncol(predictions)-2)]
                        paste("Prediction (highest probability of occurrence): ", input$text, predictedtext)
                })
  
                # -----------------------------------------------------------------------------
                # Returns information about the dictionary used and processing time        ----
                # ----------------------------------------------------------------------------- 
                output$msgGram <- renderText({
                        if (ncol(predictions)=="6"){
                                msgGram <- c(" Word predicted using the 4-Grams dictionary - top10 options below")
                        } else {
                                if (ncol(predictions)=="5"){
                                        msgGram <- c("Word predicted using the 3-Grams dictionary - top10 options below")   
                                } else {
                                        if (ncol(predictions)=="4"){
                                               msgGram <- c("Word predicted using the 2-Grams dictionary - top10 (if available) proposals below")      
                                        } else msgGram <- c("Word could not be predicted with the 4-2Grams, therefore showing 10 randomly chosen 1-grams among the top100")
                               }
                        }
                        paste(msgGram,"\n",sprintf("Processing Time: %.5f secs", (difftime(t2,t1, units="secs"))))
                })
                
                # -----------------------------------------------------------------------------
                # Returns a table of the possible next words found in the appropriate      ----
                # dictionary and which have the highest probability                        ----
                # Fallback to 10 randomly selected unigrams among the top 100 if no match  ----
                # ----------------------------------------------------------------------------- 
                output$mytable = DT::renderDataTable({
                        DT::datatable(predictions, rownames = FALSE, 
                                      filter="none", selection ="none", 
                                      editable=FALSE, 
                                      options=list(dom="t",
                                                   columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
                        formatStyle((ncol(predictions)-2), fontSize = "10pt", color="green")
                })
                   
                }
        })
})

