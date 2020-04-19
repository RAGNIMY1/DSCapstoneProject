# Capstone Project Data Science (Coursera)
# SHINY Application: Predictive Text Product
# Author: Myriam Ragni
# Date: April 2020
#
# This is the user-interface definition of a Shiny web application.
# To run the application, click 'Run App' above. 
#
# --------------------------
# Setup the environment ----
# --------------------------
rm(list = ls())
Sys.setlocale("LC_TIME", "English")

suppressWarnings(library(shiny))
suppressWarnings(library(shinythemes))
suppressWarnings(library(DT))


# ------------------
# Define the UI ----
# ------------------
shinyUI(fluidPage(
        theme = shinytheme("cerulean"),  
        # --------------
        # App title ----
        # --------------
        titlePanel(title=h1(em("A handy application to predict a word...."),align="center",style = "color: #4d3a7d;")),
        br(),br(),
        # -----------------------------------------------------
        # Sidebar layout with input and output definitions ----
        # -----------------------------------------------------
        sidebarLayout(
                # -----------------------------
                # Sidebar panel for inputs ----
                # -----------------------------
                sidebarPanel(width=4,
                             strong("If you are constantly groping for words, this APPS is for you!", style="color:blue"),
                             tags$br(),
                             p("It is based on Natural Language Processing algorithms.", style="color:blue"),
                             tags$img(src="./NLP.png", height=180, width=300),
                             tags$br(),
                             p("Read more about the APPS in the 'About' tab", style="color:blue"),
                             em("ENJOY...", style="color:blue"),
                             tags$br(),tags$br(),
                             textInput("text",
                                       label=div(style="color:black;font-size:14px",em("Enter your text for the prediction (in English)"))),
                             p("(e.g: Yesterday I wanted to go to the)"),
                             tags$br(),
                             em("Press the button below and wait for few seconds....", style="color:black;font-size:14px"),
                             submitButton("Search next word...."),
                             tags$br(),tags$br()

                ),
                # --------------------------------------
                # Main panel for displaying outputs ----
                # --------------------------------------
                mainPanel(width=8,
                          # --------------------------------
                          # Tab panels within Main Panel----
                          # --------------------------------
                          tabsetPanel(type="tabs",selected="Predictions",
                                      tags$style(type='text/css', '#text {background-color: rgba(255,0,255,0.3); color: blue;}'),
                                      tags$style(type='text/css', '#cleanedtext {background-color: rgba(0,0,255,0.3); color: black;}'),
                                      tags$style(type='text/css', '#predictedtext {background-color: rgba(0,255,0,0.3); color: blue;}'),
                                      tags$style(type='text/css', '#msgGram {background-color: rgba(192,192,192,0.3); color: blue;}'),
                                      tabPanel("About",
                                               h3("Highlights"),
                                               p("In our today's digitalized word, there are many examples of applications/tools which, while you are typing text, suggest the possible next word based on what you've already typed."),
                                               p("This application is exactly doing that, meaning: it takes as input a phrase (multiple words) and, thanks to a predictive text model, predicts, where possible, the next possible word(s)."),
                                               p("For the prediction, the algorithm is searching for a suitable word in reference dictionaries containing words and sequence of words."),
                                               p("The dictionaries generated are based on an English database containing blogs, news and twitter data (~72Mio. words)."),
                                              
                                               h3("Instructions"),
                                                withTags(
                                                       ol(
                                                          li("Type a (short) sentence [at least one word] in English in the pink box. Do not enter abbreviations, swear words, avoid tags, tweets, URLs."),
                                                          li("Click on the 'Search next word' button. Note that the process takes few seconds to complete and produce output, especially the 1st run."),
                                                          li("Navigate to the 'Predictions' tab for the results."),
                                                          li("Switch to the 'Top x-Grams' tabs to see a graphical representation of the top100 words/compination of words found in the dictionaries.")
                                                       )
                                               ),
                                               h3("Output (Predictions)"),
                                               tags$img(src="./PredictionSample_1.png", height=350, width=750),
                                               br(),
                                               tags$img(src="./PredictionSample_2.png", height=250, width=750),
                                               br(),
                                               HTML("<b>Note: </b> "),
                                               p("If the combination of the 3/2 words from the string (or the single word) you entered cannot be found in the corresponding dictionary, then the output table shows 10 randomly selected words among the top100 words. If this is the case, '???' is appended to your text, as shown below:"),
                                               tags$img(src="./NoWordsFound.png", height=250, width=650),
                                      ),
                                      
                                      tabPanel("Predictions",
                                               br(),
                                               conditionalPanel(condition="input.text !=''",
                                                               verbatimTextOutput("text"),
                                                               verbatimTextOutput("cleanedtext"),
                                                               verbatimTextOutput("predictedtext"),
                                                               verbatimTextOutput("msgGram"),
                                                               DT::dataTableOutput("mytable")
                                                               )
                                               ),

                                      tabPanel("Top 1-Grams",
                                               h4("Wordcloud depicting the top 100 uni-grams", align="center"),
                                               div(img(src="./cloudunifreq1.png"), style="text-align: center;")),
                                      tabPanel("Top 2-Grams",
                                               h4("Wordcloud depicting the top 100 bi-grams based on their probability of occurence", align="center"),
                                               div(img(src="./cloudbifreq1.png"), style="text-align: center;")),
                                      tabPanel("Top 3-Grams",
                                               h4("Wordcloud depicting the top 100 tri-grams based on their probability of occurence", align="center"),
                                               p("This chart shows that further cleanup activities would be needed to get rid of additional redundant words like amazon.*; in fact, these are not useful for the purpose of this application."),
                                               div(img(src="./cloudtrifreq.png"), style="text-align: center;")),
                                      tabPanel("Top 4-Grams",
                                               h4("Wordcloud depicting the top 100 quad-grams based on their probability of occurence", align="center"),
                                               div(img(src="./cloudquadfreq.png"), style="text-align: center;")),
                                      tabPanel("References",
                                               tags$br(),
                                               p("This application was built for the Capstone Project for the Johns Hopkings Data Science Specialization."),
                                               p("Throughput this curriculum we have learned a lot about the scientist's toolbox available to analyze data."),
                                               p("It was an amazing, sometimes challenging 10-months journey and I would like to thank Jeff Leek, Brian Caffo and Roger D. Peng for their professional talks and all the videos shared on that topic."),
                                               tags$a("This application is built with Shiny.", href="http://shiny.rstudio.com/"),
                                               tags$br(),
                                               HTML("<b>The code for this Shiny Application can be found here: </b> "),
                                               tags$a("https://github.com/RAGNIMY1/DSCapstoneProject", href="https://github.com/RAGNIMY1/DSCapstoneProject"))
                                      )
                          # --------------------------------
                          # End Tab panels              ----
                          # --------------------------------
                        )
                # --------------------------------------
                # End Main Panel                    ----
                # --------------------------------------
)))
