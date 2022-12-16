#################################################
#           tidy sentiment Analysis             #
#################################################

library("shiny")
library("tidytext")
library("tidyr")
library("dplyr")
library("ggplot2")
library("DT")
library("reshape2")
library("wordcloud")
library("plotly")

#--------------------------------------------#

shinyUI(fluidPage(
  
   # tags$head(includeScript("google_analytics.js")),
  
  #titlePanel("Sentiment Analysis with tidytext"),
  titlePanel(title=div(img(src="logo.png",align='right'),"Sentiment Analysis with tidytext")),
  
  # Input in sidepanel:
  sidebarPanel(
    fileInput("file", "Upload text file"),  
    
    selectInput("lexicon", "Sentiment Dictionary",
    c("AFINN","bing","nrc","loughran","User Defined"="userdefined"), selected = "AFINN"),
    
    uiOutput("dictionary"),
    textInput("stopw", ("Enter stop words separated by comma(,)"), value = "will,can"),
    
    numericInput("index", "Document Index", 1)
    # submitButton(text = "Apply Changes", icon("refresh"))
    
  ),
  
  # Main Panel:
  mainPanel( 
    
    
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview",h4(p("How to use this App")),
                         
                         p("To use this app you need a document corpus in txt file format. Make sure each document is separated from another document with a new line character.
                           To do basic sentiment analysis in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
	                       a(href="https://www.youtube.com/watch?v=4b7ZidC6ZOA","Youtube Link for App Navigation"),
                         
                         p("You can change the sentiment dictionary in left-sidebar panel. This app supports four inbuilt sentiment dictionaries and one user defined dictionary. If a user selects User Defined dictionary, then a browse file input will appear below sentiment dictionary drop-down in left-side-bar panel and user can upload the user defined dictionary. This user defined dictionary should be in csv format and first column of the dictionary should be word and second column should be score. You can download the sample user defined dictionary below.", align = "justify"),
                         a(href="http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010","1- Afinn"),
                         p("AFINN is a list of English words rated for valence with an integer between minus five (negative) and plus five (positive). The words have been manually labeled by Finn Arup Nielsen in 2009-2011."),
                      
                         a(href="https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html","2- Bing"),
                         p("This sentiment dictionary is created by Bing Liu and collaborators. In this dictionary, words are classified as positive or negative."),
                      
                         a(href="http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm","3- NRC"),
                         p("The NRC Emotion Lexicon is a list of English words and their associations with eight basic emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive). The annotations were manually done by crowdsourcing."),
                      
                         a(href="http://www3.nd.edu/~mcdonald/Word_Lists.html","4- Loughran"),
                         p("This dictionary is created by Tim Loughran and Bill McDonald. In this dictionary each word is classified in financial context (uncertainty, litigious, constraining, superfluous, positive, negative)"),
                        
                         h4(p("Download Sample user defined dictionary file")),
                         downloadButton('downloadData3', 'Download User Defined Dictionary (works only in browser)'),br(),br(),
                         
                         p("In the left-side bar panel you can change the document index number and accordingly document level analysis will be updated in \"Document level Analysis\" tab", align = "justify"),
                         
                         p("If plots are not working in \"Sentiments - Plot\" tab then please install latest version of ggplot2. You can install ggplot2 by command - install.packages(\"ggplot2\")", align = "justify"),
                         h4(p("Download Sample text file")),
                         downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file (works only in browser)'),br(),br(),
                         downloadButton('downloadData4', 'Download One Plus reviews txt file (works only in browser)'),br(),br(),
			 downloadButton('downloadData5', 'Download Bahubali reviews txt file (works only in browser)'),br(),br(),
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                         img(src = "example1.png")
                       
                         ),
                
                tabPanel("Sentiments - Plot",h4(),
                         # verbatimTextOutput('chk'),
                         # 
                         uiOutput("sent.plots"),
                         verbatimTextOutput("event")
                         # h4("Weights Distribution of Wordcloud"),
                         # verbatimTextOutput("dtmsummary1")
                         ),
                tabPanel("Sentiments - Stat",br(),
                         plotOutput("word.cloud",height = 700, width = 700),
                         br(),
                         dataTableOutput("count"),
                         br()
                         ),

                tabPanel("Document level Analysis",br(),
                         downloadButton('downloadData2', 'Download Sentiment Scores (Works only in browser)'), br(),br(),
                         h4("List of the most frequent sentiment words at each level"),
                         dataTableOutput("table"),
                         br(),
                         h4("Document parsed into sentences and sentiment score for each sentence"),
                         # downloadButton('downloadData4', 'Downlaod Sentiemnt Scores (Works only in browser)'), br(),br(),
                         dataTableOutput("table2")),
			          tabPanel("Valence Shifters", br(),
			                   dataTableOutput("ValencesShifted")
			            
			          ),
			          tabPanel("Sentiment Scores", br(),
			                   dataTableOutput("Scores_by_Lexicon")
			                   )
                #                         
                         )
                )
  
)
)
