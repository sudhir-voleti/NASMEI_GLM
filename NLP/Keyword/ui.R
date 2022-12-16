#################################################
#               KeyWord Filtering               #
#################################################

library(shiny)
library(purrr)
library(tidyr)
library(DT)
library(stringr)
library(tools)
library(dplyr)
library(tidytext)


shinyUI(fluidPage(
  title = "Keyword Filtering",
  titlePanel(title=div(img(src="logo.png",align='right'),"Keyword Filtering")),
  
  sidebarPanel(
    
    fileInput("file", "Upload text or CSV file : "),
    uiOutput('id_var'),
    uiOutput("doc_var"),
    textInput("wordl", ("Enter keywords to be filtered separated by comma(,) [Optional]"), value = "amazing, good"),
    fileInput("file2", "Upload keywords as a txt file. (Optional)")
    
  ),
  
  # Main Panel:
  mainPanel( 
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview & Example Dataset",h4(p("How to use this App")),
                         
                         p("To use this app you need a document corpus in txt file format. Make sure each document is separated from another document with a new line character.
                           To do filtering for multiple words in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed.", align = "justify"),
                         
                         h4("Please do not add spaces before or after the comma in the textbox for keywords."),
                         h4("The basic format of a wordlist .txt file is each word in a different line."),
                         #, height = 280, width = 400
                         br(),
                         h4(p("Download Sample text file")),
                         downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
                         downloadButton('downloadData2', 'Download word list - for Nokia'), br(), br(),
                         downloadButton('downloadData3', 'Download PM 2022 Independence Day Speech'), br(), br(),
                         #downloadButton('downloadData4', 'Download mandatory word list - Speech'), br(), 
                         br()
                         
                         
                ),
                tabPanel("Data Summary",
                         h4("Uploaded data size"),
                         verbatimTextOutput("up_size"),
                         h4("Sentence level summary"),
                         htmlOutput("text"),
                         hr(),
                         h4("Token level summary"),
                         htmlOutput("text2"),
                         hr(),
                         h4("Sample of uploaded datasest"),
                         DT::dataTableOutput("samp_data")
                ),
                tabPanel("Sentence Tokenized",
                         downloadButton('downloadThisToken', 'Download sentence tokenized corpus (as visible below'), br(),                       
                         dataTableOutput('SentenceToken')),
                tabPanel("Filtered Corpus",
                         h4("Final Filtered"),
                         h5("Filtering these keywords "),
                         verbatimTextOutput('wordl'),
                         br(),
                         h4("% Document Retained"),
                         plotOutput('checker'),
                         h4("Filtered Dataset"),
                         downloadButton('downloadThisTwo', 'Download filtered corpus (Without NAs)'), br(), br(),
                         downloadButton('highlighted','(In Beta) Download highlighted corpus'),br(),
                         downloadButton('downloadTheOne', 'Download filtered corpus'), br(), br(), 
                         dataTableOutput('downloadThisOne'),
                         
                         br() 
                )
                
    )
  )
)
)
