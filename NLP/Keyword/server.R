#################################################
#               KeyWord Filtering               #
#################################################

require(dplyr)
require(tidytext)

shinyServer(function(input, output,session) {
  set.seed=1082022   
  
  dataset <- reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      if(file_ext(input$file$datapath)=="txt"){
        Document = readLines(input$file$datapath)
        Document  =  str_replace_all(Document, "<.*?>", "") # get rid of html junk 

        #colnames(Document) <- c("Doc.id","Document")
        Doc.id=seq(1:length(Document))
        calib=data.frame(Doc.id,Document)
        print(input$file$name)
        return(calib)}
      else{
        Document = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
        Document[,1] <- str_to_title(Document[,1])
        Document[,1] <- make.names(Document[,1], unique=TRUE)
        Document[,1] <- tolower(Document[,1])
        Document[,1] <- str_replace_all(Document[,1],"\\.","_")
        Document<-Document[complete.cases(Document), ]
        Document <- Document[!(duplicated(Document[,1])), ]
        rownames(Document) <- Document[,1]
        
        # colnames(Document) <- c("Doc.id","Document")
        #Doc.id=seq(1:length(Document))
        # calib=data.frame(Doc.id,Document)
        #print(input$file$name)
        
        return(Document)
      }
      
    }
  })
  
  cols <- reactive({colnames(dataset())})
  
  
  y_col <- reactive({
    x <- match(input$x,cols())
    y_col <- cols()[-x]
    return(y_col)
    
  })
  
  output$id_var <- renderUI({
    print(cols())
    selectInput("x","Select ID Column",choices = cols())
  })
  
  
  output$doc_var <- renderUI({
    selectInput("y","Select Text Column",choices = y_col())
  })
  
  
  output$up_size <- renderPrint({
    size <- dim(dataset())
    paste0("Dimensions of uploaded data: ",size[1]," (rows) X ", size[2]," (Columns)")
  })
  
  text_summ <- reactive({summary(quanteda::corpus(dataset()[,input$y]))})
  quant_mod <- reactive({quanteda::corpus(dataset()[,input$y])})
  
  output$text <- renderUI({
    req(input$file$datapath)
    str1 <- paste("Total no of documents:", nrow(dataset()))
    str2 <- paste("Range of sentences per document: ",min(text_summ()$Sentences),"-",max(text_summ()$Sentences))
    #str3 <- paste("Maximum number of sentence: ",)
    str4 <- paste("Average number of sentences per document: ",round(mean(text_summ()$Sentences),2))
    HTML(paste(str1, str2,str4, sep = '<br/>'))
  })
  
  output$text2 <- renderUI({
    req(input$file$datapath)
    str2 <- paste("Range of words per document: ",min(text_summ()$Tokens),'-',max(text_summ()$Tokens))
    #str3 <- paste("range of words per document:: ",max(text_summ()$Tokens))
    str4 <- paste("Average number of words: ",round(mean(text_summ()$Tokens),2))
    HTML(paste(str2,str4, sep = '<br/>'))
  })
  
  output$samp_data <- DT::renderDataTable({
    DT::datatable(head(dataset()),rownames = FALSE)
  })
  
  
  wrdl <- reactive({
    if(is.null(input$file2$datapath)){return(NULL)}
    return(readLines(input$file2$datapath))
  })
  
#  This chunk is working
  wordlist0 <- reactive({
    if (is.null(input$file)) {return(NULL)}
    #else{return(values$wordlist0)}
     
       a00 = unlist(strsplit(input$wordl,","))
       #a01 = readLines(input$file2$datapath)
       a01 = wrdl()
       wordlist0 = unique(gsub("'","",c(a00,a01)))
       
       return(wordlist0)
  })
  
  
  finalwordlist <- reactive({
      if (is.null(input$file)) {return(NULL)}
      else {
        corpus_lower = dataset()
        wl1 = NULL
        for (word in wordlist0()){
          if (sum(str_detect(corpus_lower, word)) > 0) {wl1 = c(wl1, word)} }
        return(wl1)
      }
    } 
  )
  
  
  output$wordl <- renderPrint(finalwordlist())
  
  #This Chunk is Working
  textdf =  reactive({
    
    textb = dataset()[,input$y]
    #ids = dataset()[,input$x]
    
    textdf1 = textb %>% tibble(text = .) %>%
      mutate(docID = row_number()) %>%    # row_number() is v useful.    
      group_by(docID) %>%
      unnest_tokens(sents, text, token="sentences", to_lower=FALSE) %>%
      mutate(sentID = row_number()) %>%
      select(docID, sentID, sents)
    
  })
  
  
 output$downloadThisToken <- downloadHandler(
   filename = function(){paste(str_split(input$file$name,"\\.")[[1]][1],"_SentenceTokenized.csv",collapse = "") },
   content = function(file) {
      
      new_df <- textdf()
      write.csv(new_df, file, row.names=T)
            
    }
   )
  
  
  # build unit func for wl against one doc
  doc_proc <- function(i0, textdf1, wl1){

    doc00 = textdf1[(textdf1$docID == i0),]
    sent_ind = NULL
    
    for (i1 in 1:nrow(doc00)){ # outer loop
      sent0 = doc00$sents[i1]
      for (word in wl1){ if (str_detect(sent0, word)) {sent_ind = c(sent_ind, i1); break} }
    } # i1 loop ends
    
    sent_ind1 = unique(sent_ind)
    df00 = doc00[(doc00$sentID %in% sent_ind1),]
    
    # rollback extracted sents into doc
    doc_sub = NULL
    for (i1 in 1:nrow(df00)){
      doc_sub = paste(doc_sub, df00$sents[i1], sep=" ")
    }
    
    df01 = data.frame(docID=i0, filtered_sents=doc_sub)
    
    return(df01) } # func ends
  
  # wrapper func 
  wrapper_corpus <- function(textdf, wl1){
    
    list_dfs = vector(mode="list", length=max(textdf$docID)) # use in wrapper func
    
    for (i0 in 1:max(textdf$docID)){
      list_dfs[[i0]] = doc_proc(i0, textdf, wl1)   } # i0 loop ends
    
    out_df = bind_rows(list_dfs)
    return(out_df) } # func ends
  
  output$SentenceToken = renderDataTable({
    datatable(textdf())
  })
  
  a00 <- reactive({
    corpus_lower = filteredCorpus()
    for (word in finalwordlist()){
      word00 = paste0("**",word,"**")
      corpus_lower = str_replace_all(corpus_lower, word, word00)
    }
    a00 = unlist(corpus_lower)
    
    a00 <- as.data.frame(a00)
    
    return(a00)
    
    
  })
  
  output$highlighted <- downloadHandler(
    filename = function(){paste(str_split(input$file$name,"\\.")[[1]][1],"_highilighted.csv",collapse = "")},
    content = function(file){
      new_df <- a00()
      write.csv(new_df, file, row.names =T)
    }
  )
  
  filteredCorpus <- reactive({
    outdf1 = wrapper_corpus(textdf(), finalwordlist())
    
    return(outdf1)
  })
  

  output$downloadThisOne = renderDataTable({
    #outdf1 = wrapper_corpus(textdf(), finalwordlist())
    #datatable(textdf())
    datatable(filteredCorpus())
  })
  
  output$checker <- renderPlot({
    new_df <- filteredCorpus()
    newdf <- nrow(filteredCorpus())
    newdf2 <- nrow(subset(new_df, filtered_sents !=" NA "))
    x <- c(newdf, newdf2)
    labels <- c("No of Documents Containing Keywords","No of Documents Containing NAs")
    pie(x, labels)
  })
  
   output$downloadTheOne <- downloadHandler(
   filename = function(){paste(str_split(input$file$name,"\\.")[[1]][1],"_Full_filtered.csv",collapse = "") },
   content = function(file) {
      new_df <- filteredCorpus()
      write.csv(new_df, file, row.names=T)      
    }
   )
    
   output$downloadThisTwo <- downloadHandler(
   filename = function(){paste(str_split(input$file$name,"\\.")[[1]][1],"_filtered.csv",collapse = "") },
   content = function(file) {
      
      new_df <- filteredCorpus()
      new_df <- subset(new_df, filtered_sents!=" NA ")
      write.csv(new_df, file, row.names=T)
            
    }
   )
   
  
    
   
  output$downloadData1 <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )
  
   output$downloadData2 <- downloadHandler(
    filename = function() { "Wordlist_Nokia.txt" },
    content = function(file) {
      writeLines(readLines("data/dummywordl.txt"), file)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() { "modi-speech-2022.txt" },
    content = function(file) {
      writeLines(readLines("data/modi-speech-2022.txt"), file)
    }
  )
  
    output$downloadData4 <- downloadHandler(
    filename = function() { "wordlist_modi.txt" },
    content = function(file) {
      writeLines(readLines("data/wordlist-speech.txt"), file)
    }
  )
  
  output$downloadData0 <- downloadHandler(
    filename = function() { "airline_sentiment.csv" },
    content = function(file) {
      write.csv(read.csv("data/airline_sentiment.csv"), file, row.names=F, col.names=F, fileEncoding = "UTF-8")
    }
  )
  
})
