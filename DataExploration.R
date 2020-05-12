#Set WD
setwd('/Users/loanple/Desktop/Winter2020/5205/Project')

#Packages
library(tidyverse)
library(readxl)
library(tidytext)
install.packages('textstem')
library(textstem)

#Read data
project <- read_excel("/Users/loanple/Desktop/Winter2020/5205/Project/TEXT MINING DATASET.xlsx")

#Data exploration
unique(project$TICKER)
str(project)
  #converting movement variable to factor
 project$Movement <- as.factor(project$Movement)
 #convert words to lowercase
 project$`RISK FACTORS`<- tolower(project$`RISK FACTORS`)
 project$`MANAGEMENT'S DISCUSSION`<- tolower(project$`MANAGEMENT'S DISCUSSION`)
 project$`AUDITORS COMMENTS`<- tolower(project$`AUDITORS COMMENTS`)
#Mean Character
mean_char_rf <- mean(nchar(project$`RISK FACTORS`)); mean_char_rf #32133.3
mean_char_md <- mean(nchar(project$`MANAGEMENT'S DISCUSSION`)); mean_char_md #11683.2
mean_char_ac <- mean(nchar(project$`AUDITORS COMMENTS`)); mean_char_ac # 5790.7
#Mean Words
mean_word_rf <- mean(str_count(project$`RISK FACTORS`)); mean_word_rf #32039.3
mean_word_md <- mean(str_count(project$`MANAGEMENT'S DISCUSSION`)); mean_word_md #32039.3
mean_word_ac <- mean(str_count(project$`AUDITORS COMMENTS`)); mean_word_ac #5764.9
#Mean Sentences
mean_sentence_rf <- mean(str_count(string = project$`RISK FACTORS`,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]")); mean_sentence_rf #160
mean_sentence_md <- mean(str_count(string = project$`MANAGEMENT'S DISCUSSION`,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]")); mean_sentence_md #94.8
mean_sentence_ac <- mean(str_count(string = project$`AUDITORS COMMENTS`,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]")); mean_sentence_ac #26.7

#Text mining
#Unigram
  #Risk Factors
  project %>%
    unnest_tokens(word, `RISK FACTORS`)%>%
    select(word)%>%
    anti_join(stop_words)%>%
    group_by(word)%>%
    summarize(count = n())%>%
    ungroup()%>%
    arrange(desc(count))%>%
    top_n(25) %>% ggplot(aes(x=reorder(word,count), y=count, fill=count))+
    geom_col()+
    xlab('words')+
    coord_flip()
  
  #Management Discussion
  project %>%
    unnest_tokens(word, `MANAGEMENT'S DISCUSSION`)%>%
    select(word)%>%
    anti_join(stop_words)%>%
    group_by(word)%>%
    summarize(count = n())%>%
    ungroup()%>%
    arrange(desc(count))%>%
    top_n(25) %>% ggplot(aes(x=reorder(word,count), y=count, fill=count))+
    geom_col()+
    xlab('words')+
    coord_flip()
  
  #Auditor's Comments
  project %>%
    unnest_tokens(word, `AUDITORS COMMENTS`)%>%
    select(word)%>%
    anti_join(stop_words)%>%
    group_by(word)%>%
    summarize(count = n())%>%
    ungroup()%>%
    arrange(desc(count))%>%
    top_n(25) %>% ggplot(aes(x=reorder(word,count), y=count, fill=count))+
    geom_col()+
    xlab('words')+
    coord_flip()

#Adding stop words
custom_stop_words <- bind_rows(tibble(word = c("fiscal", "products", "tbd", "ep", "fmr"), lexicon = c("custom")), stop_words)

#Bigram 
#Risk Factors
  rf_binary <- project %>%
    unnest_tokens(bigram,`RISK FACTORS`, token = "ngrams", n = 2); rf_binary
  #removing stop words
  rf_bigrams <- rf_binary %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  rf_filtered <- rf_bigrams  %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  # new bigram counts:
  rf_counts <- rf_filtered %>% 
    count(word1, word2, sort = TRUE)
  view(rf_counts)
  #combining word1 + word2
  rf_counts$combined = paste(rf_counts$word1,rf_counts$word2)
  view(rf_counts)
  #graphing
  rf_counts_n16<-rf_counts %>%filter(n>16)
  ggplot(data = rf_counts_n16, mapping = aes(x = reorder(combined, n), n)) + 
    geom_bar(stat = "identity") + coord_flip()
  
#Mangement's Discussion
  md_binary <- project %>%
    unnest_tokens(bigram,`MANAGEMENT'S DISCUSSION`, token = "ngrams", n = 2); md_binary
  #removing stop words
  md_bigrams <- md_binary %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  md_filtered <- md_bigrams  %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  # new bigram counts:
  md_counts <- md_filtered %>% 
    count(word1, word2, sort = TRUE)
  view(md_counts)
  #combining word1 + word2
  md_counts$combined = paste(md_counts$word1,md_counts$word2)
  view(md_counts)
  #graphing
  md_counts_n10<-md_counts %>%filter(n>10)
  ggplot(data = md_counts_n10, mapping = aes(x = reorder(combined, n), n)) + 
    geom_bar(stat = "identity") + coord_flip()
  
#Auditor's Comments
  ac_binary <- project %>%
    unnest_tokens(bigram,`AUDITORS COMMENTS`, token = "ngrams", n = 2); ac_binary
  #removing stop words
  ac_bigrams <- ac_binary %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  ac_filtered <- ac_bigrams  %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  # new bigram counts:
  ac_counts <- ac_filtered %>% 
    count(word1, word2, sort = TRUE)
  view(ac_counts)
  #combining word1 + word2
  ac_counts$combined = paste(ac_counts$word1,ac_counts$word2)
  view(ac_counts)
  #graphing
  ac_counts_n10<-ac_counts %>%filter(n>10)
  ggplot(data = ac_counts_n10, mapping = aes(x = reorder(combined, n), n)) + 
    geom_bar(stat = "identity") + coord_flip()
  
#NRC Lexicon Analysis
  library(lexicon)
  
  #RISK FACTORS
    project %>%
    select(TICKER, `RISK FACTORS`)%>%
    group_by(TICKER)%>%
    unnest_tokens(output = word, input = `RISK FACTORS`)%>%
    inner_join(y = hash_sentiment_nrc,by = c('word'='x'))%>%
    ungroup()%>%
    group_by(y)%>%
    summarize(count = n())%>%
    ungroup()
    
    #MD DISCUSSION
    project %>%
      select(TICKER, `MANAGEMENT'S DISCUSSION`)%>%
      group_by(TICKER)%>%
      unnest_tokens(output = word, input = `MANAGEMENT'S DISCUSSION`)%>%
      inner_join(y = hash_sentiment_nrc,by = c('word'='x'))%>%
      ungroup()%>%
      group_by(y)%>%
      summarize(count = n())%>%
      ungroup()
    
    #AUDITORS COMMENTS
    project %>%
      select(TICKER, `AUDITORS COMMENTS`)%>%
      group_by(TICKER)%>%
      unnest_tokens(output = word, input = `AUDITORS COMMENTS`)%>%
      inner_join(y = hash_sentiment_nrc,by = c('word'='x'))%>%
      ungroup()%>%
      group_by(y)%>%
      summarize(count = n())%>%
      ungroup()
  
#Financial Loughran Lexicon
    library(stringr)
    
    #RISK FACTORS
      project %>%
      select(TICKER, `RISK FACTORS`) %>%
      unnest_tokens(output = word, input = `RISK FACTORS`) %>%
      count(word) %>%
      inner_join(get_sentiments("loughran"), by = "word") %>%
      group_by(sentiment) %>%
      top_n(10, n) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ sentiment, scales = "free") +
      ylab("Frequency of words in Risk Factors")
      
      #grouping by Ticker
      RF_sentiment_count <- 
        project %>%
        unnest_tokens(output = word, input = `RISK FACTORS`) %>%
        inner_join(get_sentiments("loughran"), by = "word") %>%
        count(sentiment, TICKER) %>%
        spread(sentiment, n, fill = 0)
      
      RF_sentiment_count

      #positive against negative
      RF_sentiment_count %>%
        mutate(score = (positive - negative) / (positive + negative)) %>%
        mutate(TICKER = reorder(TICKER, score)) %>%
        ggplot(aes(TICKER, score, fill = score > 0)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = "Company",
             y = "Positivity score among Companies") 
      
      #MD
      project %>%
        select(TICKER, `MANAGEMENT'S DISCUSSION`) %>%
        unnest_tokens(output = word, input = `MANAGEMENT'S DISCUSSION`) %>%
        anti_join(stop_words)%>%
        count(word) %>%
        inner_join(get_sentiments("loughran"), by = "word") %>%
        group_by(sentiment) %>%
        top_n(10, n) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        geom_col() +
        coord_flip() +
        facet_wrap(~ sentiment, scales = "free") +
        ylab("Frequency of words in Mangement's Discussion")
      
      #grouping by Ticker
      MD_sentiment_count <- 
        project %>%
        unnest_tokens(output = word, input = `MANAGEMENT'S DISCUSSION`) %>%
        anti_join(stop_words)%>%
        inner_join(get_sentiments("loughran"), by = "word") %>%
        count(sentiment, TICKER) %>%
        spread(sentiment, n, fill = 0)
      
      MD_sentiment_count
      
      #positive against negative
      MD_sentiment_count %>%
        mutate(score = (positive - negative) / (positive + negative)) %>%
        mutate(TICKER = reorder(TICKER, score)) %>%
        ggplot(aes(TICKER, score, fill = score > 0)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = "Company",
             y = "Positivity score among Companies") 
      
      
      #AUDITORS COMMENTS
      project %>%
        select(TICKER, `AUDITORS COMMENTS`) %>%
        unnest_tokens(output = word, input = `AUDITORS COMMENTS`) %>%
        anti_join(stop_words)%>%
        count(word) %>%
        inner_join(get_sentiments("loughran"), by = "word") %>%
        group_by(sentiment) %>%
        top_n(10, n) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        geom_col() +
        coord_flip() +
        facet_wrap(~ sentiment, scales = "free") +
        ylab("Frequency of words in Auditor's Comments")
      
      #grouping by Ticker
      AC_sentiment_count <- 
        project %>%
        unnest_tokens(output = word, input = `AUDITORS COMMENTS`) %>%
        anti_join(stop_words)%>%
        inner_join(get_sentiments("loughran"), by = "word") %>%
        count(sentiment, TICKER) %>%
        spread(sentiment, n, fill = 0)
      
      AC_sentiment_count
      
      #positive against negative
      AC_sentiment_count %>%
        mutate(score = (positive - negative) / (positive + negative)) %>%
        mutate(TICKER = reorder(TICKER, score)) %>%
        ggplot(aes(TICKER, score, fill = score > 0)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = "Company",
             y = "Positivity score among Companies") 
      
      
#Loughran DataFrame Setup  - LEMMATIZATION 
      
      #Lemmatize section variables
      project$AC_lem <- project$`AUDITORS COMMENTS` %>% lemmatize_strings()
      project$MD_lem <- project$`MANAGEMENT'S DISCUSSION` %>% lemmatize_strings()
      project$RF_lem <- project$`RISK FACTORS` %>% lemmatize_strings()
      
  
        
     #RISK FACTORS TEXT_MINING DATA FRAME
       RF_loughran <- project %>%
       unnest_tokens(word, `RF_lem`) %>%
       anti_join(stop_words) %>%
       count(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, word, sort = TRUE) %>%
       ungroup() %>%
       inner_join(get_sentiments("loughran"), by = "word")
     RF_loughran
      #graph: Frequency of words for sentiment
       RF_loughran %>% group_by(sentiment) %>%
         top_n(10, n) %>%
         ungroup() %>%
         mutate(word = reorder(word, n)) %>%
         ggplot(aes(word, n)) +
         geom_col() +
         coord_flip() +
         facet_wrap(~ sentiment, scales = "free") +
         ylab("Frequency of words in Risk Factors")
       #graph: Positivity vs Negativity
       RF_loughran %>%
         count(sentiment, TICKER) %>%
         spread(sentiment, n, fill = 0) %>%
         mutate(score = (positive - negative) / (positive + negative)) %>%
         mutate(TICKER = reorder(TICKER, score)) %>%
         ggplot(aes(TICKER, score, fill = score > 0)) +
         geom_col(show.legend = FALSE) +
         coord_flip() +
         labs(x = "Company",
              y = "Risk Factors - Positivity score by Ticker") 
       #graph: Sentiment range
       RF_loughran %>%
        count(sentiment, TICKER) %>%
       ggplot(aes(fill=sentiment, y=n, x=TICKER)) + 
         coord_flip()+
         geom_bar(position="fill", stat="identity") +
         ggtitle("Risk Factor Sentiment by Ticker")
    
     
     #MANAGEMENTS TEXT_MINING DATA FRAME
     MD_loughran <- project %>%
       unnest_tokens(word, `MD_lem`) %>%
       anti_join(stop_words) %>%
       count(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, word, sort = TRUE) %>%
       ungroup() %>%
       inner_join(get_sentiments("loughran"), by = "word")
     view(MD_loughran)
     #graph: Positivity vs Negativity
     MD_loughran %>%
       count(sentiment, TICKER) %>%
       spread(sentiment, n, fill = 0) %>%
       mutate(score = (positive - negative) / (positive + negative)) %>%
       mutate(TICKER = reorder(TICKER, score)) %>%
       ggplot(aes(TICKER, score, fill = score > 0)) +
       geom_col(show.legend = FALSE) +
       coord_flip() +
       labs(x = "Company",
            y = "Management's Discussion - Positivity score by Ticker") +
     #graph: Sentiment range
     MD_loughran %>%
       count(sentiment, TICKER) %>%
       ggplot(aes(fill=sentiment, y=n, x=TICKER)) + 
       coord_flip()+
       geom_bar(position="fill", stat="identity") +
       ggtitle("Management's Discussion Sentiment by Ticker")

     #AUDITOR'S COMMENTS TEXT_MINING DATA FRAME
     AC_loughran <- project %>%
       unnest_tokens(word, `AC_lem`) %>%
       anti_join(stop_words) %>%
       count(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, word, sort = TRUE) %>%
       ungroup() %>%
       inner_join(get_sentiments("loughran"), by = "word")
     view(AC_loughran)   
     #graph: Positivity vs Negativity
     AC_loughran %>%
       count(sentiment, TICKER) %>%
       spread(sentiment, n, fill = 0) %>%
       mutate(score = (positive - negative) / (positive + negative)) %>%
       mutate(TICKER = reorder(TICKER, score)) %>%
       ggplot(aes(TICKER, score, fill = score > 0)) +
       geom_col(show.legend = FALSE) +
       coord_flip() +
       labs(x = "Company",
            y = "Auditor's Comments - Positivity score by Ticker") 
       #graph: Sentiment range
       AC_loughran %>%
       count(sentiment, TICKER) %>%
       ggplot(aes(fill=sentiment, y=n, x=TICKER)) + 
       coord_flip()+
       geom_bar(position="fill", stat="identity") +
       ggtitle("Auditor's Comments Sentiment by Ticker")
       
       #LEM
       #Creating new dataframe to merge sections
       all<-bind_rows(list(AC_loughran,MD_loughran,RF_loughran))
       all <- all %>% group_by(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, sentiment,word) %>% summarise(sum(n))
       colnames(all)[which(names(all) == "sum(n)")] <- "word_count"
       all      
       
       #graph: Frequency of words for sentiment
       all %>% group_by(sentiment) %>%
         top_n(20, word_count) %>%
         ungroup() %>%
         mutate(word = reorder(word, word_count)) %>%
         ggplot(aes(word, word_count)) +
         geom_col() +
         coord_flip() +
         facet_wrap(~ sentiment, scales = "free") +
         ylab("Frequency of words")

#FINANCIAL DICTIONARY - LOUGHRAN STEM
  #change adversely --> adverse
  #change requirement --> require
       #Stem section variables 
       project$AC_stem <- project$`AUDITORS COMMENTS` %>% stem_strings()
       project$MD_stem <- project$`MANAGEMENT'S DISCUSSION` %>% stem_strings()
       project$RF_stem <- project$`RISK FACTORS` %>% stem_strings()
  
       #RISK FACTORS TEXT_MINING DATA FRAME
       RFstem_loughran <- project %>%
         unnest_tokens(word, `RF_stem`) %>%
         anti_join(stop_words) %>%
         count(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, word, sort = TRUE) %>%
         ungroup() %>%
         inner_join(get_sentiments("loughran"), by = "word")
       view(RFstem_loughran)
       
       #MANAGEMENTS TEXT_MINING DATA FRAME
       MDstem_loughran <- project %>%
         unnest_tokens(word, `MD_stem`) %>%
         anti_join(stop_words) %>%
         count(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, word, sort = TRUE) %>%
         ungroup() %>%
         inner_join(get_sentiments("loughran"), by = "word")
       view(MD_loughran)
       
       #AUDITOR'S COMMENTS TEXT_MINING DATA FRAME
       ACstem_loughran <- project %>%
         unnest_tokens(word, `AC_stem`) %>%
         anti_join(stop_words) %>%
         count(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, word, sort = TRUE) %>%
         ungroup() %>%
         inner_join(get_sentiments("loughran"), by = "word")
       
     
#Creating new dataframe to merge sections STEM
     allstem<-bind_rows(list(ACstem_loughran,MDstem_loughran,RFstem_loughran))
     allstem <- allstem %>% group_by(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, sentiment,word) %>% summarise(sum(n))
     colnames(allstem)[which(names(allstem) == "sum(n)")] <- "word_count"
     view(allstem)
     view(all)
     
     #graph: Frequency of words for sentiment
     allstem %>% group_by(sentiment) %>%
       top_n(20, word_count) %>%
       ungroup() %>%
       mutate(word = reorder(word, word_count)) %>%
       ggplot(aes(word, word_count)) +
       geom_col() +
       coord_flip() +
       facet_wrap(~ sentiment, scales = "free") +
       ylab("Frequency of words")
     
     
     

    
