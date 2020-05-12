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

#Financial Loughran Lexicon
library(stringr)

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

#RISK FACTORS TEXT_MINING DATA FRAME
RFstem_loughran1 = project %>%
  unnest_tokens(word, `RISK FACTORS`) %>%
  anti_join(stop_words) %>%
  count(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`, word, sort = TRUE) %>%
  ungroup() %>%
  inner_join(get_sentiments("loughran"), by = "word")

RFstem_loughran1$word <-textstem::stem_words(RFstem_loughran1$word)
RFstem_loughran1$word

view(project)

    #graph: Frequency of words for sentiment
RFstem_loughran %>% group_by(sentiment) %>%
      top_n(10, n) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~ sentiment, scales = "free") +
      ylab("Frequency of words in Risk Factors")
    #graph: Positivity vs Negativity
RFstem_loughran %>%
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
RFstem_loughran %>%
      count(sentiment, TICKER) %>%
      ggplot(aes(fill=sentiment, y=n, x=TICKER)) + 
      coord_flip()+
      geom_bar(position="fill", stat="identity") +
      ggtitle("Risk Factor Sentiment by Ticker")

#MANAGEMENTS TEXT_MINING DATA FRAME
MDstem_loughran <- project %>%
  unnest_tokens(word, `MD_stem`) %>%
  anti_join(stop_words) %>%
  count(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, word, sort = TRUE) %>%
  ungroup() %>%
  inner_join(get_sentiments("loughran"), by = "word")
view(MD_loughran)

    #graph: MDstem_loughran %>% group_by(sentiment) %>%
  top_n(10, n) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ sentiment, scales = "free") +
    ylab("Frequency of words in Risk Factors")
  #graph: Positivity vs Negativity
  MDstem_loughran %>%
    count(sentiment, TICKER) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(score = (positive - negative) / (positive + negative)) %>%
    mutate(TICKER = reorder(TICKER, score)) %>%
    ggplot(aes(TICKER, score, fill = score > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = "Company",
         y = "Management's Discussion - Positivity score by Ticker") 
  #graph: Sentiment range
  MDstem_loughran %>%
    count(sentiment, TICKER) %>%
    ggplot(aes(fill=sentiment, y=n, x=TICKER)) + 
    coord_flip()+
    geom_bar(position="fill", stat="identity") +
    ggtitle("Management's Discussion Sentiment by Ticker")

#AUDITOR'S COMMENTS TEXT_MINING DATA FRAME
ACstem_loughran <- project %>%
  unnest_tokens(word, `AC_stem`) %>%
  anti_join(stop_words) %>%
  count(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, word, sort = TRUE) %>%
  ungroup() %>%
  inner_join(get_sentiments("loughran"), by = "word")

  #graph: ACstem_loughran %>% group_by(sentiment) %>%
  top_n(10, n) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ sentiment, scales = "free") +
    ylab("Frequency of words in Risk Factors")
  #graph: Positivity vs Negativity
  ACstem_loughran %>%
    count(sentiment, TICKER) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(score = (positive - negative) / (positive + negative)) %>%
    mutate(TICKER = reorder(TICKER, score)) %>%
    ggplot(aes(TICKER, score, fill = score > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = "Company",
         y = "Auditor’s Comment - Positivity score by Ticker") 
  #graph: Sentiment range
  ACstem_loughran %>%
    count(sentiment, TICKER) %>%
    ggplot(aes(fill=sentiment, y=n, x=TICKER)) + 
    coord_flip()+
    geom_bar(position="fill", stat="identity") +
    ggtitle("Auditor’s Comment Sentiment by Ticker")



#Creating new dataframe to merge sections STEM
allstem<-bind_rows(list(ACstem_loughran,MDstem_loughran,RFstem_loughran))
allstem <- allstem %>% group_by(TICKER,`STOCK PRICE PRE`,`STOCK PRICE POST`,Movement, sentiment,word) %>% summarise(sum(n))
colnames(allstem)[which(names(allstem) == "sum(n)")] <- "word_count"
view(allstem)
allstem


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


#PREDICTION
library(caTools)
  #SPLITTING DATASET
  set.seed(617)
  split = sample.split(Y = allstem$Movement, SplitRatio = 0.8)
  train = allstem[split,]
  test = allstem[!split,]
  
  #Finding Unique words in train & test
      trainwords=unique(train$word)
      testwords = unique(test$word)
      
      wwtest = c()
      
      for (i in testwords){
        if (!(i %in% trainwords)){
          wwtest = cbind(wwtest,i)
        }
      }
      
      wwtest
      
      
    #Removing words not in train
      testunique = c("break","spam","complaint","constrain","entail","therefrom","default","misconduct","stringent","reinterpret","thereof","poor","wrong")
      testsubset = subset(test, ! word %in% testunique)
            view(testsubset)

  #LOGISTIC REGRESSION
  model1 = glm(Movement~sentiment+word_count,data=train,family='binomial')
  model2 = glm(Movement~sentiment+word+word_count,data=train,family='binomial')
  
  summary(model1)
  summary(model2)

  anova(model1, test="Chisq")  
  anova(model2, test="Chisq")
  
  #Predict
  pred1 = predict(model1,type="response")
  ct1 = table(train$Movement,pred1>0.5); ct1
  accuracy1 = (ct[1,1]+ct[2,2])/nrow(train); accuracy1 # Accuracy = 0.6506024
  
  pred2 = predict(model2,type="response")
  ct2 = table(train$Movement,pred2>0.5); ct2
  accuracy2 = (ct2[1,1]+ct2[2,2])/nrow(train); accuracy2 # Accuracy = 0.6168675
  
  #Test data
  pred1Test = predict(model1,newdata=test,type="response")
  ct1Test = table(testsubset$Movement,pred1Test>0.5); ct1Test
  (ct1Test[1,1]+ct1Test[2,2])/nrow(test) #Accuracy = 0.5192308
  
    #Using testsubset
  pred2Test = predict(model2,newdata=testsubset,type="response")
  ct2Test = table(testsubset$Movement,pred2Test>0.5); ct1Test
  (ct2Test[1,1]+ct2Test[2,2])/nrow(testsubset) #Accuracy = 0.4777778
  
#DECISION TREE
  library(rpart); library(rpart.plot)
  
  tree1 = rpart(Movement~sentiment+word_count,data=train,
                method="class")
  predTree1 = predict(tree1,type="class")
  ct = table(train$Movement,predTree1); ct
  (ct[1,1]+ct[2,2])/nrow(train) # Accuracy = 0.5277108
  
  
  tree2 = rpart(Movement~sentiment+word_count+word,data=train,
                method="class")
  predTree2 = predict(tree2,type="class")
  ct = table(train$Movement,predTree2); ct
  (ct[1,1]+ct[2,2])/nrow(train) # Accuracy = 0.7277108
  
  #Predict with tree model
  predTree1_test = predict(tree1,newdata=test,type="class")
  ct = table(test$Movement,predTree1_test); ct
  (ct[1,1]+ct[2,2])/nrow(test) #Accuracy = 0.5096154
  
  predTree2_test = predict(tree2,newdata=testsubset,type="class")
  ct = table(testsubset$Movement,predTree2_test); ct
  (ct[1,1]+ct[2,2])/nrow(testsubset) #Accuracy = 0.4666667
  
  
  view(test)

  