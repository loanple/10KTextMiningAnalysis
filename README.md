See markdown file: https://rpubs.com/loanple/615843

***

# 10K Text Mining Analysis
The project aims to model financial sentiment – using sentiment analysis and text mining– to predict stock price fluctuations. Leveraging sentiment analysis, we will systematically process the valence of words in various companies’ annual financial reports from Q12019-Q12020. Applying this analytical tool will enable stakeholders to quickly gauge whether the tone of these filings is positive, negative, or otherwise litigious to model the companies’ stock movements.

## **Problem Statement**

The project will evaluate whether the sentiment of 3 sections in a firm’s SEC Form 10-K is predictive of the firm’s stock fluctuation– namely, whether the stock price categorically increased or decreased. 

## **Dataset**

The dataset includes segments from 10 companies’ financial annual reports chosen from the 10 largest consumer discretionary stocks, as indicated by the S&P 500 index in early March 2020. The unique identifier are the below-specified companies chosen:
  - Amazon (AMZN)
  - HD (HD)
  - McDonalds (MCD)
  - Nike (NKE)
  - Starbucks Corp (SBUX)
  - TJ Maxx (TJX)
  - Lowe’s Cos Inc (Low)
  - Booking Holdings Inc (BKNG)
  - Target Corp (TGT)
  - Ford Motor Co (F)


The dataset derives 3 sections present in a company’s annual financial report, 10-K forms, from Q12019-Q12020, which discuss a firm’s current financial health and its future growth prospects. This section consists of character variables used for sentiment analysis. The 10-K sections included are be:
  - **Risk Factors**: outlines potential risk a company expects to face.
  - **Management Discussion**: outlines the company’s short term & long term goals, future expenses, and most importantly estimated future revenues 
  - **Auditor’s Comments**: an optional segment included in the report to delineate any additional information that could highlight the company’s prospects moving forward. 
  
 ## **Process**
 
  1. **Data exploration** (see *DataExploration.R* file)
      - Reviewed mean character count, word count, sentences
      - Reviewed top common words as unigram & bigram
  2. **Data cleaning** (see *DataExploration.R* file)
      - Tokenize words
      - Remove common stop words (e.g. the, a)
      - Remove whitespace
      - Convert all words to lowercase
      - Stemmed words
  3. **Text Mining** (see *PredictiveAnalytics.R* file)
      - Using Loughran lexicon that categorizes text into 6 sentiments (constraining, litigious, negative, positive, superfluous, uncertainty)
  4. **Predictive Analytics** (see *PredictiveAnalytics.R* file)
      - Using logistic regression & decision tree
  5. **Results** (see *PredictiveAnalytics.R* file)
      - Best model: decision tree with 3 predictor variables (sentiment, word, word count) yielded 73% accuracy on train & 47% accuracy on test

See *10K Text Mining for Predictive Analysis.pdf* for summary of each stage process.
