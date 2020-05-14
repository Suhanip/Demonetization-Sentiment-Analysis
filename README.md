# Demonetization-Sentiment-Analysis
Sentiment Analysis on Demonetization Twitter Data from kaggle.

This repository contains:
1.Twitter Dataset on Demonetization from kaggle.
2.The R code on text analysis of the dataset along with the hypothesis testing for confirming that our conclusion is TRUE or FALSE.
3.The Documentation for the above analysis.

The steps involved in Text-Analysis are the following:
1.	The sentiment score of each tweet was analyzed using ‘bag of words’ method.
2.	The sentences is broken into words which are treated as token.
3.	The text of each tweet is pulled out,cleaned and preprocessed.
4.	Lexicon is a list of words tagged positive and negative.Each tweet was parsed and scanned to the presence of words matching the words     from the list of the positive and negative words from the Lexicon.
5.	The score of the tweet was the net score of all its words.This process is repeated for all 14940 tweets.
6.	Based on the score,the tweet was identified as positive(>0),negative(<0) or neutral(=0).
7.  Lastly the hypothesis Testing is done on the Sentiment Score in order to confirm that our conclusion is TRUE or FALSE.


