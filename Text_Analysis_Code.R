library("qdap")
library("wordcloud")
library("tm")
library("readr")
library("ggplot2")

#loading the data
demo = read.csv("demonetization-tweets.csv")

#store the text column
tweet <- demo$text

#clean the tweets using gsub

#remove control characters
tweet = gsub("[[:cntrl:]]", " ", tweet)
#remove retweets
tweet = gsub("(RT|via)((?:\\b\\W*@\\W+)+)", " ", tweet,ignore.case = T)
#remove "@ user"
tweet = gsub('@\\w+',' ',tweet)
#remove punctuations
tweet = gsub("[[:punct:]]"," ",tweet)
#remove digits
tweet = gsub("[[:digit:]]", " ", tweet)
#remove links
tweet = gsub("http[s]?\\w+", " ", tweet)
#remove unwamted space
tweet = gsub("[ \t]{2,}"," ",tweet)
tweet = gsub("^\\s+|\\s+$"," ",tweet)
#remove NAs
tweet = tweet[!is.na(tweet)]
#remove other insignificant symbols
tweet = gsub("^ ", "", tweet)
tweet = gsub(" $","",tweet)
tweet = gsub("[^[:alnum:] ]"," ",tweet)
#convert the text to lower case
tweet = tolower(tweet)

#Store the text in the corpus
t_corpus = VCorpus(VectorSource(tweet))
#remove stopwords
t_corpus = tm_map(t_corpus,removeWords,c(stopwords("en"),"amp","demonetiztion","obqrhlnsl","uodwxdpmmg"))
#remove whitespace
t_corpus = tm_map(t_corpus,stripWhitespace)
#create a Term Document Matrix
t_tdm = TermDocumentMatrix(t_corpus)
#convert into matrix
t_matrix = as.matrix(t_tdm)

#calculate word frequency
word_freq = rowSums(t_matrix)
word_freq = sort(word_freq,decreasing = TRUE)

#plotting the frequency of words
barplot(word_freq[1:10],col = "blue3",las = 2,main = "Frequency plot of words")

word_freq2 = data.frame(term = names(word_freq),num = word_freq)

#creating word cloud
wordcloud(word_freq2$term,word_freq2$num,min.freq = 150,max.words = 100,random.order = 'F',rot.per = 0.1,colors = brewer.pal(8,"Dark2"),scale = c(3,0.3),random.color = T)

#Word clustering with dendrogram
#To limit the number of words in TDM
t_tdm_s = removeSparseTerms(t_tdm,sparse = 0.95)
t_matrix_s = as.matrix(t_tdm_s)
t_df = as.data.frame(t_matrix_s)
#To compute the diff b/w each row of the matrix
distance = dist(t_df)
#To perform cluster analysis
hc = hclust(distance)
#plotting the dendrogram
plot(hc)

#Word Associations
#To find the correlation of the word 'india' with other words
associations = findAssocs(t_tdm,"india",0.2)
#Putting the words and their correlations in a table
associations_df = list_vect2df(associations)[,2:3]
#Plotting the correlation
ggplot(associations_df, aes(y = associations_df[,1])) + geom_point( aes(x = associations_df[,2]), color = "firebrick3", data = associations_df, size = 1.5) + ggtitle("Correlation of the word 'India' with other words") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Correlation value") + ylab("words")

#calculating sentiment score
score.sentiment <- function(sentences, positive_words, negative_words, .progress='none')
{
require(plyr)   
require(stringr)  
#list+array+apply=laply
scores = laply(sentences, function(sentence, positive_words, negative_words) {
#Split into words
wordlist = str_split(sentence, '\\s+')
#Unlist and have as separate words to work on
word = unlist(wordlist)
#Compare the words in the file with the list of positive and negative words
negativematches = match(word, negative_words)
positivematches = match(word, positive_words)
#The match() function returns only the position of the matched terms
positivematches <- !is.na(positivematches)
negativematches <- !is.na(negativematches)
#Net score is the differrence between positive and negative matches
score = sum(positivematches)-sum(negativematches)
return(score)
}, positive_words, negative_words, .progress=.progress )
scores.df = data.frame(score=scores, text=sentences)
return(scores.df)
}

#import positive and negative words list
setwd("Opinion/")
positive = scan('positive_word.txt',what = 'character',comment.char = ';')
negative = scan('negative_word.txt',what = 'character',comment.char = ';')
#Retieving the scores
t_analysis = score.sentiment(tweet,positive,negative, .progress = "none")
#Creating the component that coresponds to the sentiments based on scores
t_analysis$sentiment[t_analysis$score == 0] = "Neutral"
t_analysis$sentiment[t_analysis$score > 0] = "Positive"
t_analysis$sentiment[t_analysis$score < 0] = "Negative"
t_analysis$sentiment = factor(t_analysis$sentiment)

#Make a table score and count
scoretable = table(t_analysis$score)
score = t_analysis$score

#Calculate the basic statiscal measures of central tendency
mean = mean(score)
median = median(score)
mean
median
summary(t_analysis$sentiment)

#Plot the bar with suitable titles
ggplot(data = t_analysis,aes(x = score,fill = sentiment))+geom_bar()+labs(title = "Sentiment Acore Bar Plot",x = "Sentiment Score",y = "Tweet Count")+scale_x_continuous(breaks = seq(-6,6,1))+scale_y_continuous(breaks = seq(0,4000,500))+scale_fill_manual(guide_legend("Sentiment"),values = c("firebrick3","dodgerblue4","seagreen4"))+theme(plot.title = element_text(hjust = 0.5))

#hypothesis testing
sd(score)
size(score)
length(score)
mu0 = 0
z = (mean - mu0)/(sd(score)/sqrt(length(score)))
z
alpha = 0.05
z.alpha = qnorm(1-alpha)
z.alpha

