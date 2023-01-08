#insert the necessary libraries
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
#library(dplyr)
library(data.table)
library(rJava)
library(RWeka)
library(SnowballC)


#locate the file and create r data frame
#create textual corpus
wines <- Corpus(DirSource("E:/wines"))

#cleaning the brands with text mining(tm) package
wines <- tm_map(wines, stripWhitespace)
wines <- tm_map(wines, tolower)
wines <- tm_map(wines, removeWords,stopwords("english"))
wines <- tm_map(wines, removePunctuation)
wines <- tm_map(wines, PlainTextDocument)
wines <- tm_map(wines , removeWords, c("carmex"))

#create tdmpromote and termfreqpromote  
tokPromote <- function(x) NGramTokenizer(x, Weka_control(min=2, max=3))
tdmPromote <- TermDocumentMatrix(wines,control = list(tokenize = tokPromote))
termFreqPromote <- rowSums(as.matrix(tdmPromote))
termFreqVectorPromote <- as.list(termFreqPromote)

#calculating the frequency of the respective terms
wines2 <- data.frame(unlist(termFreqVectorPromote), stringsAsFactors = FALSE)
setDT(wines2 , keep.rowname =TRUE)
setnames(wines2 , 1, "term")
setnames(wines2 , 2, "freq")
wines3 <- head(arrange(brands2,desc(freq)), n =30 )
wines3$npstype <- "brands"


#create wordcloud with the wordcloud package
wordcloud(words = wines2$term, freq = wines2$freq, min.freq = 1,
          max.words=250, random.order=FALSE, rot.per=0.35,
          
          colors=brewer.pal(3, "Set1"))
