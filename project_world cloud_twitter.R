#install.packages("devtools")
#install.packages("rjson")
#install.packages("bit64")
#install.packages("httr")
#install.packages("plyr")
#install.packages("twitteR")
#install.packages("SnowballC") 

# sessionInfo()
api_key <- "ECDGwCvfYAIcnqVSAh3kqLX7t"
api_secret <-"IIWLVhZelcBztSACsVwb6bpuh4oMfYBVBJUPb1rHl6I6PeEwZ0" 
access_token <- "1430056338-ScHro2FJINckZtHnl41RCtRH5jmo5TvlbRh0tRv"
access_token_secret <- "9FSaCZrN97DVmaNvYg8v4mPni2gQlI614MdsKAGnHcnXw"

library(devtools)
library(plyr)
library(twitteR)
library(httr)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets=searchTwitter('#Iphone',n=1000,lang = "en")
head(tweets)
class(tweets)

df = do.call("rbind", lapply(tweets, as.data.frame))

#install.packages("tm")
library(tm)
text=df$text
review_source= VectorSource(text)
mycorpus = Corpus(review_source)

x=as.character(mycorpus)
str(mycorpus)


mycorpus1=tm_map(mycorpus,stripWhitespace)
mycorpus2=tm_map(mycorpus1,tolower)
mycorpus3=tm_map(mycorpus2,removeWords,stopwords("english"))
mycorpus4=tm_map(mycorpus3,removePunctuation)
mycorpus5=tm_map(mycorpus4,removeNumbers)
mycorpus6=tm_map(mycorpus5,PlainTextDocument)


#converting the corpus to Term Matrix 
data_dtml=DocumentTermMatrix(mycorpus6)
data_dtml2=as.matrix(data_dtml)

frequency =colSums(data_dtml2)
frequency= sort(frequency,decreasing=TRUE)
head(frequency,n=200)

#install.packages("wordcloud",dependencies = TRUE)
library("wordcloud")

trunc_freq = frequency[2:100]
frequency[1]
words = names(frequency)


#install.packages("stringr", dependencies = TRUE)
#install.packages("RColorBrewer")
library(stringr)
library(wordcloud)

wordcloud(words[1:50],frequency[1:50],random.order = FALSE,colors = rainbow(50) )

