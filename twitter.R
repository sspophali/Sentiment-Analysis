# Twitter sentiment analysis

# Load Requried Packages
library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")
library("ggplot2")
library(plotly)

# Authonitical keys
consumer_key = 'BQBLByqXHSpF6pHuzpefdrV1v'
consumer_secret = 'nWF1Agwpk7giQ0dwlNSVdsILAZPvD4ti1AYuIdYEAV3R2OLWvX'
access_token = '745687164-Ui6s64o0H4rABZ44cJ8tufKSrX8R1abtpvMSoBZn'
access_secret = 'vKzLDYctKPwbwYFaACzVBD3priflxg2UasCUaFG0HaRXp'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets = userTimeline("narendramodi",n=2000)
tweets_hash = searchTwitter("#IndiaStrikesPakistan",n=1000)
length(tweets_hash)
n.tweet = length(tweets)

#cleaning the tweets
tweets.df = twListToDF(tweets) 
head(tweets.df)

tweets.df2 = gsub("http.*","",tweets.df$text)
tweets.df2 = gsub("https.*","",tweets.df2)
tweets.df2 = gsub("#.*","",tweets.df2)
tweets.df2 = gsub("@.*","",tweets.df2)
tweets.df2 = gsub("\n\n","",tweets.df2)
head(tweets.df2)


word.df = as.vector(tweets.df2)
emotion.df = get_nrc_sentiment(word.df)
emotion.df2 = cbind(tweets.df2, emotion.df) 
head(emotion.df2)

emotion = data.frame("anger"=sum(emotion.df2$anger),
                      "anticipation"=sum(emotion.df2$anticipation),
                      "disgust"=sum(emotion.df2$disgust),
                      "fear"=sum(emotion.df2$fear),
                      "joy"=sum(emotion.df2$joy),
                      "sadness"=sum(emotion.df2$sadness),
                      "surprise"=sum(emotion.df2$surprise),
                      "trust"=sum(emotion.df2$trust),
                      "negative"=sum(emotion.df2$negative),
                      "positive"=sum(emotion.df2$positive))
em=c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
s=c(sum(emotion.df2$anger),
    sum(emotion.df2$anticipation),
    sum(emotion.df2$disgust),
    sum(emotion.df2$fear),
    sum(emotion.df2$joy),
    sum(emotion.df2$sadness),
    sum(emotion.df2$surprise),
    sum(emotion.df2$trust),
    sum(emotion.df2$negative),
    sum(emotion.df2$positive))
emotion_1 <- data.frame("emotion"=em,
                        "score"=s)

#plotting graph
p = plot_ly(emotion_1, x = ~emotion, y = ~score, type = 'bar', 
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>%
  layout(title = "Tweets of Narendra Modi ",
         xaxis = list(title = "Emotions"),
         yaxis = list(title = "Score"))



