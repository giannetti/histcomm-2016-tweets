library("twitteR")
library("wordcloud")
library("tm")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

# Grab tweets under the hashtag
histcomm <- searchTwitter("#histcomm", n=1500)

#save text
histcomm_text <- sapply(histcomm, function(x) x$getText())

#create corpus
histcomm_text_corpus <- Corpus(VectorSource(histcomm_text))
histcomm_text_corpus <- tm_map(histcomm_text_corpus,
                               content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                               mc.cores=1
)
histcomm_text_corpus <- tm_map(histcomm_text_corpus, content_transformer(tolower), mc.cores=1)
histcomm_text_corpus <- tm_map(histcomm_text_corpus, removePunctuation, mc.cores=1)
histcomm_text_corpus <- tm_map(histcomm_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
# some cleanup
histcomm_text_corpus <- tm_map(histcomm_text_corpus,removeWords,c("histcomm","amp"))

# Create wordcloud
library(RColorBrewer)
set.seed(123)
wordcloud(histcomm_text_corpus, random.order=FALSE, colors=brewer.pal(6, "Dark2"),min.freq=10, scale=c(4,.5),rot.per=.15,max.words=200)

# Top tweets histogram
library(ggplot2)
library(ggthemes)
library(dplyr)
library(plyr)

number_ticks <- function(n) {function(limits) pretty (limits, n)}
dat <- read.csv("histcomm.csv",header=TRUE,sep=",")

dat <- dat %>%
  group_by(Screen.Name) %>%
  summarize(number_of_tweets = n())

df_top <- ddply(dat, .(number_of_tweets), 
                function(x) head(x[order(x$number_of_tweets, decreasing = TRUE),], 2))

ggplot(df_top, aes(x=reorder(Screen.Name, -number_of_tweets),y=number_of_tweets,group=1)) +
  geom_bar(stat="identity") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
