# The below code is to establish twitter api connection
# Run the code only once, once the connection is established, methods 
# such as search_tweets, select cna be called

library(rtweet)
library(dplyr)
library(tidyr)
library(twitteR)
library(tidytext)

# whatever name you assigned to your created app
appname <- "CovidDistress"
#api key (example below is not a real key)
key <- "ogRXvxribQAEt9tJKQ1rEd0c0"
#api secret (example below is not a real key)
secret <- "HlvVRoFg73JJcpcGjYxUWBagWratEIrdagPCeaiToWTKa15vCO"
# create token named "twitter_token"
access_token <- "15914217-8YYyRRAxRBL0Vu9Y0tAjVFfPvdJdYByfmsiVpLEoD"
access_secret <- "oeXIkYHBTQpGRxZCKI4q67UN3L8PuJfwb0su6EOkIk22f" 

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret,
  set_renv = TRUE)


# The below code is to fetch the tweets containing word covid or COVID-19, by
# Extract tweet with word corona
# Here q takes query argument, we are extracting tweets with the words covid OR COVID-19, 1000 indicates top 1000 tweets
corona_tweets <- search_tweets(q = "#covid19 OR #coronavirus", n=100000, include_rts=FALSE, lang="en", retryonratelimit = TRUE)

corona_tweets <- readRDS("D:/OneDrive/Documents/Documents/OvGU/SoSe 21/Data Science with R/Project/Data-Science-with-R-2021/process-nbk/data/tweets2021.rds")

#Remove retweets from the extracted tweets
corona_tweets_organic <- corona_tweets[corona_tweets$is_retweet==FALSE, ]

# Remove replies
corona_tweets_organic <- subset(corona_tweets_organic,is.na(corona_tweets_organic$reply_to_status_id))

# Arranging tweets in descending order based on number of retweets, - indicates desc
corona_tweets_organic <- corona_tweets_organic %>% arrange(-retweet_count)
corona_tweets_organic[1,5]

# Extract top 10 hashtags used along with corona or covid-19
select(corona_tweets, hashtags) %>% 
  unnest() %>% 
  mutate(hashtags = tolower(hashtags)) %>% 
  count(hashtags, sort=TRUE) %>% 

  filter(hashtags != "coronavirus") %>% 

  top_n(10)

coronaDF <- corona_tweets['text']

Wordcloud


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("stopwords")
library("ggplot2")
library("syuzhet")

word_cloud <- function(language2letters="EN"){
  
  data = data[3:nrow(data),]
  
  docs <- Corpus(VectorSource(as.character(unique(coronaDF$text))))
  
  #remove special characters and numbers
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  
  #remove most of the not significant words
  docs <- tm_map(docs, removeWords, stopwords(tolower(language2letters)))
  
  #other not significant words
  to_exclude <- c()
  
  docs <- tm_map(docs, removeWords, to_exclude)
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  set.seed(42)
  
  wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=400, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
  
}

word_cloud_en = word_cloud("EN")

#Here tweets are categorized as positive or negative and then what are the words that have contributed most for their sentiment. From this chart, we can analyze, what are the words people have used frequently to express their positive or negative feelings.

#Grabbing text data from tweets
corona.text <- sapply(coronaDF, function(x) x$getText())

#Clean text data - remove emoticons and other symbols
corona.text <- iconv(coronaDF$text,'UTF-8','ASCII')

#Remove twitter mentions
corona.text <- gsub("@[[:alpha:]]*","", corona.text)

# Removing blank spaces, punctuation, links, extra spaces, special characters and other unwanted things.
corona.text = gsub("[:blank:]", "", corona.text)
corona.text = gsub("[[:punct:]]", "", corona.text)
corona.text = gsub("[:cntrl:]", "", corona.text)
corona.text = gsub("[[:digit:]]", "", corona.text)
corona.text = gsub("[:blank:]", "", corona.text)
corona.text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ",  corona.text)
corona.text = gsub("@\\w+", "", corona.text)
corona.text = gsub("http\\w+", "", corona.text)


corona.corpus <- Corpus(VectorSource(corona.text))

doc.term.matrix <- DocumentTermMatrix(corona.corpus,control = list(removePunctuation=T,
                                                                   stopwords = c("corona","covid19","pandemic","virus", "covid", "corona virus","covid19pandemic","stay home", "stay safe",'http','https',stopwords('en')),
                                                                   removeNumbers = T,
                                                                   tolower = T))
#Plot the graph
corona.doc.term.matrix <- tidy(doc.term.matrix)
ap_sentiments <- corona.doc.term.matrix %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments %>%
  dplyr::count(sentiment, term, wt = count) %>%
  filter(n >= 1) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot2::ggplot(aes(term, n, fill = sentiment)) +
  scale_fill_discrete(name = "Sentiment", labels = c("Negative", "Positive")) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Words") +
  ylab("Contribution to Sentiment")

#Get nrc emotions
sentiment <- get_nrc_sentiment(corona.text)
sentiment_nonemotions <- get_sentiment(corona.text)

sentiment_scores <- data.frame(colSums(sentiment[,]))
names(sentiment_scores) <- "Score"
sentiment_scores <- cbind("sentiment"=rownames(sentiment_scores),sentiment_scores)
rownames(sentiment_scores) <- NULL

#References
library(ggplot2)
ggplot(data = sentiment_scores, aes(x=sentiment, y=Score)) + geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiments") + ylab("scores") + ggtitle("Sentiments of people behind the tweets on COVID19")

# https://www.rdocumentation.org/packages/syuzhet/versions/1.0.6/topics/get_nrc_sentiment
# https://levelup.gitconnected.com/sentiment-analysis-with-twitter-hashtags-in-r-af02655f2113
# https://www.rdocumentation.org/packages/twitteR/versions/1.1.9

write.csv(coronaDF,file="D:/OneDrive/Documents/Documents/OvGU/SoSe 21/Data Science with R/Project/Data-Science-with-R-2021/process-nbk/data/Covid-tweets.csv")
?write.csv
