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
key <- "key"
#api secret (example below is not a real key)
secret <- "secret"
# create token named "twitter_token"
access_token <- "access_token"
access_secret <- "access_secret" 
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
corona_tweets <- search_tweets(q = "#covid19 OR #jobs", n=10000, include_rts=FALSE, lang="en")
#Remove retweets from the extracted tweets
corona_tweets_organic <- corona_tweets[corona_tweets$is_retweet==FALSE, ]
# Remove replies
corona_tweets_organic <- subset(corona_tweets_organic,is.na(corona_tweets_organic$reply_to_status_id))
# Arranging tweets in descending order based on number of retweets, - indicates desc
corona_tweets_organic <- corona_tweets_organic %>% arrange(-retweet_count)
corona_tweets_organic[1,5]

# Keeping only the retweets
corona_retweets <- corona_tweets[corona_tweets$is_retweet==TRUE,]
# Keeping only the replies
corona_replies <- subset(corona_tweets, !is.na(corona_tweets$reply_to_status_id))

# Creating a data frame
data_refined <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2856, 192, 120)
)
# Extract top 10 hashtags used along with corona or covid-19
select(corona_tweets, hashtags) %>% 
  unnest() %>% 
  mutate(hashtags = tolower(hashtags)) %>% 
  count(hashtags, sort=TRUE) %>% 
  filter(hashtags != "rstats") %>% 
  top_n(10)
