library(dplyr)
library(tidytext)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)
library(tm)
library(wordcloud2)

dataset <- read.csv('Tweets.csv')

str(dataset)
#After loading the dataset, we should transform tweets' text into words for finding the most frequent words in each sentiment. 
#Before that, text was converted factor to character.

dataset$text <- as.character(dataset$text)
tidy_dataset <- dataset %>%
  unnest_tokens(word, text)

#Counts of each sentiments
summary(dataset$airline_sentiment)

#As we can see from summary of airline sentiment, negative tweets are higher than others. 
#Thats mean, people tend to tweet more in negative issues.


#Visualization of whether the sentiment of the tweets was positive, neutral, or negative for each airlines

ggplot(dataset, aes(x = airline_sentiment, fill = airline_sentiment)) +
  geom_bar() +
  facet_grid(. ~ airline) +
  theme(axis.text.x = element_text(angle=65, vjust=0.5),
        plot.margin = unit(c(2,0,2,0), "cm"))

#United, US Airways, American substantially get negative reactions.

# The most Frequent tweets location 

remove <- c("to", "the","i", "a", "you", "for", "on", "and", "is", "are", "am", 
                    "my", "in", "it", "me", "of", "was", "your", "so","with", "at", "just", "this",
                    "http", "t.co", "have", "that", "be", "from", "will", "we", "an", "can","place",",")

location <- tidy_dataset %>% filter(!(word %in% remove))%>%
  count(tweet_location, sort = TRUE) %>% 
  rename(freq = n)%>%filter(tweet_location != "")

# people in big cities like newyork,losangelos, chicago,boston,washington are more likely to tweet. 

wordcloud2(location,size = 0.28, ellipticity = 0.9,color = 'random-dark',backgroundColor = 'white')
#########The Most Frequent Words in Positive Sentiment################
positive <- tidy_dataset %>% 
  filter(airline_sentiment == "positive") 

#The most 21 frequent words contains too much prepositional phrase. It would be better with removing these phrases.
list <- c("to", "the","i", "a", "you", "for", "on", "and", "is", "are", "am", 
          "my", "in", "it", "me", "of", "was", "your", "so","with", "at", "just", "this",
          "http", "t.co", "have", "that", "be", "from", "will", "we", "an", "can")

positive <- positive %>%filter(!(word %in% list))


positive <- positive %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

head(positive, 21)

wordcloud2(positive,size = 0.28, ellipticity = 0.9,color = 'random-dark',backgroundColor = 'white')

#This visualization shows us 'thanks', ' thank', 'great', 'love', 'good', 'best', 'awesome' words are 
#some of the frequently used positive words in tweets.

#####. The Most Frequent Words in Negative Sentiment#########
negative <- tidy_dataset %>% 
  filter(airline_sentiment == "negative") 

negative <- negative %>% filter(!(word %in% list))

negative <- negative %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)


wordcloud2(negative,size = 0.28, ellipticity = 0.9,color = 'random-dark',backgroundColor = 'white')

#'not', 'no', 'cancelled', 'help', 'but', 'customer', 'time' words are some of the frequently used negative words in tweets.

#Intersection of positive and negative words
intersect(negative$word, positive$word)
#Top words which included in only positive sentiment
setdiff(positive$word, negative$word)
#Top words which included in only negative sentiment
setdiff(negative$word, positive$word)

######. What is the negative reason ?#########
dataset %>%
  filter(negativereason != "") %>%
  ggplot(aes(x = negativereason)) + 
  geom_bar(fill = "tomato") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
#This visualization shows us people mostly complain about customer service. After that, late flight is the another reason of complaints.


#########. The Most Frequent Words in Neutral Sentiment##########

neutral <- tidy_dataset %>% 
  filter(airline_sentiment == "neutral") 

neutral <- neutral %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

neutral <- neutral %>%
  filter(!(word %in% list))

wordcloud2(neutral,size = 0.28, ellipticity = 0.9,color = 'random-dark',backgroundColor = 'white')


########. How many words for each sentiment ?#############

totals <- tidy_dataset %>%
  # Count by tweet id to find the word totals for tweet
  count(tweet_id) %>%
  # Rename the new column
  rename(total_words = n) 


totals <- dataset %>%
  inner_join(totals, by = "tweet_id") %>%
  select(tweet_id, total_words, airline_sentiment) %>%
  arrange(desc(total_words))


ggplot(totals, aes(x = airline_sentiment , y = total_words, fill = airline_sentiment)) +
  geom_col() 

# people more tweets longer text while encountering negative situations.

#retweet analysis 
d <-dataset %>% group_by(airline,airline_sentiment)%>% summarise(retweet_count = sum(retweet_count))
ggplot(d,aes(x = airline,y=retweet_count,fill = airline_sentiment )) +
geom_col() +
  facet_grid(. ~ airline_sentiment)+
  theme(axis.text.x = element_text(angle=90, vjust=0.6))

# To sum up, people are more likely to retweet on negative tweets and big airline companies. 


