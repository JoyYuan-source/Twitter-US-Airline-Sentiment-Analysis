library(dplyr)
df <- read.csv('Tweets.csv')
set.seed(1234)
nn = 0.1
data = df
sub <- sample(1:nrow(data), round(nrow(data)*nn))
dataset <- data[sub,]
dim(dataset)
dataset$text <- as.character(dataset$text)
dataset <- select(dataset,c('text','airline_sentiment'))
#dataset$airline_sentiment <- as.numeric(dataset$airline_sentiment)
dataset[which(dataset$airline_sentiment =='positive'),2]='positive'
dataset[which(dataset$airline_sentiment =='neutral'),2]='neutral'
dataset[which(dataset$airline_sentiment =='negative'),2]='negative'
library(caret)

set.seed(101) #any random number
indexes <- createDataPartition(dataset$airline_sentiment , times = 1,
                               p = 0.8, list = FALSE)

train <- dataset[indexes,]
test <- dataset[-indexes,]


# Let us verify proportions of targetted customers in training and test datasets.
prop.table(table(train$airline_sentiment))
prop.table(table(test$airline_sentiment))

library(quanteda)

train.tokens <- tokens(train$text, what = "word", 
                       remove_numbers = FALSE, remove_punct = TRUE,
                       remove_symbols = TRUE, split_hyphens = TRUE)

train.tokens <- tokens_tolower(train.tokens)
train.tokens <- tokens_select(train.tokens, stopwords(), 
                              selection = "remove")

train.tokens <- tokens_wordstem(train.tokens, language = "english")
train.tokens.dfm <- dfm(train.tokens)

train.tokens.matrix <- as.matrix(train.tokens.dfm)
dim(train.tokens.matrix)


k <- round(sqrt(ndoc(train.tokens.dfm)/2))


clusterkmax <- kmeans(train.tokens.matrix, k)
split(docnames(train.tokens.dfm), clusterkmax$cluster)

plot(clusterkmax$cluster)

train.tokens.df <- cbind(text = train$text, convert(train.tokens.dfm, to="data.frame"))

#check the columnnames. some of these are numbers and will throw errors when we try to apply Machine Learning algorithms. So we fix the syntax.
names(train.tokens.df) <- make.names(names(train.tokens.df))

#using caret library, we shall create 10 random stratified samples (cross validation)
#5  fold and each 2 times
set.seed(105)
#create multifolds calls create data partition
cv.folds <- createMultiFolds(train$text, k = 5, times = 2)

#executes the training process with repeated cross validation
cv.cntrl <- trainControl(method = "repeatedcv", number = 5,
                         repeats = 2, index = cv.folds)

#we can use many modeling techniques.
# we will use a single decision tree based method rpart
#but you can easily change the method to randomforest etc.
#tuning parameter tries 3 different confugurations for rpart (hyper parameter tuning)
rpart.cv.1 <- train(text~ ., data = train.tokens.df, method = "rpart", 
                    trControl = cv.cntrl, tuneLength = 3)



#Result
rpart.cv.1


