# Business problem definition
#-------------------------------------------------------------------------------
# Conducting a feelings analysis of the comments from twitter


# Packages and Authentication
#-------------------------------------------------------------------------------
# install.packages("twitteR")
# install.packages("httr")
# https://cran.r-project.org/src/contrib/Archive/Rstem/
# https://cran.r-project.org/src/contrib/Archive/sentiment/
# install.packages("RColorBrewer")
# install.packages("wordcloud")
# install.packages("src/Rstem_0.4-1.tar.gz", sep = "", repos = NULL, type = "source")
# install.packages("src/sentiment_0.2.tar.gz",sep = "", repos = NULL, type = "source")
# install.packages("ggplot2")
# install.packages("tm")
# install.packages("SnowballC")

library(twitteR)
library(httr)
source('02.src/utils.R')
library(SnowballC)
library(tm)
library(Rstem)
library(sentiment)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)

# Twitter authentication keys
key <- "zrMYVqwOMi4miW6T63qMz2TBr"
secret <- "wnq5f7ZQ3KQ6iGdMYDaRthtRquZmuUS92iTsZ0RzxGGoJrD1WJ"
token <- "1245549012196999168-Om6FlAQAeOzHf9YXuuJ48ncuCGCaxl"
tokensecret <- "lecikook0QR0uayysLpKTW5dnqpBuH0gGRJ0Cfdhqxff1"
setup_twitter_oauth(key, secret, token, tokensecret)


# Connection and acquisition of tweets
#-------------------------------------------------------------------------------
userTimeline("seu_username")

# Capturing tweets
theme <- "machine learning"
qtd_tweets <- 290
language <- "pt"
tweetdata = searchTwitter(theme, n = qtd_tweets, lang = language)
head(tweetdata)


# Exploratory data analysis
#-------------------------------------------------------------------------------

# cleaning, organization, and transformation ----------------------------------- 
options(warn=-1)
tweetlist <- sapply(tweetdata, function(x) x$getText())
tweetlist <- iconv(tweetlist, to = "utf-8", sub="")
tweetlist <- cleanTweets(tweetlist)
tweetcorpus <- Corpus(VectorSource(tweetlist))
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <- tm_map(tweetcorpus, content_transformer(tolower))
tweetcorpus <- tm_map(tweetcorpus, function(x)removeWords(x, stopwords()))

# Converting the Corpus object to plain text 
termo_por_documento = as.matrix(TermDocumentMatrix(tweetcorpus), 
                                control = list(stopwords = c(stopwords("portuguese"))))

# Wordcloud, association between words and dendograma -------------------------- adicionar gráfico de barras
# Generation a word cloud
pal2 <- brewer.pal(8,"Dark2")
png(paste("03.reports/Nuvem de palavras para", theme ,".png"), width = 500, height = 500, res = 72)
wordcloud(tweetcorpus, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)
dev.off()

# Creating the dendrogram (checking how the words are grouped)
tweettdm <- TermDocumentMatrix(tweetcorpus)             # Convert the text object to matrix format
findFreqTerms(tweettdm, lowfreq = 11)                   # Finding the words that appear most often
findAssocs(tweettdm, theme, 0.60)                       # Seeking associations
tweet2tdm <- removeSparseTerms(tweettdm, sparse = 0.9)  # Removing sparse terms (not used often)
tweet2tdmscale <- scale(tweet2tdm)                      # Creating scale in the data
tweetdist <- dist(tweet2tdmscale, method = "euclidean") # Distance Matrix
tweetfit <- hclust(tweetdist)                           # clusterization
plot(tweetfit)                                          # ploting dendrogram
cutree(tweetfit, k = 4)                                 # checking the groups
rect.hclust(tweetfit, k = 3, border = "red")            # Viewing the word groups in the dendrogram


# Data Munging
#-------------------------------------------------------------------------------
options(warn=-1)
tweetdata = sapply(tweetdata, function(x) x$getText())         # Getting the text
tweetdata = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetdata) # Removing special characters
tweetdata = gsub("@\\w+", "", tweetdata)                       # Removing @
tweetdata = gsub("[[:punct:]]", "", tweetdata)                 # Removing punctuation
tweetdata = gsub("[[:digit:]]", "", tweetdata)                 # Removing digits
tweetdata = gsub("http\\w+", "", tweetdata)                    # Removing html links
tweetdata = gsub("[ \t]{2,}", "", tweetdata)                   # Removing unnecessary spaces
tweetdata = gsub("^\\s+|\\s+$", "", tweetdata)                 # Removing unnecessary spaces
tweetdata = sapply(tweetdata, try.error)                       # Lower case
tweetdata = tweetdata[!is.na(tweetdata)]                       # Removing NAs
names(tweetdata) = NULL


# Using Naive Bayes Classifier for sentiment analysis
#-------------------------------------------------------------------------------
# Sorting by emotion
class_emo = classify_emotion(tweetdata, algorithm = "bayes", prior = 1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "Neutro"

# Sorting by polarity
class_pol = classify_polarity(tweetdata, algorithm = "bayes")
polarity = class_pol[,4]

# Generating a data frame with the results
sent_df = data.frame(text = tweetdata, emotion = emotion,
                     polarity = polarity, stringsAsFactors = FALSE)

# # Ordering the data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels = names(sort(table(emotion), 
                                                                decreasing=TRUE))))

# Plotting the results and saving in png format
#-------------------------------------------------------------------------------
# Emotions found
png(paste("03.reports/Gráfico das Emoções para expressão", theme ,".png"), width = 500, height = 500, res = 72)
ggplot(sent_df, aes(x = emotion)) +
  geom_bar(aes(y = ..count.., fill = emotion)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Categorias de Sentimento", y = "Número de Tweets")+
  ggtitle(paste("Gráfico das Emoções para a expressão:", theme))
dev.off()

# Polarity
png(paste("03.reports/Gráfico de Polaridade para expressão", theme ,".png"), width = 500, height = 500, res = 72)
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x = "Categorias", y = "Número de Tweets")
dev.off()
