##https://apps.twitter.com/app

# we need these 2 packages
library("twitteR")
library("httr")
library("ROAuth")

key = "dPf0GNX6WZg9AN50W8mYTxw60"

secret = "xn46cHIOFaQDp5TTDhR7jPj5YT64ks4VC9vMrBRiuZxjPRu4Xv"

secrettk = "F9PDbqUgvrUECyJPNmjDKu6RvUBFyIXEf7un1x55TQti6"

mytoken = "861271732480618496-k1lR51w18VsCnSVi7o1Tha3oV3mMHUh"


#?setup_twitter_oauth


# keep this order of arguments
setup_twitter_oauth(key, secret, mytoken, secrettk)

tweets = searchTwitter("#BacardiHousePartySessions", n=1000)

class(tweets)
length(tweets)

library("tm")
library("wordcloud")

tweettext = sapply(tweets, function(x) x$getText()) # extracting the text, tweets is the object your scraped from 

tweettext=lapply(tweettext, function(x) iconv(x, "latin1", "ASCII", sub="")) # conversion to standard characters # tweettext=lapply(tweettext, function(x) gsub("htt.*",' ',x)) # this step is optional, it removes urls and anything after it
tweettext=lapply(tweettext, function(x) gsub("#",'',x)) # removing the hashes
tweettext=unlist(tweettext) # getting the character vector
tweettext=tolower(tweettext) # putting words to lower cases

wordcloud(tweettext, min.freq=4, scale=c(5,1), 
          random.color=F, max.word=45, random.order=F) # plotting the wordcloud with a simple vector






########################################################3




# changing to a tdm
tdm <- TermDocumentMatrix(tweettext)

# a DocumentTermMatrix is a very useful tool when it comes to text mining
# it structures the text in a matrix where each term is organized in a column
# each row is a document and the number represents the counts of that term

udemytdm

# frequent terms
findFreqTerms(udemytdm, lowfreq=11)

?findFreqTerms

# associations
findAssocs(udemytdm, 'android', 0.60)

# Lets get a dendrogram to see related terms

# Remove sparse (infrequently used) terms from the term-document matrix
udemy2tdm <-removeSparseTerms(udemytdm, sparse=0.9)

# Lets scale the data
udemy2tdmscale <- scale(udemy2tdm)

# distance matrix
udemydist <- dist(udemy2tdmscale, method = "euclidean")

# hierarchical clustering
udemyfit <- hclust(udemydist)

# Visualize the result
plot(udemyfit)

# to calculate a certain number of groups
cutree(udemyfit, k=6)

# we can even color the 6 groups and plot them
rect.hclust(udemyfit, k=6, border="red")


