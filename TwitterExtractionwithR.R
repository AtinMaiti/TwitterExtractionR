#This is the code we use in this section. Just copy and paste it into R and follow along with the videos.

#####################################

# Social Media Mining

# Twitter is a great source for sentiment data and social media mining
# furthermore it is quite easy to get significant amounts of data
# to be able to scrape data from Twitter you need a standard Twitter account
# and you need to update it to a developer account
# note that Twitter limits the amount of searches you can perform (15min: 15 scrapes)

# package twitteR
library("twitteR")

# all this info is obtained for the Twitter developer account
key = "22LdoMzzR7KJgp2LxWH6XDrfT"

secret = "ZpKjG6xHMTgGPHLJpx9kunYl3ZgBpUUUJ7HRpE90BNU7JJ74Jr"

# set a working directory for the whole process - you need to download a few files 
# and R needs to know where to look for that stuff
setwd("C:/Users/maiti/Desktop/TwitterExtractionR")

# this is crucial step - at least for windows users
# if you are on Linux or Mac you might skip this step, Win needs the certificate collection
# Cacert.pem is a collection of certificates
download.file(url="http://curl.haxx.se/ca/cacert.pem", 
              destfile="C:/Users/maiti/Desktop/TwitterExtractionR/cacert.pem",
              method="auto")
# hint: download.file is really handy when it comes to downloading material from the web
# the dest file is the location on your computer, here it is my working directory
# and url points to the place where you want to get the file from

# we are entering the whole Twitter API info and call the whole object authenticate
authenticate <-  OAuthFactory$new(consumerKey=key,
                                  consumerSecret=secret,
                                  requestURL='https://api.twitter.com/oauth/request_token',
                                  accessURL='https://api.twitter.com/oauth/access_token',
                                  authURL='https://api.twitter.com/oauth/authorize')

# this will get you to a Twitter Site - obtain the PIN
# the whole process is meant to provide the signature for your Twitter usage
authenticate$handshake(cainfo="C:/Users/maiti/Desktop/TwitterExtractionR/cacert.pem")

# insert the PIN from Twitter
9573167

save(authenticate, file="twitter authentication.Rdata")

registerTwitterOAuth(authenticate)

# Lets start with the Twitter scraping

library("twitteR")

# we need to specify the cainfo to avoid a SSL cert error - this is for Windows machines
# Lets check the latest tweets of Udemy
userTimeline("Udemy", cainfo="cacert.pem")

# searchTwitter is the main function of the package

?searchTwitter

# arguments: since and until are for time specifications
# lang: for languge specification
# geocode: for location specification

# we are now scraping 1k tweekts for Udemy, and we als specify our certificate
udemytweets = searchTwitter("#Udemy", n=1000, cainfo="cacert.pem")

# as you can see, scraping that data is quite time consuming - your machine limits the
# the efficiency and speed of your mining 
# if you are plan to scrape a lot in the future 64bit systems and high RAM is desireable

class(udemytweets)
length(udemytweets)
head(udemytweets)

library("tm")

 udemylist <- sapply(udemytweets, function(x) x$getText()) # initiating a function
# in depth info about the apply family and functions in the course "R Level 1"

udemycorpus <- Corpus(VectorSource(udemylist)) # use the corpus function
# a corpus is the text body consisting of all the text including the meta info

udemycorpus <- tm_map(udemycorpus, tolower) # putting text to lower case

udemycorpus <- tm_map(udemycorpus, removePunctuation) # remove punct.

udemycorpus <- tm_map(udemycorpus, function(x)removeWords(x,stopwords())) # remove stopwords (meaningless words)

# there is a link to a stop word list in the link lecture

# Lets see which other transformations tm offers
?getTransformations

# to trasform to plain text which wordcloud can use
udemycorpus <- tm_map(udemycorpus, PlainTextDocument)

library("wordcloud")

? wordcloud

wordcloud(udemycorpus, min.freq=4, scale=c(5,1), 
          random.color=F, max.word=45, random.order=F)

# changing to a tdm
udemytdm <- TermDocumentMatrix(udemycorpus)

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