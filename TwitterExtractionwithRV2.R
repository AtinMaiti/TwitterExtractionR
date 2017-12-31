##https://apps.twitter.com/app

key <- "vYKJtmEaonRNkH42Hc9TyeIYN"

secret <- "nYqAuqWpXkvsbEDvU8GQ029OlcRjzpx5xI5bY5qEiwpINahTTl"

secrettk <- "UQChGugAym3FSFQjfyioCgXY5aBQhgPOkJQIjfyhgiXFp"

mytoken <- "861271732480618496-jU05ARZuD3jSpjMfgUMMtKXMfMDpQB"

library("twitteR")
library("httr")

setup_twitter_oauth(key,secret,mytoken,secrettk)

udemytweets = searchTwitter("#udemy", n=1000)

#iconv(udemytweets$text, "ASCII", "UTF-8", sub="")

class(udemytweets)
length(udemytweets)

library("tm")

udemylist <- sapply(udemytweets, function(x) x$getText()) # initiating a function
# in depth info about the apply family and functions in the course "R Level 1"

udemycorpus <- Corpus(VectorSource(udemylist)) # use the corpus function
# a corpus is the text body consisting of all the text including the meta info

udemycorpus <- tm_map(udemycorpus, tolower) # putting text to lower case
udemycorpus <- tm_map(udemycorpus, removePunctuation) # remove punct.

udemycorpus <- tm_map(udemycorpus, function(x) removeWords(x,stopwords())) # remove stopwords (meaningless words)

# there is a link to a stop word list in the link lecture
udemycorpus
# Lets see which other transformations tm offers
?getTransformations

# to trasform to plain text which wordcloud can use
udemycorpus <- tm_map(udemycorpus, PlainTextDocument)

library("wordcloud")

?wordcloud

?simple_triplet_matrix

wordcloud(udemycorpus, min.freq=2, scale=c(3,1), random.color=F, max.word=45, random.order=F)

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


