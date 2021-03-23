# code to use IBM DB2 in memory R studio connection
# certain functions below require the ibmdbr package
library(ibmdbR)
dsn_driver <- c("BLUDB")
dsn_database <- c("BLUDB")
dsn_hostname <- c("dashdb-txn-sbox-yp-dal09-10.services.dal.bluemix.net")
dsn_port <- "50000"
dsn_protocol <- "TCPIP"
dsn_uid <- c("mfs65449")
dsn_pwd <- c("q2ctx4jj9qd5^9gm")
conn_path <- paste(dsn_driver,  
                   ";DATABASE=",dsn_database,
                   ";HOSTNAME=",dsn_hostname,
                   ";PORT=",dsn_port,
                   ";PROTOCOL=",dsn_protocol,
                   ";UID=",dsn_uid,
                   ";PWD=",dsn_pwd,sep="")
mycon <- idaConnect(conn_path) 
idaInit(mycon)

#Read the data from database
SENTIMENT <- idaQuery("SELECT * from GOP_Debate")

# read data from local drive if not using direct in database analytics
#SENTIMENT<- read.csv("C:/Users/Yissa/OneDrive/data_UMGC/650/Sentiment/First_GOP_Debate.csv")

#Check the row counts
nrow(SENTIMENT)

# check metadata
ls()
dim(SENTIMENT) 

# 
idadf(mycon, "SELECT count(*) FROM  GOP_Debate")
#List the database tables
idaShowTables()
#List the database tables that contain GOP in the name
idaShowTables(matchStr='GOP')
#To check if the table exists
idaExistTable('GOP_Debate')

# sentiment counts and exploration

#number of tweets per sentiment
table(SENTIMENT$SENTIMENT)
barplot(table(SENTIMENT$SENTIMENT), main = 'Sentiment Per Tweet')
idadf(mycon, "SELECT SENTIMENT, 
      count(1) COUNT 
      FROM GOP_Debate 
      GROUP BY SENTIMENT")

#number of tweets per candidate
table(SENTIMENT$CANDIDATE)


#number of tweets per sentiment by candidate
table(SENTIMENT$CANDIDATE, SENTIMENT$SENTIMENT)

#Number of tweets by CANDIDATE per subject
table(SENTIMENT$SUBJECT_MATTER, SENTIMENT$CANDIDATE)

# sentiment by subject matter
table(SENTIMENT$SUBJECT_MATTER, SENTIMENT$SENTIMENT)


# plotting results
#number of tweets per sentiment
barplot(table(SENTIMENT$SENTIMENT), names.arg = c('Negative',  'Neutral', 'Positive' ), ylab = 'Number of Tweets')
t <-table(SENTIMENT$SENTIMENT)
t
# percentages
barplot(t / sum(t), main = 'Percentage of Tweets by Sentiment')

#number of tweets per candidate
barplot(table(SENTIMENT$CANDIDATE), ylab= 'Number of Tweets', main = 'Tweets per Candidate')
# as a %
barplot(table(SENTIMENT$CANDIDATE)/sum(table(SENTIMENT$CANDIDATE)),ylab= 'Percentage of Tweets', main = 'Percentage of Tweets for each Candidate')


#Pie charts
#Reset the margin
par(mar=c(1,1,1,1))
#Use default colors tweets per airline
pie (table(SENTIMENT$CANDIDATE))

#load the plyr package
library("plyr")
#List the most common issues
reasonCounts<-na.omit(plyr::count(SENTIMENT$SUBJECT_MATTER))
reasonCounts<-reasonCounts[order(reasonCounts$freq, decreasing=TRUE), ]
reasonCounts

# Using ggplot2 package
#  frequency plot
library(ggplot2)
wf <- data.frame(reasonCounts)
p <- ggplot(wf, aes(wf$x, wf$freq))
p <- p + geom_bar(stat="identity") 
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


# now plot for positive tweets
# subject garnering positive feedback
x <-SENTIMENT [SENTIMENT$SENTIMENT=="Positive",]
# which candidates succeeded in which topics
table(x$SUBJECT_MATTER, x$CANDIDATE)

# plot topics for positive tweets
reasonCounts<-na.omit(plyr::count(x$SUBJECT_MATTER))
reasonCounts<-reasonCounts[order(reasonCounts$freq, decreasing=TRUE), ]
reasonCounts

wf <- data.frame(reasonCounts)
p <- ggplot(wf, aes(wf$x, wf$freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p



#Number of retweets per topic
ddply(SENTIMENT, ~ SUBJECT_MATTER, summarize, numRetweets = sum(RETWEET_COUNT, na.rm = TRUE))


#number of posts per day
posts<-as.Date(SENTIMENT$TWEET_CREATED, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y"),optional = FALSE)
table(posts)
#day with the maximum number of posts
table(posts)[which.max(table(posts))]

#number of posts per day by sentiment plot
library (dplyr)
drs <- idadf(mycon, "SELECT TWEET_CREATED, SENTIMENT
             FROM GOP_Debate")
drs$TWEET_CREATED<- as.Date(drs$TWEET_CREATED, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y"),optional = FALSE)
# Calculate and plot number of tweets per day by airline
ByDateBySent <- drs %>% group_by(SENTIMENT,TWEET_CREATED) %>% dplyr::summarise(count = n())
ByDateBySentPlot = ggplot() + geom_line(data=ByDateBySent, aes(x=TWEET_CREATED, y=count, group =SENTIMENT , color=SENTIMENT)) 
ByDateBySentPlot


dfa<-SENTIMENT[ , c('CANDIDATE', 'SENTIMENT',  'SUBJECT_MATTER',  'RETWEET_COUNT', 'USER_TIMEZONE')]
dfa$CANDIDATE<-as.factor(dfa$CANDIDATE)
dfa$SENTIMENT<-as.factor(dfa$SENTIMENT)
dfa$SUBJECT_MATTER<-as.factor(dfa$SUBJECT_MATTER)
dfa$USER_TIMEZONE<-as.factor(dfa$USER_TIMEZONE)

dfa$RETWEET_COUNT<-cut(dfa$RETWEET_COUNT, breaks=c(0, 1, 2, Inf), right=F, labels=c("0", "1",  "2+"))
#detach(package:tm, unload=TRUE)
rules<-apriori(dfa)
inspect(rules)

# display other measures
interestMeasure(rules, c("support", "chiSquare", "confidence", "conviction", "cosine", "coverage", "leverage", "lift", "oddsRatio"), SENTIMENT)
#  Plot the rules
#install.packages("arulesViz")
library("arulesViz")
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))
plot(rules, method = "grouped")
#plot(rules, method="matrix", measure=c("lift", "confidence"))


#Text Mining
#Load the packages in memory
library("tm")
library("wordcloud")
library ("SnowballC")

#Load the tweets with positive sentiment into the data frame positive
positive <- idadf(mycon, "SELECT 
                  TEXT FROM GOP_Debate 
                  WHERE GOP_Debate.SENTIMENT='Positive' ")



docs<-VectorSource(positive$TEXT)
docs<-Corpus(docs)
inspect(docs[[1]])
inspect(docs[[2]])
inspect(docs[[20]])

#Strip the white space
docs <- tm_map(docs, stripWhitespace)

#Remove the URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))

#Remove non ASCII character
removeInvalid<-function(x) gsub("[^\x01-\x7F]", "", x)
docs <- tm_map(docs, content_transformer(removeInvalid))

#Remove punctuation
docs <- tm_map(docs, removePunctuation)

#remove the numbers
docs <- tm_map(docs, removeNumbers)
# make the case uniform
docs <- tm_map(docs, tolower)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "@")   #Remove @
docs <- tm_map(docs, toSpace, "/")   #Remove /
docs <- tm_map(docs, toSpace, "\\|") #Remove |


#Remove the stop word
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("SMART"))


docs <- tm_map(docs, stemDocument)


#Remove the white space introduced during the pre-processing
docs <- tm_map(docs, stripWhitespace)
#create the doc term matrix
dtm <- DocumentTermMatrix(docs)

m <- as.matrix(dtm)   #Convert dtm to a matrix
dim(m)                # Display number of terms and number of documents
View(m[1:10, 1:10])   # Preview the first 10 rows and the first 10 columns in m

#find the terms that appear at least 50 times
findFreqTerms(dtm, lowfreq=50)
#find the terms asosciated with good and great with correlation at least 0.15
findAssocs(dtm, c("great", "good"), corlimit=0.15)

dtms <- removeSparseTerms(dtm, 0.6) # Prepare the data (max 60% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, min.freq=35, max.words=100, rot.per=0.2, scale=c(0.9, 0.9), colors=dark2) 

# now try with most important words using tfidf
dtm1 <- dtm %>% weightTfIdf(normalize=T) ## normalized TF-IDF

m <- as.matrix(dtm1)   #Convert dtm to a matrix
dim(m)                # Display number of terms and number of documents
View(m[1:10, 1:10])   # Preview the first 10 rows and the first 10 columns in m

freq=colSums(m)

#Show 30 most important terms based on the TFIDF weighting 
high.freq=tail(sort(freq),n=30)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 
#show weights for top thirty words
hfp.df

ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity",fill = "#FF6666") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("TFIDF Term frequencies") +
  theme(plot.title = element_text(size = 18, face = "bold",hjust = .5))


#find the terms that appear at least 50 times
findFreqTerms(dtm1, lowfreq=50)
#find the terms asosciated with good and great with correlation at least 0.15
findAssocs(dtm1, c("great", "good"), corlimit=0.15)

dtms1 <- removeSparseTerms(dtm1, 0.6) # Prepare the data (max 60% empty space)   
freq1 <- colSums(as.matrix(dtm1)) # Find word frequencies   
wordcloud(names(freq1), freq1, min.freq=35, max.words=100, rot.per=0.2, scale=c(0.9, 0.9), colors=dark2) 

#Network of terms
library ("igraph")
tdm<-TermDocumentMatrix(docs)              # Term document matrix
tdm <- removeSparseTerms(tdm, 0.96)        # Remove sparse terms
termDocMatrix <- as.matrix(tdm)            # Convert tdm to matrix

termDocMatrix[termDocMatrix>=1] <- 1       # Set non-zero entries to 1 (1=term present, 0=term absent)
termMatrix <- termDocMatrix %*% t(termDocMatrix)   
View (termMatrix)

g <- graph.adjacency(termMatrix, weighted=T, mode="undirected")
g <- simplify(g)                 # Remove the self-relationships
# V(g) is a graph vertex
V(g)$label <- V(g)$name          # Label each vertex with a term
V(g)$degree <- degree(g)
set.seed(3952)

plot(g, layout=layout.fruchterman.reingold(g), vertex.color="cyan")
plot(g, layout=layout_with_gem(g), vertex.color="pink")
plot(g, layout=layout_as_star(g), vertex.color="yellow", vertex.shape="square")
plot(g, layout=layout_on_sphere(g), vertex.color="magenta")
plot(g, layout=layout_randomly(g), vertex.size=10)
plot(g, layout=layout_in_circle(g), vertex.color="pink", vertex.size=35)
plot(g, layout=layout_nicely(g), vertex.color="plum", vertex.size=25)
plot(g, layout=layout_on_grid(g), vertex.color="green", vertex.size=20)
plot(g, layout=layout_as_tree(g), vertex.color="brown", vertex.size=20)

# now try for negatives for contrast
#Load the tweets
neg <- idadf(mycon, "SELECT 
                  TEXT FROM GOP_Debate 
                  WHERE GOP_Debate.SENTIMENT='Negative' ")



docs<-VectorSource(neg$TEXT)
docs<-Corpus(docs)
inspect(docs[[1]])
inspect(docs[[2]])
inspect(docs[[20]])

#Strip the white space
docs <- tm_map(docs, stripWhitespace)

#Remove the URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))

#Remove non ASCII character
removeInvalid<-function(x) gsub("[^\x01-\x7F]", "", x)
docs <- tm_map(docs, content_transformer(removeInvalid))


#Remove punctuation
docs <- tm_map(docs, removePunctuation)

#remove the numbers
docs <- tm_map(docs, removeNumbers)
# make the case uniform
docs <- tm_map(docs, tolower)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "@")   #Remove @
docs <- tm_map(docs, toSpace, "/")   #Remove /
docs <- tm_map(docs, toSpace, "\\|") #Remove |


#Remove the stop word
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("SMART"))


docs <- tm_map(docs, stemDocument)


#Remove the white space introduced during the pre-processing
docs <- tm_map(docs, stripWhitespace)
#create the doc term matrix
dtm_neg <- DocumentTermMatrix(docs)

m <- as.matrix(dtm_neg)   #Convert dtm to a matrix
dim(m)                # Display number of terms and number of documents
View(m[1:10, 1:10])   # Preview the first 10 rows and the first 10 columns in m


#find the terms that appear at least 50 times
findFreqTerms(dtm_neg, lowfreq=50)
#find the terms asosciated  with correlation at least 0.15
findAssocs(dtm_neg, c("awful", "bad"), corlimit=0.15)

dtms <- removeSparseTerms(dtm_neg, 0.6) # Prepare the data (max 60% empty space)   
freq <- colSums(as.matrix(dtm_neg)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, min.freq=35, max.words=100, rot.per=0.2, scale=c(0.9, 0.9), colors=dark2) 

# now try with most important words using tfidf
dtm2 <- dtm_neg %>% weightTfIdf(normalize=T) ## normalized TF-IDF

m <- as.matrix(dtm2)   #Convert dtm to a matrix
dim(m)                # Display number of terms and number of documents
View(m[1:10, 1:10])   # Preview the first 10 rows and the first 10 columns in m

freq=colSums(m)


#Show 30 most important terms based on the TFIDF weighting 
high.freq=tail(sort(freq),n=30)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 
#show weights for top thirty words
hfp.df

# plot those frequencies
ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity",fill = "#FF6666") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("TFIDF Term frequencies") +
  theme(plot.title = element_text(size = 18, face = "bold",hjust = .5))



# set up the negative word networks

tdm1<-TermDocumentMatrix(docs)              # Term document matrix
tdm1 <- removeSparseTerms(tdm1, 0.96)        # Remove sparse terms
termDocMatrix1 <- as.matrix(tdm1)            # Convert tdm to matrix

termDocMatrix1[termDocMatrix1>=1] <- 1       # Set non-zero entries to 1 (1=term present, 0=term absent)
termMatrix1 <- termDocMatrix1 %*% t(termDocMatrix1)   
View (termMatrix1)

g <- graph.adjacency(termMatrix1, weighted=T, mode="undirected")
g <- simplify(g)                 # Remove the self-relationships
# V(g) is a graph vertex
V(g)$label <- V(g)$name          # Label each vertex with a term
V(g)$degree <- degree(g)
set.seed(3952)

plot(g, layout=layout.fruchterman.reingold(g), vertex.color="cyan")
plot(g, layout=layout_with_gem(g), vertex.color="pink")
plot(g, layout=layout_as_star(g), vertex.color="yellow", vertex.shape="square")
plot(g, layout=layout_on_sphere(g), vertex.color="magenta")
plot(g, layout=layout_randomly(g), vertex.size=10)
plot(g, layout=layout_in_circle(g), vertex.color="pink", vertex.size=35)
plot(g, layout=layout_nicely(g), vertex.color="plum", vertex.size=25)
plot(g, layout=layout_on_grid(g), vertex.color="green", vertex.size=20)
plot(g, layout=layout_as_tree(g), vertex.color="brown", vertex.size=20)

# Terms Cluster dendogram for positive tweets
library("cluster")

dtms <- removeSparseTerms(dtm, 0.98)    #Remove sparse terms
d <- dist(t(dtms), method="euclidian")  #Build the dissimilarity matrix
fit <- hclust(d=d, method="ward.D2")
plot(fit, hang=-1)

fit <- hclust(d=d, method="complete")   # for a different look 
fit  
plot(fit, hang=-1)  
groups <- cutree(fit, k=8)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=8, border="red") # draw dendogram with red borders around the  clusters   

# for neg tweets
dtms <- removeSparseTerms(dtm_neg, 0.98)    #Remove sparse terms
d <- dist(t(dtms), method="euclidian")  #Build the dissimilarity matrix
fit <- hclust(d=d, method="ward.D2")
plot(fit, hang=-1)

# kmeans
k <- 3
kfit <- kmeans(d, k)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
kfit$cluster
#top  words per cluster
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(kfit$centers[i,], decreasing=T)
  cat(names(s)[1:5], "\n")
}  


