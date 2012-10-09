# tweet_analysis.R 
# A R script analyzing the twitter posts from the Cyber Summmit 2012 
#
# Copyright 2012 MPK Analytics, Inc.
#
# This program is free software: you can redistribute it and/or modify it under the terms 
# of the GNU General Public License as published by the Free Software Foundation, either 
# version 3 of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
# PARTICULAR PURPOSE. See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along with this 
# program. If not, see <http://www.gnu.org/licenses/>.

# MPK Analytics, Inc. (www.mpkanalytics.com)
# Suite #300, 8507-112 Street, Edmonton, AB T6G 2L7, Canada
# Email: info@mpkanalytics.com

# Load the necessary libraries
require(XML)       # Tools for parsing and generating XML
require(wordcloud) # Wordcloud tools
require(tm)        # Text mining tools

# The scraping code below searches and downloads twitter posts with the hash tag 
# #cybersummit. It appears that one can only retrieve tweets going back about one week 
# using the twitter search functionality. The file 'tweets.Rdata' contains a data frame 
# with tweets that were scraped from twitter following the Cyber Summit 2012. Running the 
# scaping code will overwrite the tweets.Rdata file and populate it with more recent 
# tweets having the #cybersumit which may not be what you want. Just saying.

# Name of Rdata file with the Cyber Summit tweet data
file.name <- "./tweets.Rdata"

# Folder where figures should be saved
path.figures <- './figures/'

# Start of tweet scraping code
if (!file.exists(file.name)) {
  cat(file.name,'does not exists - scraping tweets from web\n')

  # Scraping preliminaries 
  twitter.search <- "#cybersummit"
  QUERY <- URLencode(twitter.search)
  tweets <- NULL
  tweet.count <- 0
  page <- 1
  read.more <- TRUE

  # Read one page at a time of search results
  while (read.more) {
   
    # construct Twitter search URL
    URL <- paste('http://search.twitter.com/search.atom?q=',QUERY,'&rpp=100&page=', page, sep='')
  
    # Fetch remote URL and parse
    cat("Retrieving page ", page,"\n")
    XML <- htmlTreeParse(URL, useInternal=TRUE)
  
    # Extract list of "entry" nodes
    entry     <- getNodeSet(XML, "//entry")   
    read.more <- (length(entry) > 0)
    if (read.more) {
       for (i in 1:length(entry)) {
        subdoc <- xmlDoc(entry[[i]])   # put entry in separate object to manipulate       
        published  <- unlist(xpathApply(subdoc, "//published",    xmlValue))
               
        # see Converting time zones in R: tips, tricks and pitfalls
        # http://blog.revolutionanalytics.com/2009/06/converting-time-zones.html
        # Example "published" string:  "2011-05-19T00:57:41Z"
        # Let's remove "T" and "Z" from string
        published  <- gsub("Z"," ", gsub("T"," ",published) )
              
        # Convert from GMT to central time
        time.gmt   <- as.POSIXct(published,"GMT")
        local.time <- format(time.gmt, tz="America/Edmonton")
        title      <- unlist(xpathApply(subdoc, "//title",        xmlValue))
        author     <- unlist(xpathApply(subdoc, "//author/name",  xmlValue))
        #tweet <-  paste(local.time, " @", author, ":  ", title, sep="")
        entry.frame <- data.frame(title, author, local.time, stringsAsFactors=FALSE)
        tweet.count <- tweet.count + 1
        rownames(entry.frame) <- tweet.count
        tweets <- rbind(tweets, entry.frame)
      }
      page <- page + 1
      read.more <- (page <= 15)   # Seems to be 15 page limit
    }
    # Save retrieved tweet data in an Rdata file
    save(tweets, file=file.name)
  } 

# if the tweets.Rdata file already is present then just load it
} else {
  cat(file.name,'exists - loading tweets from file\n')
  load(file.name)
}
# End of tweet scraping code

# Subset the tweet data to only include tweets posted during the conference days 
tweets <- tweets[tweets$local.time >= as.POSIXlt("2012-10-01 00:00:00") & 
                 tweets$local.time <= as.POSIXlt("2012-10-03 23:59:59"),]
cat('A total of', dim(tweets)[1],'tweets were posted during the Cybersummit 2012\n')

##
# Barplot of top twitteratis 
##
n <- 20 # Top n twitteratis

# Count and sort the twitter frequency for different users
authors <- table(tweets$author)
top.twiteratis <- head(sort(authors, decreasing=TRUE),n)
 
# Parse twitter names (extract only alias, drop real name)
twitter.names <- strsplit(names(top.twiteratis),"(", fixe=TRUE)
twitter.alias <- NULL
for (i in seq(length(twitter.names))) 
  twitter.alias <- c(twitter.alias, unlist(twitter.names[i])[1])
names(top.twiteratis) <- twitter.alias
 
# Generate barplot of top n twitteratias 
fn <- paste(path.figures,"twitteratis.png", sep='')
png(fn, width=480*2, height=480*2, pointsize=12*2)
  par(mar=c(7, 4, 2, 0.5))
  barplot(top.twiteratis, las=2, ylab="Number of tweets", ylim=c(0, 140), col='blue', 
          border=NA, main=paste('Top',n,'twitteratis at the Cyber Summit 2012'))
dev.off()
cat('Generated figure:', fn,'\n')

##
# Time series of the number of tweets per hour
##
d.vec <- NULL
f.vec <- NULL
day <- sprintf("%02d", seq(3))
hrs <- sprintf("%02d", seq(0,24))

# Loop over the days (3) and daily hrs (24) and summarize the number of tweets each hour
for (j in seq(3)) {
  for (i in seq(2,25)) {
  
    # Define the start and en time of the current time slice
    start.time <- paste("2012-10-",day[j]," ",hrs[i-1],":00:00", sep='')
    end.time <- paste("2012-10-",day[j]," ",hrs[i],":00:00", sep='')
      
    # Subset the tweets in the time interval and tally them
    slice <- tweets[tweets$local.time >= as.POSIXlt(start.time),]
    slice <- slice[slice$local.time < as.POSIXlt(end.time),]
    n.tweets <- dim(slice)[1]
  
    #cat(start.time, "->", end.time,", n=",n.tweets,"\n", sep='') # Very verbose but useful for debugging
    
    # Assemble hourly results
    d.vec <- c(d.vec, start.time)
    f.vec <- c(f.vec, n.tweets)
  }
}

# Generate time series of number of tweets per hour
fn <- paste(path.figures,"time_series.png", sep='')
png(fn, width=480*2, height=480*2, pointsize=16)
  plot(f.vec, type='s', col='blue', lwd=3, bty='n', xaxt='n', ylab='Number of tweets', 
              xlab="Time", ylim=c(0,70), 
              main="Time series of tweets during the Cyber Summit 2012", cex=3)
  axis(1, at=seq(1,73,3), labels=c(seq(0,21,3), seq(0,21,3), seq(0,23,3), 0))
  axis(3, at=c(1,25,49,73), labels=FALSE)
  mtext("Oct 1", side=3, at=12)
  mtext("Oct 2", side=3, at=36)
  mtext("Oct 3", side=3, at=60)
  abline(v=c(25, 49))

  # Define function for plotting and labelling time slices indicating summit events
  time.slot <- function(day, start.time, end.time, name, color) {
    x1 <- (day-1)*24+start.time+1 
    x2 <- (day-1)*24+end.time+1
    polygon(c(x1, x2, x2, x1), c(-10, -10, 80, 80), col=color, border=color)
    text(x1, 70, name, pos=2, srt=90, cex=.9)
  }

  # Add summit events to time series
  color <- rgb(0,0,1,.4)
  time.slot(day=1, start.time=19, end.time=20.5, name="Opening Plenary", color=color)
  time.slot(day=2, start.time=9, end.time=10.25, name="Keynote: Andrew Hessel", color=color)
  time.slot(day=2, start.time=13.5, end.time=14.75, name="Keynote: Hilary Mason", color=color)
  time.slot(day=2, start.time=16.5, end.time=17.5, name="PechaKucha", color=color)
  time.slot(day=2, start.time=19, end.time=22, name="Jay and John Talk Show", color=color) 
  time.slot(day=3, start.time=13.5, end.time=14.75, name="Closing Plenary", color=color)
dev.off()
cat('Generated figure:', fn,'\n')

##
# Bar chart of top hash tags
##

# Use text mining tools to clean up the tweets, extract and tally the hash tags 
words.corpus <- Corpus(DataframeSource(data.frame(as.character(tweets$title))))
words.corpus <- tm_map(words.corpus, tolower)
words.tdm <- TermDocumentMatrix(words.corpus)
words.m <- as.matrix(words.tdm)
words.v <- sort(rowSums(words.m), decreasing=TRUE)
words.d <- data.frame(word = names(words.v),freq=words.v)

w <- NULL
n <- NULL
for(i in seq(length(words.d$word))) {
  if (!is.na(pmatch("#", words.d$word[i]))) w <- c(w, i)
}
hashtags <- words.d$freq[w]
names(hashtags) <- words.d$word[w]

# Fix "rogue" hashtags
m <- names(hashtags) == "#cybersummit."
hashtags[names(hashtags) == "#cybersummit"] <- hashtags[names(hashtags) == "#cybersummit"] + hashtags[m]
hashtags <- hashtags[!m]
hashtags[names(hashtags) == "#cybersummit"] <- hashtags[names(hashtags) == "#cybersummit"] + hashtags[7]
hashtags[1] <- hashtags[1] + 7
# hashtags <- hashtags[1:6, 8:length(hashtags)] # ERROR!
hashtags <- c(hashtags[1:3], hashtags[5:22]) # Remove the #cybersummit" hash tag (typo)
# hashtags <- c(hashtags[1:10], hashtags[12:21]) # Remove the #bigdata, hash tag (typo)

# Generate bar plot of 20 hash tags 
fn <- paste(path.figures,"hashtags1.png", sep='')
png(fn, width=480*2, height=480*2, pointsize=12*2)
  par(mar=c(9, 4, 2, 0.5))
  barplot(head(hashtags,11), las=2, ylab="Frequency", ylim=c(0, 600), col='blue', 
                             border=NA, main="Top 20 hashtags used during the Cyber Summit 2012")
dev.off()
cat('Generated figure:', fn,'\n')

# Generate another bar plot of the top 10 hashtags but without the #cybersummit hashtag
fn <- paste(path.figures,"hashtags2.png", sep='')
png(fn, width=480*2, height=480*2, pointsize=12*2)
  par(mar=c(10, 4, 2, 0.5))
  barplot(hashtags[2:11], las=2, ylab="Frequency", ylim=c(0, 70), col='blue', border=NA, 
                          main="Top 10 hashtags used during the Cyber Summit 2012")
dev.off()
cat('Generated figure:', fn,'\n')

##
# Word clouds of tweets
##

# Sanitize the tweets, ie. remove punctuations, caps, stop words, etc.
words.corpus <- Corpus(DataframeSource(data.frame(as.character(tweets$title))))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, tolower)
words.corpus <- tm_map(words.corpus, function(x) removeWords(x, stopwords("english")))
words.tdm <- TermDocumentMatrix(words.corpus)
words.m <- as.matrix(words.tdm)

# Tally frequency of the tweet words
words.v <- sort(rowSums(words.m), decreasing=TRUE)
words.d <- data.frame(word = names(words.v),freq=words.v)
table(words.d$freq)

# Generate word cloud. To maximize resolution we generate a pdf of the word cloud.
color.map <- brewer.pal(11,"Spectral")
fn <- paste(path.figures,"wordcloud.pdf", sep='')
pdf(fn, width=7*2, height=7*2)
  wordcloud(words.d$word,words.d$freq, scale=c(10,2),min.freq=5, max.words=Inf, 
            random.order=FALSE, rot.per=.25, colors=color.map, vfont=c("script","plain"), 
            main="Tweet word cloud from the Cyber Summit 2012")
dev.off()
cat('Generated figure:', fn,'\n')



