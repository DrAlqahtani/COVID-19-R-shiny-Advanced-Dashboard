
#GLOBAL.R
## Allocate memory
options(java.parameters = "-Xmx10g")

## clear console
cat("\014")
require(wordcloud)
## clear global variables
rm(list=ls())
# library(twitteR)
# library(ROAuth)
# consumer_key <- "7zwAr0NLrvTIVrQOeUWZgWmGp"
# consumer_secret <- "UQwNwc2ohRqzljcSRN90Ru4kytMYLT7iNPs426lhTiSrLQxDfs"
# access_token <- "392535828-aHVb3XFYVyRTQSbb5k3YqlHP5K4PmgYbQKlHvCGe"
# access_secret <- "MjyNn9rWebetckEiBoP7GTWkQtaj6lKtH9eVMmZ9E9JcG"
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

## list of packages required
library(ggplot2)
library(plotly)
library(gapminder)
list.of.packages = c("visNetwork","stringr","shinymaterial","grid","igraph","sna","Matrix","SparseM","reshape2","pwr")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(visNetwork)
library(shinymaterial)
#nstall_github("hadley/devtools")
#library(devtools)
#install_github("geoffjentry/twitteR")

###
# Methods of retrieving tweets

# TODO:
# 1) metadate structures of different sources are different
#    need to make them adopt a same structure
#    refer to https://dev.twitter.com/docs/platform-objects/tweets


GetTweetsBySearch <- function(term, n = 1500) {
  # Get tweets by searching Twitter API
  # 
  # Args: 
  #   term: search term (e.g., #education)
  #   n: max number of tweets
  #
  # Returns:
  #   Data frame containing tweets
  
  EnsurePackage("twitteR")
  EnsurePackage("RCurl")
  EnsurePackage("bitops")
  EnsurePackage("rjson")
  
  # get tweets, and put in a df
  results <- searchTwitter(term, n)
  df <- do.call("rbind", lapply(results, as.data.frame))
  
  # rename metadata
  names.twitteR <- c("screenName", "created") # change from
  names.api <- c("screen_name", "created_at") # change to
  for(name in names.twitteR) {
    names(df)[which(names(df)==name)] <- names.api[which(names.twitteR==name)]
  }
  df$from_user <- df$screen_name
  
  return(df)
}

GetTweetsFromCSV <- function(file) {
  # Get tweets from a csv file
  # 
  # Args: 
  #   file: file name, with path
  #
  # Returns:
  #   Data frame containing tweets
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  # may need addtional handling here
  return(df)
}

GetTweetsFromGoogleDrive <- function(key, gid = 82) {
  # Get tweets from Google Spreadsheet
  # For how to archive tweets in Google Spreadsheet, read:
  # http://mashe.hawksey.info/2013/02/twitter-archive-tagsv5/
  #
  # Args:
  #   key: file key
  #   gid: grid id of archive sheet
  #
  # Returns:
  #   Data frame containing tweets
  
  EnsurePackage("RCurl")
  
  url <- paste(sep="", 'https://docs.google.com/spreadsheet/pub?key=', key, 
               '&single=true&gid=', gid, '&output=csv')
  conn <- textConnection(getURL(url, ssl.verifypeer = FALSE))
  df <- read.csv(conn, stringsAsFactors = FALSE)
  close(conn)
  
  # formatting
  df$created_at <- strptime(df$time, "%d/%m/%Y %H:%M:%S")
  df$geo_coordinates[df$geo_coordinates == ""] <- NA
  df$screen_name <- df$from_user
  
  return(df)
}
####
# Utilities for this package

EnsurePackage <- function(x) {
  # EnsurePackage(x) - Installs and loads a package if necessary
  # Args:
  #   x: name of package
  
  x <- as.character(x)
  if (!require(x, character.only=TRUE)) {
    install.packages(pkgs=x, repos="http://cran.r-project.org")
    require(x, character.only=TRUE)
  }
}

TrimAt <- function(x) {
  # remove @ from text
  
  sub('@', '', x)
}

TrimHead <- function(x) {
  # remove starting @, .@, RT @, MT @, etc.
  
  sub('^(.*)?@', '', x)
}

TrimUsers <- function(x) {
  # remove users, i.e. "@user", in a tweet
  
  str_replace_all(x, '(@[[:alnum:]_]*)', '')
}

TrimHashtags <- function(x) {
  # remove hashtags, i.e. "#tag", in a tweet
  
  str_replace_all(x, '(#[[:alnum:]_]*)', '')
}

TrimUrls <- function(x) {
  # remove urls in a tweet
  
  str_replace_all(x, 'http[^[:blank:]]+', '')
}

TrimOddChar <- function(x) {
  # remove odd charactors
  iconv(x, to = 'UTF-8')
}

CosineSimilarity <- function(va, vb) {
  # Computer cosine similarity between two numeric vectors of the same length
  
  crossprod(va, vb) / sqrt(crossprod(va) * crossprod(vb))
}
#####
EnsurePackage("stringr")


RemoveOddChars <- function(df) {
  # Remove odd characters in tweets
  
  df$text <- sapply(df$text, function(x) TrimOddChar(x))
  return(df)
}

ExtractUserInfo <- function(df) {
  # For each tweet, extract information related to users
  # such as to_user, rt_user...
  EnsurePackage("stringr")
  # extract to_user
  df$reply_to <- sapply(df$text, function(tweet) 
    TrimHead(str_extract(tweet,"^((\\.)?(@[[:alnum:]_]*))")))
  # extract rt_user
  df$retweet_from <- sapply(df$text, function(tweet) 
    TrimHead(str_extract(tweet,"^[RM]T (@[[:alnum:]_]*)")))
  
  return(df)
}

ExtractUrls <- function(df) {
  # For each tweet, extract url, remove it from the tweet,
  # and put them separately in a new column
  # TODO: cannot deal with multiple urls in one tweet right now
  
  EnsurePackage("stringr")
  EnsurePackage("grid")
  
  # extracts links (quick and dirty)
  # wish to have something like http://daringfireball.net/2009/11/liberal_regex_for_matching_urls
  df$links <- sapply(df$text,function(tweet) str_extract(tweet,("http[^[:blank:]]+")))
  df$text <- sapply(df$text, function(x) TrimUrls(x))
  
  return(df)
}

PreprocessTweets <- function(df) {
  # Perform a few preprocessing tasks
  
  # removing odd characters
  #df.new <- RemoveOddChars(df)
  # extract user info and add to df
  df.new <- ExtractUserInfo(df)
  # extract urls and add to df
  df.new <- ExtractUrls(df.new)
  
  return(df.new)
}

GetTweetCountTable <- function(df, col, threshold = 0) {
  # Count tweets for each user, 
  # sort the table in a decending order,
  # and filter users who posted less than the threshold
  
  counts <- table(df[, col])
  # create an ordered data frame
  counts <- data.frame(user = unlist(dimnames(counts)),
                       count = as.numeric(counts), 
                       row.names = NULL)
  counts <- counts[with(counts, order(-count, user)), ]
  # create a subset of those who tweeted at least 5 times or more
  counts <- subset(counts, counts$count > threshold)
  return(counts)
}

GetURLCountTable <- function(df) {
  # Extract URLs from tweets and count them
  
  EnsurePackage(stringr)
  EnsurePackage(grid)
  
  # get frequencies of each link and put in rank order
  countLinks <- data.frame(url = as.character(unlist(dimnames(sort(table(df$links))))), 
                           count = sort(table(df$links)))
  rownames(countLinks) <- NULL # remove rownames
  countLinks$count <- as.integer(countLinks$count)
  
  return(countLinks)
}

AnonymizeUsers <- function(df) {
  # Anonymize users, by creating random numbers for each user
  #
  # Args:
  #   df: data frame of tweets
  #
  # Returns:
  #   new data frame with a new column containing ids
  
  # find out how many random numbers we need
  n <- length(unique(df$screenName))
  # generate a vector of random number to replace the names
  # we'll get four digits just for convenience
  randuser <- round(runif(n, 1000, 9999),0)
  # match up a random number to a username
  screenName <- unique(df$screenName)
  screenName <- sapply(screenName, as.character)
  randuser <- cbind(randuser, screenName)
  # merge the random numbers with the rest of the Twitter data
  # and match up the correct random numbers with multiple instances of the usernames
  rand.df  <-  merge(randuser, df, by="screenName")
  
  return(rand.df)
}
#####
GetTweetCountTable <- function(df, col, threshold = 0) {
  # Count tweets for each user, 
  # sort the table in a decending order,
  # and filter users who posted less than the threshold
  
  counts <- table(df[, col])
  # create an ordered data frame
  counts <- data.frame(user = unlist(dimnames(counts)),
                       count = as.numeric(counts), 
                       row.names = NULL)
  counts <- counts[with(counts, order(-count, user)), ]
  # create a subset of those who tweeted at least 5 times or more
  counts <- subset(counts, counts$count > threshold)
  return(counts)
}

GetURLCountTable <- function(df) {
  # Extract URLs from tweets and count them
  
  EnsurePackage(stringr)
  EnsurePackage(grid)
  
  # get frequencies of each link and put in rank order
  countLinks <- data.frame(url = as.character(unlist(dimnames(sort(table(df$links))))), 
                           count = sort(table(df$links)))
  rownames(countLinks) <- NULL # remove rownames
  countLinks$count <- as.integer(countLinks$count)
  
  return(countLinks)
}

AnonymizeUsers <- function(df) {
  # Anonymize users, by creating random numbers for each user
  #
  # Args:
  #   df: data frame of tweets
  #
  # Returns:
  #   new data frame with a new column containing ids
  
  # find out how many random numbers we need
  n <- length(unique(df$screenName))
  # generate a vector of random number to replace the names
  # we'll get four digits just for convenience
  randuser <- round(runif(n, 1000, 9999),0)
  # match up a random number to a username
  screenName <- unique(df$screenName)
  screenName <- sapply(screenName, as.character)
  randuser <- cbind(randuser, screenName)
  # merge the random numbers with the rest of the Twitter data
  # and match up the correct random numbers with multiple instances of the usernames
  rand.df  <-  merge(randuser, df, by="screenName")
  
  return(rand.df)
}

EnsurePackage <- function(x) {
  # EnsurePackage(x) - Installs and loads a package if necessary
  # Args:
  #   x: name of package
  
  x <- as.character(x)
  if (!require(x, character.only=TRUE)) {
    install.packages(pkgs=x, repos="http://cran.r-project.org")
    require(x, character.only=TRUE)
  }
}

CreateSNADataFrame <- function(df, from, to, linkNames) {
  # Create SNA data frame
  #
  # Args:
  #   df: data frame containing raw data
  #   from: name of "from" column
  #   to: vector of names of "to" columns
  #   linkNames: vector of link names (e.g., retweet, reply)
  #
  # Note: I start with implementing 1-1 links
  
  EnsurePackage("plyr")
  EnsurePackage("DT")
  df.sna <- data.frame(from = df[[from]], 
                       to = df[[to]], 
                       link = linkNames)
  # remove rows with NA
  df.sna <- na.omit(unique(df.sna))
  
  # merge rows with same metadata, and compute weight
  ddply(df.sna, .(from, to, link), summarise, weight=length(from))
}



ConstructCorpus <- function(textVec, 
                            toLower = TRUE, 
                            removePunctuations = TRUE, 
                            removeStopwords = TRUE, 
                            removeNumbers = FALSE, 
                            stemming = FALSE,
                            removeTags = FALSE, 
                            removeUsers = FALSE) {
  # Construct text corpus
  
  more.stopwords <- c("via", "rt", "mt", "amp")
  
  EnsurePackage("tm")
  
  # create a object
  corpus <- Corpus(VectorSource(textVec))
  
  if(toLower) corpus <- tm_map(corpus, tolower)
  
  if(removeTags) {
    corpus <- tm_map(corpus, TrimHashtags)
  }
  if(removeUsers) {
    corpus <- tm_map(corpus, TrimUsers)
  }
  
  if(removePunctuations) corpus <- tm_map(corpus, removePunctuation) 
  if(removeNumbers) corpus <- tm_map(corpus, removeNumbers)
  if(removeStopwords) corpus <- tm_map(corpus, function(x) 
    removeWords(x, append(stopwords("english"), more.stopwords)))
  
  if(stemming) {
    EnsurePackage("rJava")
    EnsurePackage("Snowball")
    corpus <- tm_map(corpus, stemDocument, language = "english")
  }
  
  
  
  return(corpus)
}

MakeWordCloud <- function(corpus) {
  # Make a word cloud
  #
  # Args:
  #   textVec: a text vector
  #
  # Returns:
  #   A word cloud created from the text vector
  
  EnsurePackage("tm")
  EnsurePackage("wordcloud")
  EnsurePackage("RColorBrewer")
  
  corpus <- tm_map(corpus, function(x) {
    removeWords(x, c("via", "rt", "mt"))
  })
  
  ap.tdm <- TermDocumentMatrix(corpus)
  ap.m <- as.matrix(ap.tdm)
  ap.v <- sort(rowSums(ap.m), decreasing=TRUE)
  ap.d <- data.frame(word = names(ap.v), freq=ap.v)
  table(ap.d$freq)
  pal2 <- brewer.pal(8, "Dark2")
  
  wordcloud(ap.d$word, ap.d$freq, 
            scale=c(8, .2), min.freq = 3, 
            max.words = Inf, random.order = FALSE, 
            rot.per = .15, colors = pal2)
}

TrainLDAModel <- function(td.mat) {
  # Train a LDA model based on a sparse term-document matrix
  # 
  # Args:
  #   td.mat.sp: a sparse term-document matrix
  #
  # Returns:
  #   A LDA model with the optimal number of topics
  
  EnsurePackage("topicmodels") # have to install libgsl0-dev before installing this package on Ubuntu
  EnsurePackage("slam")
  
  # create document term matrix and convert to data frame
  td.mat.sp <- removeSparseTerms(td.mat, sparse=0.99)
  #   td.mat.sp.df <- as.data.frame(inspect(td.mat.sp))
  # check how many words are left
  #   nrow(td.mat.sp.df)
  
  # transpose document term matrix
  td.mat.sp.t <- t(td.mat.sp)
  # summary(col_sums(td.mat.sp.t)) # check median
  
  # calculate tf-idf values
  term_tfidf <- tapply(td.mat.sp.t$v/row_sums(td.mat.sp.t)[td.mat.sp.t$i], td.mat.sp.t$j,mean) * 
    log2(nDocs(td.mat.sp.t)/col_sums(td.mat.sp.t > 0))
  # summary(term_tfidf) # check median... note value for next line...
  
  # keep only those terms that are slightly less frequent that the median
  td.mat.sp.t.tdif <- td.mat.sp.t[, term_tfidf >= as.numeric(summary(term_tfidf)[3])]
  td.mat.sp.t.tdif <- td.mat.sp.t[row_sums(td.mat.sp.t) > 0, ]
  # summary(col_sums(td.mat.sp.t.tdif)) # have a look
  
  # train a topic model for every number of topics between 2 and 50 (may take long)
  best.model <- lapply(seq(2, 50, by = 1), function(d) LDA(td.mat.sp.t.tdif, d))
  
  # a list of logLiks for each model
  best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
  
  # Find the number of topics which has the highest log likelihood
  best.model.logLik.df <- data.frame(topics=c(2:50), 
                                     LL=as.numeric(as.matrix(best.model.logLik)))
  optimal.num <- best.model.logLik.df$topics[
    which(best.model.logLik.df$LL == max(best.model.logLik.df$LL))]
  
  # plot the distribution of logliklihoods by topic
  #   ggplot(best.model.logLik.df, aes(x = topics, y = LL)) + 
  #     xlab("Number of topics") + 
  #     ylab("Log likelihood of the model") + 
  #     geom_line() + 
  #     geom_vline(xintercept=optimal.num, linetype="dotted", colour="red") + 
  #     annotate("text", x=optimal.num, y=25, label=paste("num =",optimal.num), hjust=0)
  
  # generate a LDA model with the best number of topics
  LDA(td.mat.sp.t.tdif, optimal.num)
}

ScoreSentiment <- function(sentences, .progress='none') {
  # Score sentiment of sentences
  # Ref: http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/
  
  EnsurePackage("plyr")
  EnsurePackage("stringr")
  
  #load sentiment lexicon
  pos.words = c(scan('positive-words.txt', what='character', comment.char=';'))
  neg.words = c(scan('negative-words.txt', what='character', comment.char= ';'))
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores <- laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence <- tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list <- str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words <- unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score <- sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df <- data.frame(score=scores, text=as.character(sentences))
  return(scores.df)
}

ScoreSentiment2 <- function(sentences){
  # Score sentiment through 'sentiment' package
  
  # To install sentiment, check:
  # https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment
  # install 'Rstem' and 'sentiment' from source
  # use command like: sudo R CMD INSTALL sentiment_0.2.tar.gz
  
  EnsurePackage("sentiment")
  
  class_emo <- classify_emotion(sentences, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion <- class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] <- "unknown"
  
  # classify polarity
  class_pol <- classify_polarity(sentences, algorithm="bayes")
  
  # data frame with results
  sent.df <- data.frame(text=sentences, 
                        emotion=emotion, 
                        score=as.numeric(class_pol[, 3]),
                        best_fit=class_pol[, 4],
                        stringsAsFactors=FALSE)
  
  return(sent.df)
}

ScoreSentimentViralHeat <- function(text, key = "QKzubH1Qv6n9ZU4Jw") {
  # Score sentiment with ViralHeat
  # TODO: doesn't work for now; json returns empty
  
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  
  text <- URLencode(text)
  
  # save all the spaces, then get rid of the weird characters that break the API, 
  # then convert back the URL-encoded spaces.
  text <- str_replace_all(text, "%20", " ")
  text <- str_replace_all(text, "%\\d\\d", "")
  text <- str_replace_all(text, " ", "%20")
  
  if(str_length(text) > 360) text <- substr(text, 0, 359)
  
  data <- getURL(paste("http://www.viralheat.com/api/sentiment/review.json?api_key=", key, 
                       "&text=", text, sep=""))
  
  js <- fromJSON(data, asText=TRUE)
  
  mood <- js$prob
  
  if (js$mood == "negative"){
    mood <- mood * -1
  } else if(js$mood == "positive") {
    # do nothing
  } else {
    mood <- 0
  }
  
  return(mood)
}

require(igraph)
require(sna)
require(Matrix)
require(SparseM)
library(dplyr)
library(DT)
library(devtools)
#devtools::install_github("lchiffon/wordcloud2")
require(wordcloud)
## data manipultion
library(pwr)

############################################ Material news paper
require(shiny)
#install.packages("shinyBS")
require(shinyBS)
#install.packages("shinyjs")
require(shinyjs)
#install.packages("shinythemes")
require(shinythemes)
#install.packages("V8")
require(V8)
require(dplyr)
require(stringr)
require(lubridate)
library(shinymaterial)
tab = 'CONTENT'

gt_code = "<body>
<div id='google_translate_element'></div><script type='text/javascript'>
function googleTranslateElementInit() {
new google.translate.TranslateElement({pageLanguage: 'en', layout: google.translate.TranslateElement.InlineLayout.SIMPLE, multilanguagePage: true}, 'google_translate_element');
}
</script><script type='text/javascript' src='//translate.google.com/translate_a/element.js?cb=googleTranslateElementInit'></script>"

process_iframe = function(url){
  d = readLines(url) %>% paste(collapse='\n')
  tbl_ran = str_locate_all(d, 'TABLE') %>% range()
  header = substr(d, 1, tbl_ran[1]-2) %>% str_replace('<body>', gt_code) %>% 
    str_replace(fixed('html, body {'), 'html, body { max-width: 100%; overflow-x: hidden; ') # disable horiz scroll
  main   = substr(d, start = tbl_ran[1]-1, stop = tbl_ran[2]+1)
  # footer = substr(d, start = tbl_ran[2]+2, stop = nchar(d))
  footer = '</body></html>'
  main_head = substr(main, 1, str_locate(main, '<TR')[1]-1)
  main = substr(main, str_locate(main, '<TR')[1], nchar(main)) %>% str_replace("</TABLE>", '')
  main_foot = '</TABLE>'
  l = main %>% str_split('</TR>') %>% .[[1]] %>% subset(., . != '') %>% paste('</TR>')
  gt_root = 'https://translate.google.com/translate?hl=en&sl=auto&tl=en&u='
  gt_tail = '&sandbox=1'
  l2 = lapply(l, function(i){
    lnk = str_extract(i, 'javascript:window.open.*<B>') %>% str_extract('http[^\']*')
    str_replace(i, '</span>[ ]*</A>', paste0("</span></A><A href='", gt_root, lnk, gt_tail, "' target='_BLANK'>  (English)</A>")) %>% 
      str_replace(fixed("javascript:window.open('"), '') %>% str_replace(fixed("');\""), '" target="_BLANK"')
  }) %>% paste(collapse = '\n')
  paste(header, main_head, l2, main_foot, footer, collapse = '\n')
}

now = as.Date(Sys.Date())
add_days = function(d, n){
  d <- ymd(d)
  d %m+% days(n)
}
# add_hours = function(d, n){
#   #d = as.POSIXct(d)
#   d <- ymd_hms(d)
#   d %m+% minutes(n*60)
# }
rf_date = function(x) paste0(substr(x,1,4), '-', substr(x,5,6), '-', substr(x,7,8))

# function to translate logical inputs into a search string
format_search_str = function(x, mode){
  mode = paste0(mode, ':')
  x1 = x %>% str_replace_all(., ' ', '') %>% str_replace(., '^', mode) %>% 
    str_replace(., paste0(mode, '-'), paste0('-', mode)) %>%  # move minus to prefix position
    str_replace(., paste0(mode, '%22-'), paste0('-', mode, '%22')) # for image tags which also have %22
  ifelse(any(str_detect(x1, '-')),
         paste(x1, collapse='%20'),
         ifelse(length(x1) > 1, paste0('(', paste(x1, collapse='%20OR%20'), ')'),
                paste(x1, collapse='%20OR%20'))
  )
}

# paths for GDELT's doc' and 'geo' APIs, documented at:
# https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/
# https://blog.gdeltproject.org/gdelt-geo-2-0-api-debuts/
root =     'https://api.gdeltproject.org/api/v2/doc/doc?query='
geo_root = 'https://api.gdeltproject.org/api/v2/geo/geo?query='
tv_root='https://api.gdeltproject.org/api/v2/tv/tv?query='
# Menus for the respective API modes available, with 'doc' API  modes grouped appropriately
content_modes = c('MEDIA - a simple list of news articles that matched the search' = 'ArtList',
                  'MEDIA-ART - art layout of news articles that matched the search AND that include a social sharing image' = 'ArtGallery',
                  'IMAGE COLLAGE - matching images. Most relevant when used with the image-related search terms.' = 'ImageCollage',
                  'IMAGE COLLAGE INFO - as for IMAGE COLLAGE but includes links' = 'ImageCollageInfo',
                  'IMAGE GALLERY - alternative layout to IMAGE COLLAGE INFO' = 'ImageGallery',
                  'IMAGE COLLAGE SHARE - lists social sharing images found in the matching articles, where present.' = 'ImageCollageShare',
                  'WORDCLOUD ENGLISH - wordcloud of English translations of most relevant articles.' = 'WordCloudEnglish',
                  'WORDCLOUD NATIVE - wordcloud of most relevant articles (author language).' = 'WordCloudNative',
                  'WORDCLOUD THEME - histogram of the GDELT GKG Themes assigned to each article.' = 'WordCloudTheme',
                  'WORDCLOUD IMAGE TAGS - histogram of image tags assigned by Google\'s Cloud Vision algorithms.' = 'WordCloudImageTags',
                  'WORDCLOUD IMAGE WEB-TAGS - as above, but based on taxonomy tags assigned by Google CV based on captions identified via reverse Google Images searches.' = 'WordCloudImageWebTags',
                  'TONE CHART - emotional histogram showing the tonal distribution of coverage of your query. Articles typically score within ±20 (max ±100).' = 'ToneChart')

timeline_modes = c('VOLUME - % of global news coverage' = 'TimelineVol', 
                   'VOLUME INFO - as VOLUME but includes interactive URL links to top sources' = 'TimelineVolInfo', 
                   'SENTIMENT - average "tone" of all matching coverage' = 'TimelineTone',
                   'LANGUAGE - volume breakdown by language (65 supported)' = 'TimelineLang',
                   'SOURCE COUNTRY - which countries are focusing the most on the topic' = 'TimelineSourceCountry')

geo_modes = c('POINT-DATA - displays a dot at each location mentioned in proximity to your search term. Image functionality is disabled' = 'PointData',
              'IMAGE POINT-DATA - as above, but for image searches. Search terms are disabled' = 'ImagePointData',
              'POINT HEATMAP - heatmap of the locations most closely associated with your search term (GeoJSON only)' = 'PointHeatmap',
              'IMAGE POINT HEATMAP - as above, but for image searches' = 'ImagePointHeatmap',
              'POINT ANIMATION - a series of heatmaps in 15 minute increments over the past 24 hours (GeoJSON only)' = 'PointAnimation',
              'IMAGE POINT-ANIMATION - as above, but for image searches' = 'ImagePointAnimation',
              'COUNTRY - aggregates all locations to country level. Also performs normalization, dividing number of mentions in context of your search by total mentions' = 'Country',
              'IMAGE COUNTRY - as above, but for image searches' = 'ImageCountry',
              'SOURCE COUNTRY - reflects the country or origin of your search results' = 'SourceCountry',
              'IMAGE SOURCE COUNTRY - as above, but for image searches' = 'ImageSourceCountry',
              'ADM1 - as COUNTRY but higher administrative granularity' = 'ADM1',
              'IMAGE ADM1 - as above, but for image searches' = 'ImageADM1')
tv_modes=c("ClipGallery-This displays up to the top 50 most relevant clips matching your search"="ClipGallery",
           "StationChart-This compares how many results your search generates from each of the selected stations over the selected time period"="StationChart",
           "TimelineVol-This tracks how many results your search generates"="TimelineVol","TimelineVolNorm-This displays the total airtime"="TimelineVolNorm","WordCloud-This mode returns the top words that appear most frequently in clips matching your search"="WordCloud")
sort_options = c('', 'Date: newest first' = 'DateDesc', 'Date: oldest first' = 'DateAsc', 'Tone: most positive first' = 'ToneDesc', 'Tone: most negative first' = 'ToneAsc')
sort_options_tv=c('', 'Date: newest first' = 'DateDesc', 'Date: oldest first' = 'DateAsc')
# Options selection lists to read in
country_codes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT', sep='\t', stringsAsFactors = F, header = F)
country_codes = c('', setNames(as.character(country_codes$V1), country_codes$V2))
lang_codes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT', sep = '\t', stringsAsFactors = F, header = F)
lang_codes = c('', English = 'eng', setNames(as.character(lang_codes$V1), lang_codes$V2))
image_tags = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT', sep='\t', stringsAsFactors = F, header = F) %>% .[['V1']]
themes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT', sep = '\t', stringsAsFactors = F, header = F) %>% 
  arrange(V1) %>% mutate(V1 = str_replace_all(V1, '_', ' ')) %>% .[['V1']] # underscores removed for aesthetic. Need to re-add to URL calls
adm1 = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-ADM1S.TXT', header=F, sep='\t', stringsAsFactors = F)
adm1 = c('', setNames(as.character(adm1$V1), adm1$V2))



###########################################
#hashtaq 
list.of.packages = c("twitteR","stringr","ggplot2",
                     "ggvis","geonames","knitr","jsonlite","dplyr", "shiny","tidytext")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(twitteR)
library(tidytext)
library(stringr)
library(ggplot2)
library(ggvis)
library(knitr)
library(jsonlite)
library(dplyr)
library(geonames)

### disAnal
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(RCurl)
library(plotly)
library(viridis)
library(tidyverse)

variable <-F
# names(data)
url <- "https://twitter.com/intent/tweet?url=https://thibautfabacher.shinyapps.io/covid-19"

countries <- readOGR(dsn ="ne_50m_admin_0_countries.shp", 
                     layer = "ne_50m_admin_0_countries", 
                     encoding = "utf-8",use_iconv = T,
                     verbose = FALSE)

#load("shapeFile.RData")

dataCook<- function(data, pop, countries){
  
  countries$NAME<-c("Zimbabwe", "Zambia", "Yemen", "Vietnam", "Venezuela", "Vatican", 
                    "Vanuatu", "Uzbekistan", "Uruguay", "Micronesia", "Marshall Is.", 
                    "N. Mariana Is.", "U.S. Virgin Is.", "Guam", "American Samoa", 
                    "Puerto Rico", "United States of America", "S. Geo. and the Is.", 
                    "Br. Indian Ocean Ter.", "Saint Helena", "Pitcairn Is.", "Anguilla", 
                    "Falkland Is.", "Cayman Is.", "Bermuda", "British Virgin Is.", 
                    "Turks and Caicos Is.", "Montserrat", "Jersey", "Guernsey", "Isle of Man", 
                    "United Kingdom", "United Arab Emirates", "Ukraine", "Uganda", 
                    "Turkmenistan", "Turkey", "Tunisia", "Trinidad and Tobago", "Tonga", 
                    "Togo", "Timor-Leste", "Thailand", "Tanzania", "Tajikistan", 
                    "Taiwan", "Syria", "Switzerland", "Sweden", "eSwatini", "Suriname", 
                    "S. Sudan", "Sudan", "Sri Lanka", "Spain", "South Korea", "South Africa", 
                    "Somalia", "Somaliland", "Solomon Is.", "Slovakia", "Slovenia", 
                    "Singapore", "Sierra Leone", "Seychelles", "Serbia", "Senegal", 
                    "Saudi Arabia", "São Tomé and Principe", "San Marino", "Samoa", 
                    "St. Vin. and Gren.", "Saint Lucia", "St. Kitts and Nevis", "Rwanda", 
                    "Russia", "Romania", "Qatar", "Portugal", "Poland", "Philippines", 
                    "Peru", "Paraguay", "Papua New Guinea", "Panama", "Palau", "Pakistan", 
                    "Oman", "Norway", "North Korea", "Nigeria", "Niger", "Nicaragua", 
                    "New Zealand", "Niue", "Cook Is.", "Netherlands", "Aruba", "Curaçao", 
                    "Nepal", "Nauru", "Namibia", "Mozambique", "Morocco", "W. Sahara", 
                    "Montenegro", "Mongolia", "Moldova", "Monaco", "Mexico", "Mauritius", 
                    "Mauritania", "Malta", "Mali", "Maldives", "Malaysia", "Malawi", 
                    "Madagascar", "Macedonia", "Luxembourg", "Lithuania", "Liechtenstein", 
                    "Libya", "Liberia", "Lesotho", "Lebanon", "Latvia", "Laos", "Kyrgyzstan", 
                    "Kuwait", "Kosovo", "Kiribati", "Kenya", "Kazakhstan", "Jordan", 
                    "Japan", "Jamaica", "Italy", "Israel", "Palestine", "Ireland", 
                    "Iraq", "Iran", "Indonesia", "India", "Iceland", "Hungary", "Honduras", 
                    "Haiti", "Guyana", "Guinea-Bissau", "Guinea", "Guatemala", "Grenada", 
                    "Greece", "Ghana", "Germany", "Georgia", "Gambia", "Gabon", "France", 
                    "St. Pierre and Miquelon", "Wallis and Futuna Is.", "St-Martin", 
                    "St-Barthélemy", "Fr. Polynesia", "New Caledonia", "Fr. S. Antarctic Lands", 
                    "Åland", "Finland", "Fiji", "Ethiopia", "Estonia", "Eritrea", 
                    "Eq. Guinea", "El Salvador", "Egypt", "Ecuador", "Dominican Rep.", 
                    "Dominica", "Djibouti", "Greenland", "Faeroe Is.", "Denmark", 
                    "Czechia", "N. Cyprus", "Cyprus", "Cuba", "Croatia", "Côte d'Ivoire", 
                    "Costa Rica", "Dem. Rep. Congo", "Congo", "Comoros", "Colombia", 
                    "China", "Macao", "Hong Kong", "Chile", "Chad", "Central African Rep.", 
                    "Cabo Verde", "Canada", "Cameroon", "Cambodia", "Myanmar", "Burundi", 
                    "Burkina Faso", "Bulgaria", "Brunei", "Brazil", "Botswana", "Bosnia and Herz.", 
                    "Bolivia", "Bhutan", "Benin", "Belize", "Belgium", "Belarus", 
                    "Barbados", "Bangladesh", "Bahrain", "Bahamas", "Azerbaijan", 
                    "Austria", "Australia", "Indian Ocean Ter.", "Heard I. and McDonald Is.", 
                    "Norfolk Island", "Ashmore and Cartier Is.", "Armenia", "Argentina", 
                    "Antigua and Barb.", "Angola", "Andorra", "Algeria", "Albania", 
                    "Afghanistan", "Siachen Glacier", "Antarctica", "Sint Maarten"
  )
  data$`Country/Region`<-as.character(data$`Country/Region`)
  data$`Country/Region`[data$`Country/Region`=="Macau"]<- "Macao"
  data$`Country/Region`[data$`Country/Region`=="Mainland China"]<- "China"
  data$`Country/Region`[data$`Country/Region`=="South Korea"]<- "South Korea"
  data$`Country/Region`[data$`Country/Region`=="North Macedonia"]<- "Macedonia"
  data$`Country/Region`[data$`Country/Region`=="Czech Republic"]<- "Czechia"
  data$`Country/Region`[data$`Country/Region`=="Dominican Republic"]<- "Dominican Rep."
  data$`Country/Region`[data$`Country/Region`=="UK"]<- "United Kingdom"
  data$`Country/Region`[data$`Country/Region`=="Gibraltar"]<- "United Kingdom"
  data$`Country/Region`[data$`Country/Region`=="US"]<- "United States"
  data$`Country/Region`[data$`Country/Region`=="Saint Barthelemy"]<- "St-Barthélemy"
  
  data$`Country/Region`[data$`Country/Region`=="Faroe Islands"]<- "Faeroe Is."
  data$`Country/Region`[data$`Country/Region`=="Bosnia and Herzegovina"]<- "Bosnia and Herz."
  data$`Country/Region`[data$`Country/Region`=="Vatican City"]<- "Vatican"
  data$`Country/Region`[data$`Country/Region`=="Korea, South"]<- "South Korea"
  data$`Country/Region`[data$`Country/Region`=="Republic of Ireland"]<- "Ireland"
  data$`Country/Region`[data$`Country/Region`=="Taiwan*"]<-"Taiwan"
  data$`Country/Region`[data$`Country/Region`=="Taiwan*"]<-"Taiwan"
  
  data$`Country/Region`[data$`Country/Region`=="Congo (Kinshasa)"]<-"Congo"
  data$`Country/Region`[data$`Country/Region`=="Cote d'Ivoire"]<-"Côte d'Ivoire"
  data$`Country/Region`[data$`Country/Region`=="Reunion"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Martinique"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="French Guiana"]<-"France"
  data$`Country/Region`[data$`Country/Region`=="Holy See"]<-"Vatican"
  
  # 
  # countries$NAME<-as.character(countries$NAME)
  # countries$NAME[is.na(countries$NAME)]<-"Côte d'Ivoire"
  data$Pays<-as.character(unique(countries$NAME)[charmatch(data$`Country/Region`,unique(countries$NAME))])
  print(data$`Country/Region`[is.na(data$Pays)])
  dataPays<- data%>%dplyr::select(-`Province/State`, -Lat, -Long,-`Country/Region`)%>%group_by(Pays)%>%summarise_each(sum)
  dataPays$Pays<-as.character(dataPays$Pays)
  return(dataPays)
}

##
#https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population



population<- read.csv("pop.csv",stringsAsFactors = F)
population<- population[-1,]
population$pays<-as.character(unique(countries$NAME)[charmatch(population$Country,unique(countries$NAME))])
###

URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
data <- read.csv(text = URL, check.names = F)
dataCases<- dataCook(data, pop, countries)

URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data <- read.csv(text = URL, check.names = F)
dataDeaths<- dataCook(data, pop, countries)

#time_series_19-covid-Recovered.csv
URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
data <- read.csv(text = URL, check.names = F)
dataRecovered<- dataCook(data, pop, countries)
####

dataPays<-function(data=dataCases) return(data)
jour<-names(dataCases%>%select(contains( "/")))
jourDate<- as.Date(jour, "%m/%d/%y")
names(dataCases)[str_detect(names(dataCases), "/")]<-format.Date(jourDate, "%m/%d/%y")
names(dataDeaths)[str_detect(names(dataDeaths), "/")]<-format.Date(jourDate, "%m/%d/%y")
names(dataRecovered)[str_detect(names(dataRecovered), "/")]<-format.Date(jourDate, "%m/%d/%y")


dataCases<-left_join(data.frame(Pays = countries$NAME%>%as.character(), Pop =countries$POP_EST%>%as.character()%>%as.numeric()),dataCases)

dataDeaths<-left_join(data.frame(Pays = countries$NAME%>%as.character(), Pop =countries$POP_EST%>%as.character()%>%as.numeric()),dataDeaths)

dataRecovered<-left_join(data.frame(Pays = countries$NAME%>%as.character(), Pop =countries$POP_EST%>%as.character()%>%as.numeric()),dataRecovered)


arrondi<- function(x) 10^(ceiling(log10(x)))

dataDeaths[is.na(dataDeaths)]<- 0
dataCases[is.na(dataCases)]<- 0
dataRecovered[is.na(dataRecovered)]<- 0
dataraw=data.frame(Country=dataRecovered$Pays,Cases=dataCases[,dim(dataCases)[2]],Death=dataDeaths[,dim(dataDeaths)[2]],Recovered=dataRecovered[,dim(dataRecovered)[2]])
Ccase=sum(dataraw$Cases)
Ddeaths=sum(dataraw$Death)
Rrecovered=sum(dataraw$Recovered)
Dp=round((Ddeaths/Ccase)*100,2)
Rp=round((Rrecovered/Ccase)*100,2)
# ######## GPR
library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(metathis)
library(directlabels)
library(ggthemes)
library(hrbrthemes)
library(shinyMobile)
library(wordcloud2)
library(png)
library(magick)
library(gridExtra)
library(grid)

options(scipen = 9999)



get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

get_txt <- function(txt) {
  grid::textGrob(txt)
}

add_logoplot <- function(p, size = 0.05){
  #imgby = get_png("by.png")
  #imgcc = get_png("cc.png")
  #txt = get_txt("CC-BY André Calero Valdez/@sumidu")
  xpos = 0.01
  #size = 0.05
  logo <-
    ggplot() +
    aes(x = 0:1, y = 1) +
    theme_void() +
    #annotation_custom(txt, xmin = 0, xmax = 0.5, ymin = 0) +
    #annotation_custom(imgby, xmin = 1-xpos, xmax = 1-(xpos + size), ymin = 0)+
    #annotation_custom(imgcc, xmin = 1-(xpos+size), xmax = 1-(xpos+(2*size)), ymin = 0) +
    NULL
  gridExtra::grid.arrange(p, logo, heights = c(1-size, size))
}

# 
# #p <- qplot(mtcars$mpg) 
# #add_logoplot(p, 0.05)
# 
write_ts <- function(key) {
  file_timestamp <- paste0(key,"_updated.rds")
  timestamp <- lubridate::now()
  write_rds(timestamp, file_timestamp)
}
# 
read_ts <- function(key) {
  file_timestamp <- paste0(key,"_updated.rds")
  if(file.exists(file_timestamp)) {
    timestamp <- read_rds(file_timestamp)
  }
  else timestamp <- Sys.Date() - 1
  timestamp
}

read_cached_file <- function(url, file){
  timestamp <- read_ts(str_sub(file, end = -5))
  now <- lubridate::now()
  data_age <- lubridate::interval(timestamp, now)

  if (time_length(data_age, "hours") > 12) {
    data <- read_csv(url)
    write_rds(data, file)
    write_ts(str_sub(file, end = -5))
  } else {
    cat(paste("Data age:", as.duration(data_age), "- Using cached version."))
    data <- read_rds(file)
  }
  data
}
# 
read_confirmed_cases <- function() {
  read_cached_file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                   "confirmed.rds")
}

read_death_cases <- function() {
  read_cached_file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                   "deaths.rds")
}
# 
confirmed <- read_confirmed_cases() %>%
  gather(date, value, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = paste0(date,"20")) %>%
  mutate(date = mdy(date)) %>%
  mutate(type = "confirmed")

deaths <- read_death_cases() %>%
  gather(date, value, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = paste0(date,"20")) %>%
  mutate(date = mdy(date)) %>%
  mutate(type = "deceased")

# 
all_data <- bind_rows(confirmed, deaths) %>%
  group_by(`Country/Region`, date, type) %>%
  summarise(value = sum(value))

countries1 <- all_data %>% arrange(desc(value)) %>%
  pull(`Country/Region`) %>% unique()
countries1=str_sort(countries1)

data_start <- all_data %>% pull(date) %>% min()
data_end <- all_data %>% pull(date) %>% max()

all_cases <- all_data %>% group_by(`Country/Region`) %>%  summarise(cases = max(value)) %>% tally(cases) %>% pull(n)
#all_death <- all_data %>% group_by(`Country/Region`) %>%  summarise(deaths = max(value)) %>% tally(deaths) %>% pull(n)
#all_recovered <- all_data %>% group_by(`Country/Region`) %>%  summarise(cases = max(value)) %>% tally(cases) %>% pull(n)


############ 10 day forcasrt 

## ---------------------------
##
## Script name: functions.R
##
## Purpose of script: Hold a bunch of functions for coronaRisk app
##
## Author: Ben Phillips
##
## Date Created: 2020-03-12
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 

## ---------------------------

## function definitions

# calculates doubling time over the last inWindow days.
doubTime <- function(cases, time, inWindow = 10){
  r <- projSimpleSlope(cases, time, inWindow = inWindow)[2]
  log(2)/r
}


# growth rate
growthRate <- function(cases, inWindow=10){
  nn <- length(cases)
  ss <- (nn - inWindow + 1):nn
  rate <- numeric(length(ss))
  rate[ss] <- 100 * (cases[ss] - cases[ss-1]) / cases[ss-1]
}


# aggregates results to country
countryAgg<-function(x){
  xSelect<-x[, dateCols(x)]
  aggregate(xSelect, by = list(Country = x$Country.Region), FUN = sum)
}

# calculates the curve flatenning index.
# it is the second derivative of logA wrt t (the change in growth rate) divided by first differential (the current growth rate).
cfi <- function(active){
  lnact <-log(active)
  cfiInd <- -diff(diff(lnact))/abs(diff(lnact)[-1])
  cfiInd[abs(cfiInd)>10]<-NA # remove crazy values associated with changed test/diagnosis
  cfiInd
}

# estimates detection rate based on assumptions about cfr, ttd
detRate<-function(infd, deaths, cfr = 0.033, ttd=17, window=5){
  obs<-c(rep(NA, window), diff(infd, window)) # observed new cases
  deathDiff<-diff(deaths, window) # observed new deaths
  expd<-deathDiff/cfr #expected new cases given cfr
  expd<-expd[-(1:(ttd-window))]
  expd<-c(expd, rep(NA, ttd))
  detRate<-obs/expd
  detRate[detRate==0]<-NA
  detRate[is.infinite(detRate)]<-NA
  out<-mean(detRate, na.rm = TRUE)
  if (is.nan(out)) return(NA)
  if (out>1) out<-1
  out
}

# Simple projection based on growth over last inWindow days
# returns extended plotting data
projSimple<-function(rawN, rawTime, inWindow=10, extWindow=10){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  x <- c(rawTime[ss], rawTime[nn]+1:extWindow)
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  extFit <- predict(mFit, newdata = list(tIn = x), interval = "confidence")
  y <- exp(extFit)
  list(x=x, y=y)
}

# Simple projection based on growth over last inWindow days
# returns coefficients
projSimpleSlope<-function(rawN, rawTime, inWindow=10){
  nn <- length(rawN)
  ss <- (nn-inWindow+1):nn
  x <- c(rawTime[ss], rawTime[nn]+1:inWindow)
  lnN <- log(rawN[ss])
  lnN[is.infinite(lnN)]<-NA
  tIn <- rawTime[ss]
  mFit <- lm(lnN~tIn)
  coefficients(mFit)
}


# to identify the date columns in ts dataframes
dateCols<-function(x){
  grepl(pattern = "\\d", x = colnames(x))
}

# To subset time series data and aggregate totals
tsSub <- function(x, subset){
  xSub<-x[subset, dateCols(x)]
  colSums(xSub)
}


library("readr")

## ---------------------------
## load up functions


## ---------------------------


## Get data
tsConf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
tsDeath <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
tsTesting <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_testing_global.csv"

tsI<-read_csv(file = tsConf)
tsD<-read_csv(file = tsDeath)
#tsT<-read_csv(file = tsTesting)

## get Date range
dCols<-dateCols(tsI)
dates<-as.Date(colnames(tsI)[dCols], format = "%m/%d/%y")

## Tidy up names
names(tsI)[!dCols] <- make.names(names(tsI)[!dCols])
names(tsD)[!dCols] <- make.names(names(tsD)[!dCols])
#names(tsT)[!dCols] <- make.names(names(tsT)[!dCols])

## add recovery lag -- assumes all cases recover at 22 days
matI<-as.matrix(tsI[, dCols])
matD<-as.matrix(tsD[, dCols])
matA<-matI-matD #remove deaths
matR <- cbind(matrix(0, nrow = nrow(matA), ncol = 22), matA[, -((ncol(matA)-21):ncol(matA))]) # recovered
matA <- matA - matR

tsA <- cbind(tsI[,!dCols], matA) # active cases

tsACountry <- countryAgg(tsA) # aggregated to country

# This would order from most to least active cases - but lets leave it alphabetical
#tsACountry <- tsACountry[rev(order(tsACountry[[ncol(tsACountry)-1]])),] 

## Define menus
# get region names with 20 or more cases as of yesterday
ddNames <- tsACountry$Country[tsACountry[[ncol(tsACountry)-1]]>19]

ddReg <- ddNames
names(ddReg) <- ddNames

######## Klerman filtter
library(KFAS)
library(TTR)
require(lubridate)

read_cached_file <- function(url, file){
  timestamp <- read_ts(str_sub(file, end = -5))
  now <- lubridate::now()
  data_age <- lubridate::interval(timestamp, now)
  
  if (time_length(data_age, "hours") > 12) {
    data <- read_csv(url)
    write_rds(data, file)
    write_ts(str_sub(file, end = -5))
  } else {
    cat(paste("Data age:", as.duration(data_age), "- Using cached version."))
    data <- read_rds(file)
  }
  data
}
# 
read_confirmed_cases <- function() {
  read_cached_file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                   "confirmed.rds")
}
# Death
read_death_cases <- function() {
  read_cached_file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                   "deaths.rds")
}
#
confirmed <- read_confirmed_cases() %>%
  gather(date, value, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = paste0(date,"20")) %>%
  mutate(date = mdy(date)) %>%
  mutate(type = "confirmed")

deaths <- read_death_cases() %>%
  gather(date, value, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
  mutate(date = paste0(date,"20")) %>%
  mutate(date = mdy(date)) %>%
  mutate(type = "deceased")

all_data <- bind_rows(confirmed, deaths) %>%
  group_by(`Country/Region`, date, type) %>%
  summarise(value = sum(value))

#countries1 <- all_data %>% arrange(desc(value)) %>%
 # pull(`Country/Region`) %>% unique()

data_start <- all_data %>% pull(date) %>% min()
data_end <- all_data %>% pull(date) %>% max()
#require(smooth)
#require(Mcomp)
#require(Quandl)
####
cat("\014")

