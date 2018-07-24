library(rtimes)
library(plyr)
library(quantmod)
library(tm)
library(stringr)
library(lubridate)
library(ggplot2)
library(XML)
library(httr)

setwd("/Users/apple/Desktop/582 project")
pwords <- scan('positive-words.txt', what='character')
nwords <- scan('negative-words.txt', what='character')
# Words downloaded from 
# https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/tree/08a269765a6b185d5f3dd522c876043ba9628715/data/opinion-lexicon-English

# Get New NYT article data-------------------------------------------------------

#key = "2542f5e7ed5e4d07bff7ab59e3a084b6"
key = "325da26d73604830b77d8337c5afcc4a"
q = "APPLE" # Query
fq = 'section_name:("Business") AND type_of_material:("News")' # Filtered search query
#fq = 'section_name:("Business")'
begin_date <- "20160101"
end_date <- "20160630"
sort <- "oldest"

# Search articles by key words and page
search_articles <- function(pag) { 
    as <- as_search(key = key, q = q, fq = fq,
                    begin_date = begin_date,
                    end_date = end_date,
                    sort = sort, page = pag)
    data <- as$data
    return(data)
}

# Get article body texts by url
parseArticleBody <- function(artHTML) {
    xpath2try <- c('//div[@class="articleBody"]//p',
                   '//p[@class="story-body-text story-content"]',
                   '//p[@class="story-body-text"]'
    )
    for(xp in xpath2try) {
        bodyi <- paste(xpathSApply(htmlParse(artHTML), xp, xmlValue), collapse='')
        if(nchar(bodyi)>0) break
    }
    return(bodyi)
}
# http://brooksandrew.github.io/simpleblog/articles/new-york-times-api-to-mongodb/#extracting-and-parsing-the-article-body-text

# Get articles
articles <- search_articles(0)
tocountpages <- as_search(key = key, q = q, fq = fq,
                          begin_date = begin_date, end_date = end_date,
                          sort = sort, page = 0)
pages <- as.numeric(floor(tocountpages$meta["hits"]/10)) # Page numbers

word_count <- c()
pub_date <- c()
score <- c() # score = Positive word number - Negative word number
# Process data
# Convert the obtained values to a data frame
for (i in 0:pages) {
    Sys.sleep(1)
    articles <- try(search_articles(i),TRUE)
    if(inherits(articles, "try-error")){next}
    texttoprocess <- c()
    for (j in 1:length(articles)) {
        theurl <- articles[[j]]$web_url
        p <- GET(theurl)
        html <- content(p, 'text')
        artBodytext <- parseArticleBody(html)
        if (artBodytext == "") {next}
        texttoprocess <- c(texttoprocess, artBodytext)
        word_count <- c(word_count, articles[[j]]$word_count)
        pub_date <- c(pub_date, articles[[j]]$pub_date)
    }
    processtext <- Corpus(VectorSource(texttoprocess))
    processtext <- tm_map(processtext, removePunctuation)
    processtext <- tm_map(processtext, content_transformer(tolower))
    processtext <- tm_map(processtext, removeWords, stopwords("en"))
    scorecount<- laply(processtext, 
                 function(processtext, pwords,nwords) {
                       word.list = str_split(processtext, '\\s+' )
                       words = unlist(word.list)
                       pos.matches = match(words, pwords)
                       neg.matches = match(words, nwords)
                       pos.matches.status = !is.na(pos.matches)
                       neg.matches.status = !is.na(neg.matches)
                       score = sum(pos.matches.status) - sum(neg.matches.status)
                       return(score) }, pwords, nwords)
    score <- c(score, scorecount)
    print(i)
}
word_count <- as.numeric(word_count)
df <- data.frame("Date" = pub_date, "Word count" = word_count, "Score" =score)
df$Date <- str_sub(df$Date, end=10)
df$Date <- ymd(df$Date)
df$Day <- weekdays(df$Date)

# Get Stock data-------------------------------------------------------

begin.date <- "2016-01-01"
end.date <- "2016-06-30"

stockname <- "AAPL"
apple <- getSymbols(stockname, from = begin.date, to = end.date, auto.assign = F)
apple <- as.data.frame(apple)

getstock <- function(stock.name, prd) {
    stock <- getSymbols(stock.name, from = begin.date, to = end.date, auto.assign = F)
    prdrt <- periodReturn(stock, period = "daily")
    stock <- as.data.frame(stock)
    date <- rownames(stock)
    volume <- stock[, 5]
    a <- data.frame("Date" = date, "Return" = prdrt, "Volume" = volume)
    return(a)
}
aapl <- getstock("AAPL")
aapl$Date <- ymd(aapl$Date)

# Plots --------------------------------------------------------
dfdays <- factor(df$Day,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", "Sunday"))
barplot(table(dfdays), xlab = "Day", ylab = "Count")
ggplot(df, aes(x=dfdays, y=df$Word.count))+geom_boxplot()
ggplot(df, aes(x=dfdays, y=df$Word.count))+geom_boxplot()
# Weekday-word count

plot(df$Date, df$Score, xlab = "Date", ylab = "Score", cex = .5)
plot(aapl$Date, aapl$daily.returns, 
     xlab = "Date", ylab = "Daily return")

#dev.off()

mergeaapldf <- merge(aapl, df, by.x = "Date", by.y = "Date")
plot(mergeaapldf$daily.returns, mergeaapldf$score)
plot(mergeaapldf$Volume, mergeaapldf$score)

# Logistic Regression
glm.fit <- glm(Score~Date, data=df)
summary(glm.fit)
summary(glm.fit$coefficients)

glm.probs <- predict(glm.fit, data=stockreturns, type ="response")


glm.pred=rep("No", length(stockreturns$AAPL))
glm.pred[glm.probs >.5]="Yes"

table(glm.pred, default)
mean(glm.pred == default)








