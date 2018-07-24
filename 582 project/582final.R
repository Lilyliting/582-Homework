
library(plyr)
library(rtimes)

library(XML)
library(httr)
library(quantmod)
library(tm)
library(stringr)

library(lubridate)
library(ggplot2)


setwd("/Users/apple/Desktop/582 project")
pwords <- scan('positive-words.txt', what='character')
nwords <- scan('negative-words.txt', what='character')
# Words downloaded from 
# https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/tree/08a269765a6b185d5f3dd522c876043ba9628715/data/opinion-lexicon-English

# Get New NYT article data-------------------------------------------------------

key = "8a4cbc18b4a9411da578859acc859a26"
q = "APPLE" # Query
fq = 'section_name:("Business") AND type_of_material:("News")' # Filtered search query
begin_date <- "20160101"
end_date <- "20161130"
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

word_count_date <- c()
word_count <- c()
pub_date <- c()
sd <- c()
ave <- c()
score <- c()
# Process data
# Convert the obtained values to a data frame
for (i in 0:pages) {
    Sys.sleep(1) # If not, it will get a error message "Too Many Request".
    articles <- try(search_articles(i),TRUE)
    if(inherits(articles, "try-error")){next}
    texttoprocess <- c()
    for (j in 1:length(articles)) {
        theurl <- articles[[j]]$web_url
        word_count_date <- c(word_count_date, articles[[j]]$pub_date)
        word_count <- c(word_count, articles[[j]]$word_count)
        p <- GET(theurl)
        html <- content(p, "text")
        artBodytext <- parseArticleBody(html)
        print(artBodytext)
        if (artBodytext == "") {next}
        texttoprocess <- c(texttoprocess, artBodytext)
        word_count <- c(word_count, articles[[j]]$word_count)
        pub_date <- c(pub_date, articles[[j]]$pub_date)
    }
    if (length(texttoprocess) == 0) {next}
    stmt <- sentiment_by(texttoprocess)
    sd <- c(sd, stmt$sd)
    ave <- c(ave, stmt$ave_sentiment)
    # sentiment analysis
    
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
    # sentiment analysis with score
    
    print(i)
}

word_count <- as.numeric(word_count)
word_count <- word_count[seq(1:length(word_count_date))]
word_count_date <- str_sub(word_count_date, end=10)
word_count_date <- ymd(word_count_date)
word <- data.frame("Date" = word_count_date, word_count)
word$Day <- weekdays(word$Date)


stmtdf <- data.frame("Date" = pub_date, "sd" =sd, "ave" =ave, "Score" =score)
stmtdf$Date <- str_sub(stmtdf$Date, end=10)
stmtdf$Date <- ymd(stmtdf$Date)

# Get stock data ---------------------------------
begin.date <- "2016-01-01"
end.date <- "2016-11-30"

getstock <- function(stock.name, prd) {
    stock <- getSymbols(stock.name, from = begin.date, to = end.date, auto.assign = F)
    prdrt <- periodReturn(stock, period = "daily")
    prdrt <- as.data.frame(prdrt)
    stock <- as.data.frame(stock)
    date <- rownames(stock)
    volume <- stock[, 5]
    a <- data.frame("Date" = date, "Return" = prdrt, "Volume" = volume)
    return(a)
}
aapl <- getstock("AAPL")
aapl$Date <- ymd(aapl$Date)

# Plots --------------------------------------------------------
dfdays <- factor(word$Day,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", "Sunday"))
barplot(table(dfdays), xlab = "Day", ylab = "Count", main = "Article numbers by day")
ggplot(word, aes(x=dfdays, y=word$word_count)) + geom_boxplot() + xlab("Day") +
    ylab("Word Count") + ggtitle("Word counts by day")
# day-word count

plot(stmtdf$Date, stmtdf$ave, xlab = "Date", ylab = "Average Sentiment", cex = .5)
plot(stmtdf$Date, stmtdf$Score, xlab = "Date", ylab = "Score", cex = .5)
plot(aapl$Date, aapl$daily.returns, xlab = "Date", ylab = "Daily return")



mergeaapl <- merge(aapl, stmtdf, by.x = "Date", by.y = "Date")
mergeaapl$class <- as.factor(ifelse(mergeaapl$daily.returns > 0,"UP","DOWN")) 
mergeaapl$Score <- as.numeric(mergeaapl$Score)

par(mfrow=c(1,2))
plot(mergeaapl$ave, mergeaapl$daily.returns, 
     xlab = "Sentiment value", ylab = "Stock daily return")
plot(mergeaapl$ave, mergeaapl$Volume, 
     xlab = "Sentiment value", ylab = "Trading volume")
#dev.off()
par(mfrow=c(1,2))
plot(mergeaapl$sd, mergeaapl$daily.returns, 
     xlab = "Sentiment sd", ylab = "Stock daily return")
plot(mergeaapl$sd, mergeaapl$Volume, 
     xlab = "Sentiment sd", ylab = "Trading volume")
#dev.off()
par(mfrow=c(1,2))
plot(mergeaapl$Score, mergeaapl$daily.returns, 
     xlab = "Score", ylab = "Stock daily return")
plot(mergeaapl$Score, mergeaapl$Volume, 
     xlab = "Score", ylab = "Trading volume")
#dev.off()

# Regression ---------------------------------

# Logistic Regression

div <- ceiling(nrow(mergeaapl)*2/3)
TrainingSet <- mergeaapl[1:div,]
TestSet <- mergeaapl[(div+1):nrow(mergeaapl),]
# Divide 2/3 into training set and 1/3 into testing set 

# Regression by sentiment value
glm.fit1 <- glm(class~sd + ave, data = TrainingSet, family=binomial)
summary(glm.fit1)
glm.probs1 <- predict(glm.fit1, TestSet, type ="response")

glm.pred1=rep("DOWN", nrow(TestSet))
glm.pred1[glm.probs1 >.5]="UP"

table(glm.pred1, TestSet$class)
mean(glm.pred1 == TestSet$class)
# 0.6125

# Regression by score
glm.fit2 <- glm(class~Score, data = TrainingSet, family=binomial)
summary(glm.fit2)
glm.probs2 <- predict(glm.fit2, TestSet, type ="response")

glm.pred2=rep("DOWN", nrow(TestSet))
glm.pred2[glm.probs2 >.5]="UP"

table(glm.pred2, TestSet$class)
mean(glm.pred2 == TestSet$class)
# 0.5875
