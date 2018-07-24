library(plyr)
library(rtimes)
library(XML)
library(httr)
library(quantmod)
library(tm)
library(stringr)
library(sentimentr)
library(lubridate)
library(ggplot2)


setwd("/Users/apple/Desktop/582 project")
pwords <- scan('positive-words.txt', what='character')
nwords <- scan('negative-words.txt', what='character')
# Words downloaded from 
# https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/tree/08a269765a6b185d5f3dd522c876043ba9628715/data/opinion-lexicon-English

# Get New NYT article data-------------------------------------------------------

key = "325da26d73604830b77d8337c5afcc4a"
q = "cola" # Query
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
articles <- search_articles(1)
tocountpages <- as_search(key = key, q = q, fq = fq,
                          begin_date = begin_date, end_date = end_date,
                          sort = sort, page = 1)
pages <- as.numeric(floor(tocountpages$meta["hits"]/10)) # Page numbers

word_count_date <- c()
word_count <- c()
pub_date <- c()
sd <- c()
ave <- c()
score <- c()
# Process data
# Convert the obtained values to a data frame
for (i in 1:pages) {
    Sys.sleep(1) # If not, it will get a error message "Too Many Request".
    articles <- try(search_articles(i),TRUE)
    if(inherits(articles, "try-error")){next}
    texttoprocess <- c()
    for (j in 1:length(articles)) {
        theurl <- articles[[j]]$web_url
        word_count_date <- c(word_count_date, articles[[j]]$pub_date)
        word_count <- c(word_count, articles[[j]]$word_count)
        p <- GET(theurl)
        html <- content(p, 'text')
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
ko <- getstock("KO")
ko$Date <- ymd(ko$Date)

# Plots --------------------------------------------------------
dfdays <- factor(word$Day,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", "Sunday"))
barplot(table(dfdays), xlab = "Day", ylab = "Count", main = "Article numbers by day")
ggplot(word, aes(x=dfdays, y=word$word_count)) + geom_boxplot() + xlab("Day") +
    ylab("Word Count") + ggtitle("Word counts by day")
# day-word count

plot(stmtdf$Date, stmtdf$ave, xlab = "Date", ylab = "Average Sentiment", cex = .5)
plot(stmtdf$Date, stmtdf$Score, xlab = "Date", ylab = "Score", cex = .5)
plot(ko$Date, ko$daily.returns, xlab = "Date", ylab = "Daily return")



mergeko <- merge(ko, stmtdf, by.x = "Date", by.y = "Date")
mergeko$class <- as.factor(ifelse(mergeko$daily.returns > 0,"UP","DOWN")) 
mergeko$Score <- as.numeric(mergeko$Score)

par(mfrow=c(1,2))
plot(mergeko$ave, mergeko$daily.returns, 
     xlab = "Sentiment value", ylab = "Stock daily return")
plot(mergeko$ave, mergeko$Volume, 
     xlab = "Sentiment value", ylab = "Trading volume")
#dev.off()
par(mfrow=c(1,2))
plot(mergeko$sd, mergeko$daily.returns, 
     xlab = "Sentiment sd", ylab = "Stock daily return")
plot(mergeko$sd, mergeko$Volume, 
     xlab = "Sentiment sd", ylab = "Trading volume")
#dev.off()
par(mfrow=c(1,2))
plot(mergeko$Score, mergeko$daily.returns, 
     xlab = "Score", ylab = "Stock daily return")
plot(mergeko$Score, mergeko$Volume, 
     xlab = "Score", ylab = "Trading volume")
#dev.off()

# Regression ---------------------------------

# Logistic Regression

div <- ceiling(nrow(mergeko)*2/3)
TrainingSet <- mergeko[1:div,]
TestSet <- mergeko[(div+1):nrow(mergeko),]
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
