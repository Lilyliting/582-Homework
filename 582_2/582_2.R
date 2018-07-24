# Problem 1 -------------------------------------------------

library (ISLR)
library (MASS)
library (class)
setwd("/Users/apple/Desktop/FE/582/582_2")
DefaultPredict <- read.csv("DefaultPredict.csv")
head(DefaultPredict)
dimDP <- dim(DefaultPredict)

default <- Default$default
# Logistic Regression
str(Default)
glm.fit <- glm(default~balance+student+income, data=Default, family=binomial)
summary(glm.fit)
summary(glm.fit$coefficients)

glm.probs <- predict(glm.fit, data=Default, type ="response")

glm.pred=rep("No", 10000)
glm.pred[glm.probs >.5]="Yes"

table(glm.pred, default)
mean(glm.pred == default)
# 0.9732

glm.probs <- predict(glm.fit, DefaultPredict, type="response")
glm.pred <- rep("No", dimDP[1])
glm.pred[glm.probs >.5] <- "Yes"
DefaultPredict$glm.pred <- glm.pred

# Linear Discriminant Analysis
lda.fit <- lda(default~balance+student+income, data=Default)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, Default)
lda.class <- lda.pred$class
table(lda.class, default)
mean(lda.class == default)
# 0.9724

sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)
sum(lda.pred$posterior[, 1] > .9)

lda.probs <- predict(lda.fit, DefaultPredict, type="response")
lda.pred <- rep("No", dimDP[1])
lda.pred[lda.probs$posterior[, 2] > .5] <- "Yes"
DefaultPredict$lda.pred <- lda.pred

# Quadratic Discriminant Analysis
qda.fit <- qda(default~balance+student+income, data=Default)
qda.fit

qda.probs <- predict(qda.fit, Default)
qda.class <- qda.pred$class
table(qda.class, default)
mean(qda.class == default)
# 0.973

qda.probs <- predict(qda.fit, DefaultPredict, type="response")
DefaultPredict$qda.pred <- qda.probs$class

attach(Default)
detach(Default)
# K-Nearest Neighbors
Default.X <- cbind(Default$student, Default$balance, Default$income)
DefaultPredict.X <- cbind(DefaultPredict$student, DefaultPredict$balance, DefaultPredict$income)
class(Default.X)
dim(Default.X)
dim(DefaultPredict.X)


knn.pred1 <- knn(Default.X, DefaultPredict.X, default, k = 1)
knn.pred3 <- knn(Default.X, DefaultPredict.X, default, k = 3)
knn.pred6 <- knn(Default.X, DefaultPredict.X, default, k = 6)
knn.pred10 <- knn(Default.X, DefaultPredict.X, default, k = 10)
DefaultPredict$knn.pred1 <- knn.pred1
DefaultPredict$knn.pred3 <- knn.pred3
DefaultPredict$knn.pred6 <- knn.pred6
DefaultPredict$knn.pred10 <- knn.pred10
detach(Default)

write.csv(DefaultPredict, "DefaultPredicts.csv", row.names = F)
DefaultPredict


# Problem 2 -------------------------------------------------

library(NLP)
library(tm)

spamPath = "/Users/apple/Desktop/582/RSpamData" #spamPath = "."
list.dirs(spamPath, full.names = FALSE)

list.files(path = paste(spamPath, "messages", 
                        sep = .Platform$file.sep))

head(list.files(path = paste(spamPath, "messages", "spam_2",
                             sep = .Platform$file.sep)))

dirNames = list.files(path = paste(spamPath, "messages", 
                                   sep = .Platform$file.sep))
length(list.files(paste(spamPath, "messages", dirNames, 
                        sep = .Platform$file.sep)))

sapply(paste(spamPath, "messages", dirNames, 
             sep = .Platform$file.sep), 
       function(dir) length(list.files(dir)) )

fullDirNames = paste(spamPath, "messages", dirNames, 
                     sep = .Platform$file.sep)

fileNames = list.files(fullDirNames[1], full.names = TRUE)
fileNames[1]

msg = readLines(fileNames[1])
head(msg)

indx = c(1:5, 15, 27, 68, 69, 329, 404, 427, 516, 852, 971)
fn = list.files(fullDirNames[1], full.names = TRUE)[indx]
sampleEmail = sapply(fn, readLines)        

msg = sampleEmail[[1]]
which(msg == "")[1]

match("", msg)

splitPoint = match("", msg)

msg[ (splitPoint - 2):(splitPoint + 6) ]

header = msg[1:(splitPoint-1)]
body = msg[ -(1:splitPoint) ]

splitMessage = function(msg) {
    splitPoint = match("", msg)
    header = msg[1:(splitPoint-1)]
    body = msg[ -(1:splitPoint) ]
    return(list(header = header, body = body))
}

sampleSplit = lapply(sampleEmail, splitMessage)

header = sampleSplit[[1]]$header
grep("Content-Type", header)

grep("multi", tolower(header[46]))

header[46]

headerList = lapply(sampleSplit, function(msg) msg$header)
CTloc = sapply(headerList, grep, pattern = "Content-Type")
CTloc

sapply(headerList, function(header) {
    CTloc = grep("Content-Type", header)
    if (length(CTloc) == 0) return(NA)
    CTloc
})

hasAttach = sapply(headerList, function(header) {
    CTloc = grep("Content-Type", header)
    if (length(CTloc) == 0) return(FALSE)
    grepl("multi", tolower(header[CTloc])) 
})

hasAttach

header = sampleSplit[[6]]$header
boundaryIdx = grep("boundary=", header)
header[boundaryIdx]

sub(".*boundary=\"(.*)\";.*", "\\1", header[boundaryIdx])

header2 = headerList[[9]]
boundaryIdx2 = grep("boundary=", header2)
header2[boundaryIdx2]

sub('.*boundary="(.*)";.*', "\\1", header2[boundaryIdx2])

boundary2 = gsub('"', "", header2[boundaryIdx2])

sub(".*boundary= *(.*);?.*", "\\1", boundary2)

boundary = gsub('"', "", header[boundaryIdx])
sub(".*boundary= *(.*);?.*", "\\1", boundary)

sub(".*boundary= *([^;]*);?.*", "\\1", boundary)

getBoundary = function(header) {
    boundaryIdx = grep("boundary=", header)
    boundary = gsub('"', "", header[boundaryIdx])
    gsub(".*boundary= *([^;]*);?.*", "\\1", boundary)
}

sampleSplit[[6]]$body

boundary = getBoundary(headerList[[15]]) 
body = sampleSplit[[15]]$body

bString = paste("--", boundary, sep = "")
bStringLocs = which(bString == body)
bStringLocs

eString = paste("--", boundary, "--", sep = "")
eStringLoc = which(eString == body)
eStringLoc

msg = body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)]
tail(msg)

msg = c(msg, body[ (eStringLoc + 1) : length(body) ])
tail(msg)

dropAttach = function(body, boundary){
    
    bString = paste("--", boundary, sep = "")
    bStringLocs = which(bString == body)
    
    if (length(bStringLocs) <= 1) return(body)
    
    eString = paste("--", boundary, "--", sep = "")
    eStringLoc = which(eString == body)
    if (length(eStringLoc) == 0) 
        return(body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)])
    
    n = length(body)
    if (eStringLoc < n) 
        return( body[ c( (bStringLocs[1] + 1) : (bStringLocs[2] - 1), 
                         ( (eStringLoc + 1) : n )) ] )
    
    return( body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1) ])
}

head(sampleSplit[[1]]$body)

msg = sampleSplit[[3]]$body
head(msg)

msg[ c(1, 3, 26, 27) ]

cleanMsg = tolower(gsub("[[:punct:]0-9[:blank:]]+", " ", msg))
cleanMsg[ c(1, 3, 26, 27) ]

words = unlist(strsplit(cleanMsg, "[[:blank:]]+"))

words = words[ nchar(words) > 1 ]

head(words)

cleanText =
    function(msg)   {
        tolower(gsub("[[:punct:]0-9[:space:][:blank:]]+", " ", msg))
    }

findMsgWords = 
    function(msg) {
        if(is.null(msg))
            return(character())
        
        words = unique(unlist(strsplit(cleanText(msg), "[[:blank:]\t]+")))
        
        # drop empty and 1 letter words
        words = words[ nchar(words) > 1]
        invisible(words)
    }

processAllWords = function(dirName)
{
    # read all files in the directory
    fileNames = list.files(dirName, full.names = TRUE)
    # drop files that are not email, i.e., cmds
    notEmail = grep("cmds$", fileNames)
    if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
    
    messages = lapply(fileNames, readLines, encoding = "latin1")
    
    # split header and body
    emailSplit = lapply(messages, splitMessage)
    # put body and header in own lists
    bodyList = lapply(emailSplit, function(msg) msg$body)
    headerList = lapply(emailSplit, function(msg) msg$header)
    rm(emailSplit)
    
    # determine which messages have attachments
    hasAttach = sapply(headerList, function(header) {
        CTloc = grep("Content-Type", header)
        if (length(CTloc) == 0) return(0)
        multi = grep("multi", tolower(header[CTloc])) 
        if (length(multi) == 0) return(0)
        multi
    })
    
    hasAttach = which(hasAttach > 0)
    
    # find boundary strings for messages with attachments
    boundaries = sapply(headerList[hasAttach], getBoundary)
    
    # drop attachments from message body
    bodyList[hasAttach] = mapply(dropAttach, bodyList[hasAttach], 
                                 boundaries, SIMPLIFY = FALSE)
    
    # extract words from body
    msgWordsList = lapply(bodyList, findMsgWords)
    
    invisible(msgWordsList)
}
msgWordsList = lapply(fullDirNames, processAllWords)  
########################################################################
removeStopWords =
    function(x, stopWords)
    {
        stopWords <- stopwords()
        if(is.character(x))
            setdiff(x, stopWords)
        else if(is.list(x))
            lapply(x, removeStopWords, stopWords)
        else x
    }

msgWordsList = lapply(msgWordsList, removeStopWords, stopWords)  
########################################################################
numMsgs = sapply(msgWordsList, length)
numMsgs

isSpam = rep(c(FALSE, FALSE, FALSE, TRUE, TRUE), numMsgs)

msgWordsList = unlist(msgWordsList, recursive = FALSE)

numEmail = length(isSpam)
numSpam = sum(isSpam)
numHam = numEmail - numSpam

set.seed(418910)

testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

testMsgWords = c((msgWordsList[isSpam])[testSpamIdx],
                 (msgWordsList[!isSpam])[testHamIdx] )
trainMsgWords = c((msgWordsList[isSpam])[ - testSpamIdx], 
                  (msgWordsList[!isSpam])[ - testHamIdx])

testIsSpam = rep(c(TRUE, FALSE), 
                 c(length(testSpamIdx), length(testHamIdx)))
trainIsSpam = rep(c(TRUE, FALSE), 
                  c(numSpam - length(testSpamIdx), 
                    numHam - length(testHamIdx)))

bow = unique(unlist(trainMsgWords))

length(bow)

spamWordCounts = rep(0, length(bow))

names(spamWordCounts) = bow

tmp = lapply(trainMsgWords[trainIsSpam], unique)
tt = table( unlist(tmp) )
spamWordCounts[ names(tt) ] = tt

computeFreqs =
    function(wordsList, spam, bow = unique(unlist(wordsList)))
    {
        # create a matrix for spam, ham, and log odds
        wordTable = matrix(0.5, nrow = 4, ncol = length(bow), 
                           dimnames = list(c("spam", "ham", 
                                             "presentLogOdds", 
                                             "absentLogOdds"),  bow))
        
        # For each spam message, add 1 to counts for words in message
        counts.spam = table(unlist(lapply(wordsList[spam], unique)))
        wordTable["spam", names(counts.spam)] = counts.spam + .5
        
        # Similarly for ham messages
        counts.ham = table(unlist(lapply(wordsList[!spam], unique)))  
        wordTable["ham", names(counts.ham)] = counts.ham + .5  
        
        
        # Find the total number of spam and ham
        numSpam = sum(spam)
        numHam = length(spam) - numSpam
        
        # Prob(word|spam) and Prob(word | ham)
        wordTable["spam", ] = wordTable["spam", ]/(numSpam + .5)
        wordTable["ham", ] = wordTable["ham", ]/(numHam + .5)
        
        # log odds
        wordTable["presentLogOdds", ] = 
            log(wordTable["spam",]) - log(wordTable["ham", ])
        wordTable["absentLogOdds", ] = 
            log((1 - wordTable["spam", ])) - log((1 -wordTable["ham", ]))
        
        invisible(wordTable)
    }

trainTable = computeFreqs(trainMsgWords, trainIsSpam)

newMsg = testMsgWords[[1]]

newMsg = newMsg[!is.na(match(newMsg, colnames(trainTable)))]

present = colnames(trainTable) %in% newMsg

sum(trainTable["presentLogOdds", present]) + 
    sum(trainTable["absentLogOdds", !present])

newMsg = testMsgWords[[ which(!testIsSpam)[1] ]]
newMsg = newMsg[!is.na(match(newMsg, colnames(trainTable)))]
present = (colnames(trainTable) %in% newMsg)
sum(trainTable["presentLogOdds", present]) + 
    sum(trainTable["absentLogOdds", !present])

computeMsgLLR = function(words, freqTable) 
{
    # Discards words not in training data.
    words = words[!is.na(match(words, colnames(freqTable)))]
    
    # Find which words are present
    present = colnames(freqTable) %in% words
    
    sum(freqTable["presentLogOdds", present]) +
        sum(freqTable["absentLogOdds", !present])
}

testLLR = sapply(testMsgWords, computeMsgLLR, trainTable)

tapply(testLLR, testIsSpam, summary)

#pdf("SP_Boxplot.pdf", width = 6, height = 6)
spamLab = c("ham", "spam")[1 + testIsSpam]
boxplot(testLLR ~ spamLab, ylab = "Log Likelihood Ratio",
        #  main = "Log Likelihood Ratio for Randomly Chosen Test Messages",
        ylim=c(-500, 500))
#dev.off()

typeIErrorRate = 
    function(tau, llrVals, spam)
    {
        classify = llrVals > tau
        sum(classify & !spam)/sum(!spam)
    }

typeIIErrorRate = 
    function(tau, llrVals, spam)
    {
        classify = llrVals < tau
        sum(classify & spam)/sum(spam)
    }

typeIErrorRate(-43, testLLR,testIsSpam)
typeIIErrorRate(-43, testLLR,testIsSpam)

typeIErrorRates = 
    function(llrVals, isSpam) 
    {
        o = order(llrVals)
        llrVals =  llrVals[o]
        isSpam = isSpam[o]
        
        idx = which(!isSpam)
        N = length(idx)
        list(error = (N:1)/N, values = llrVals[idx])
    }

typeIIErrorRates = function(llrVals, isSpam) {
    
    o = order(llrVals)
    llrVals =  llrVals[o]
    isSpam = isSpam[o]
    
    idx = which(isSpam)
    N = length(idx)
    list(error = (1:(N))/N, values = llrVals[idx])
}  

xI = typeIErrorRates(testLLR, testIsSpam)
xII = typeIIErrorRates(testLLR, testIsSpam)
tau01 = round(min(xI$values[xI$error <= 0.01]))
t2 = max(xII$error[ xII$values < tau01 ])

#pdf("LinePlotTypeI+IIErrors.pdf", width = 8, height = 6)

library(RColorBrewer)
cols = brewer.pal(9, "Set1")[c(3, 4, 5)]
plot(xII$error ~ xII$values,  type = "l", col = cols[1], lwd = 3,
     xlim = c(-300, 250), ylim = c(0, 1),
     xlab = "Log Likelihood Ratio Values", ylab="Error Rate")
points(xI$error ~ xI$values, type = "l", col = cols[2], lwd = 3)
legend(x = 50, y = 0.4, fill = c(cols[2], cols[1]),
       legend = c("Classify Ham as Spam", 
                  "Classify Spam as Ham"), cex = 0.8,
       bty = "n")
abline(h=0.01, col ="grey", lwd = 3, lty = 2)
text(-250, 0.05, pos = 4, "Type I Error = 0.01", col = cols[2])

mtext(tau01, side = 1, line = 0.5, at = tau01, col = cols[3])
segments(x0 = tau01, y0 = -.50, x1 = tau01, y1 = t2, 
         lwd = 2, col = "grey")
text(tau01 + 20, 0.05, pos = 4,
     paste("Type II Error = ", round(t2, digits = 2)), 
     col = cols[1])

#dev.off()

k = 5
numTrain = length(trainMsgWords)
partK = sample(numTrain)
tot = k * floor(numTrain/k)
partK = matrix(partK[1:tot], ncol = k)

testFoldOdds = NULL
for (i in 1:k) {
    foldIdx = partK[ , i]
    trainTabFold = computeFreqs(trainMsgWords[-foldIdx], trainIsSpam[-foldIdx])
    testFoldOdds = c(testFoldOdds, 
                     sapply(trainMsgWords[ foldIdx ], computeMsgLLR, trainTabFold))
}

testFoldSpam = NULL
for (i in 1:k) {
    foldIdx = partK[ , i]
    testFoldSpam = c(testFoldSpam, trainIsSpam[foldIdx])
}

xFoldI = typeIErrorRates(testFoldOdds, testFoldSpam)
xFoldII = typeIIErrorRates(testFoldOdds, testFoldSpam)
tauFoldI = round(min(xFoldI$values[xFoldI$error <= 0.01]))
tFold2 = xFoldII$error[ xFoldII$values < tauFoldI ]

smallNums = rep((1/2)^40, 2000000)
largeNum = 10000

print(sum(smallNums), digits = 20)

print(largeNum + sum(smallNums), digits = 20)

for (i in 1:length(smallNums)) {
    largeNum = largeNum + smallNums[i]
}
print(largeNum, digits = 20)


# Problem 3 -------------------------------------------------

library(XML)
u = paste("http://real-chart.finance.yahoo.com/table.csv","s=PEP&a=5&b=1&c=1972&d=10&e=9&f=2016&g=d",sep = "?")
download.file(u,"PEP.csv")
u = paste("http://real-chart.finance.yahoo.com/table.csv","s=KO&a=0&b=2&c=1962&d=10&e=9&f=2016&g=d",sep = "?")
download.file(u,"KO.csv")
u = paste("http://real-chart.finance.yahoo.com/table.csv","s=DPS&a=4&b=7&c=2008&d=10&e=8&f=2016&g=d",sep = "?")
download.file(u,"DPS.csv")
# Download CSV files

readData =
    #read the data and convert the Date column to an object of class Date.  
    function(fileName, dateFormat = c("%Y-%m-%d", "%Y/%m/%d"), ...)
    {
        data = read.csv(fileName, header = TRUE, 
                        stringsAsFactors = FALSE, ...)
        for(fmt in dateFormat) {
            tmp = as.Date(data$Date, fmt)
            if(all(!is.na(tmp))) {
                data$Date = tmp
                break
            }
        }
        
        data[ order(data$Date), ]
    }

pep <- readData("PEP.csv")
ko <- readData("KO.csv")
dps <- readData("DPS.csv")

combine2Stocks = 
    function(a, b, stockNames = c(deparse(substitute(a)), 
                                  deparse(substitute(b))))
    {
        rr = intersect(a$Date, b$Date)
        a.sub=a[which(a$Date %in% rr),]
        b.sub=b[which(b$Date %in% rr),]
        structure(data.frame(as.Date(a.sub$Date), 
                             a.sub$Adj.Close, 
                             b.sub$Adj.Close),
                  names = c("Date", stockNames))
    }

overlap = combine2Stocks(pep, ko)
r = overlap$pep/overlap$ko

plotRatio =
    function(r, k = 1, date = seq(along = r), ...)
    {
        plot(date, r, type = "l", ...)
        abline(h = c(mean(r), 
                     mean(r) + k * sd(r), 
                     mean(r) - k * sd(r)), 
               col = c("darkgreen", rep("red", 2*length(k))), 
               lty = "dashed")
    }

plotRatio(r, k = 1, overlap$Date, col = "lightgray", xlab = "Date", ylab = "Ratio")
length(r)
# 11212

findNextPosition =
    # Check they are increasing and correctly offset
    function(ratio, startDay = 1, k = 1, 
             m = mean(ratio), s = sd(ratio))
    {
        up = m + k *s
        down = m - k *s
        
        if(startDay > 1)
            ratio = ratio[ - (1:(startDay-1)) ]
        
        isExtreme = ratio >= up | ratio <= down
        
        if(!any(isExtreme))
            return(integer())
        
        start = which(isExtreme)[1]
        backToNormal = if(ratio[start] > up)
            ratio[ - (1:start) ] <= m
        else
            ratio[ - (1:start) ] >= m
        
        # return either the end of the position or the index 
        # of the end of the vector.
        # Could return NA for not ended, i.e. which(backToNormal)[1]
        # for both cases. But then the caller has to interpret that.
        
        end = if(any(backToNormal))
            which(backToNormal)[1] + start
        else
            length(ratio)
        
        c(start, end) + startDay - 1 
    }


k = 1
a = findNextPosition(r, k = k)         #1 1095
b = findNextPosition(r, a[2], k = k)   #2186 2661
c = findNextPosition(r, b[2], k = k)   #3886 4827
d = findNextPosition(r, c[2], k = k)   #4996 5175
e = findNextPosition(r, d[2], k = k)   #5565 5175
f = findNextPosition(r, e[2], k = k)   #8148 9777
g = findNextPosition(r, f[2], k = k)   #10902 11212


symbols(overlap$Date[a[1]],r[a[1]],circles = 60,fg = "darkgreen", add=TRUE, inches = FALSE)
symbols(overlap$Date[a[2]],r[a[2]],circles = 60,fg = "red", add=TRUE, inches = FALSE)

symbols(overlap$Date[b[1]],r[b[1]],circles = 60,fg = "darkgreen", add=TRUE, inches = FALSE)
symbols(overlap$Date[b[2]],r[b[2]],circles = 60,fg = "red", add=TRUE, inches = FALSE)

showPosition = 
    function(days, ratios, radius = 100)
    {
        symbols(days, ratios, circles = rep(radius, 2), 
                fg = c("darkgreen", "red"), add = TRUE, inches = FALSE)
    }

showPosition(overlap$Date[a],r[a])
showPosition(overlap$Date[b],r[b])
showPosition(overlap$Date[c],r[c])
showPosition(overlap$Date[d],r[d])
showPosition(overlap$Date[e],r[e])
showPosition(overlap$Date[f],r[f])
showPosition(overlap$Date[g],r[g])

getPositions =
    function(ratio, k = 1, m = mean(ratio), s = sd(ratio))
    {
        when = list()
        cur = 1
        
        while(cur < length(ratio)) {
            tmp = findNextPosition(ratio, cur, k, m, s)
            if(length(tmp) == 0)  # done
                break
            when[[length(when) + 1]] = tmp
            if(is.na(tmp[2]) || tmp[2] == length(ratio))
                break
            cur = tmp[2]
        }
        
        when
    }
k = 1
pos <- getPositions(r,k) # Find all positions
plotRatio(r, k, overlap$Date, col = "lightgray", xlab = "Date", ylab = "Ratio")
invisible(lapply(pos, function(p) showPosition(overlap$Date[p],r[p])))

k = 0.5
pos <- getPositions(r,k) 
plotRatio(r, k, overlap$Date, col = "lightgray", xlab = "Date", ylab = "Ratio")
invisible(lapply(pos, function(p) showPosition(overlap$Date[p],r[p])))


positionProfit =
    #  r = overlap$pep/overlap$ko
    #  pos = getPositions(r, k)
    #  positionProfit(pos[[1]], overlap$pep, overlap$ko)
    function(pos, stockPriceA, stockPriceB, 
             ratioMean = mean(stockPriceA/stockPriceB), 
             p = .0002, byStock = FALSE)
    {
        if(is.list(pos)) {
            ans = sapply(pos, positionProfit, 
                         stockPriceA, stockPriceB, ratioMean, p, byStock)
            if(byStock)
                rownames(ans) = c("A", "B", "commission")
            return(ans)
        }
        # prices at the start and end of the positions
        priceA = stockPriceA[pos]
        priceB = stockPriceB[pos]
        
        # how many units can we by of A and B with $1
        unitsOfA = 1/priceA[1]
        unitsOfB = 1/priceB[1]
        
        # The dollar amount of how many units we would buy of A and B
        # at the cost at the end of the position of each.
        amt = c(unitsOfA * priceA[2], unitsOfB * priceB[2])
        
        # Which stock are we selling
        sellWhat = if(priceA[1]/priceB[1] > ratioMean) "A" else "B"
        
        profit = if(sellWhat == "A") 
            c((1 - amt[1]),  (amt[2] - 1), - p * sum(amt))
        else 
            c( (1 - amt[2]),  (amt[1] - 1),  - p * sum(amt))
        
        if(byStock)
            profit
        else
            sum(profit)
    }

prof = positionProfit(pos, overlap$pep, overlap$ko, mean(r))
prof
summary(prof)

i = 1:floor(nrow(overlap)/2)
train = overlap[i, ]
test = overlap[ - i, ]
# Divide the data half to a training period and and half to a test period

r.train = train$pep/train$ko
r.test = test$pep/test$ko

k.max = max((r.train - mean(r.train))/sd(r.train))
k.min = min((abs(r.train - mean(r.train))/sd(r.train)))
ks = seq(k.min, k.max, length = 1000)
m  = mean(r.train)

profits =
    sapply(ks,
           function(k) {
               pos = getPositions(r.train, k)
               sum(positionProfit(pos, train$pep, train$ko, 
                                  mean(r.train)))
           })

plot(ks, profits, type = "l", xlab = "k", ylab = "Profit")
tmp.k = ks[profits == max(profits)]  
# [1] 0.03661572

pos = getPositions(r.train, tmp.k[1])
all(sapply(tmp.k[-1],
           function(k) 
               identical(pos, getPositions(r.train, k))))

k.star = mean(ks[profits == max(profits)])


pos = getPositions(r.test, k.star, mean(r.train), sd(r.train))
testProfit = sum(positionProfit(pos, test$pep, test$ko)) 
testProfit
# [1] 1.548414

# PEP vs DPS
overlap_pd <- combine2Stocks(pep, dps)
r_pd <- overlap_pd$ko/overlap_pd$dps

i = 1:floor(nrow(overlap_pd)/2)
train = overlap_pd[i, ]
test = overlap_pd[ - i, ]
# Divide the data half to a training period and and half to a test period

r.train = train$pep/train$dps
r.test = test$pep/test$dps
sd(r.train)
k.max = min(max((r.train - mean(r.train))/sd(r.train)), 
            (mean(r.train)-min(r.train))/sd(r.train))
#k.max = max((r.train - mean(r.train))/sd(r.train))
k.min = max(0.1, min((abs(r.train - mean(r.train))/sd(r.train))))
ks = seq(k.min, k.max, length = 1000)
m  = mean(r.train)

profits =
    sapply(ks,
           function(k) {
               pos = getPositions(r.train, k)
               sum(positionProfit(pos, train$pep, train$dps, 
                                  mean(r.train)))
           })

plot(ks, profits, type = "l", xlab = "k", ylab = "Profit")
tmp.k = ks[profits == max(profits)]  
#[1] 1.061245 1.062219 1.063193 1.064167 1.065141 1.066115 1.067089 1.068063 1.069037 1.070011
#[11] 1.070984 1.071958

k.star = mean(ks[profits == max(profits)])
# [1] 1.066602

pos = getPositions(r.test, k.star)
testProfit = sum(positionProfit(pos, test$pep, test$dps)) 
testProfit

plotRatio(r.test, k.star, test$Date, col = "lightgray", xlab = "Date", ylab = "Ratio")

# KO vs DPS
overlap_kd <- combine2Stocks(ko, dps)
r_kd <- overlap_kd$ko/overlap_kd$dps

i = 1:floor(nrow(overlap_kd)/2)
train = overlap_kd[i, ]
test = overlap_kd[ - i, ]
# Divide the data half to a training period and and half to a test period

r.train = train$ko/train$dps
r.test = test$ko/test$dps

k.max = max((r.train - mean(r.train))/sd(r.train))
k.min = max(0.1, min((abs(r.train - mean(r.train))/sd(r.train))))
ks = seq(k.min, k.max, length = 1000)
m  = mean(r.train)

profits =
    sapply(ks,
           function(k) {
               pos = getPositions(r.train, k)
               sum(positionProfit(pos, train$ko, train$dps, 
                                  mean(r.train)))
           })

plot(ks, profits, type = "l", xlab = "k", ylab = "Profit")
tmp.k = ks[profits == max(profits)]  
# [1] 0.4365644 0.4402227

k.star = mean(ks[profits == max(profits)])
# [1] 0.4383935

pos = getPositions(r.test, k.star)
testProfit = sum(positionProfit(pos, test$ko, test$dps)) 
testProfit

plotRatio(r.test, k.star, test$Date, col = "lightgray", xlab = "Date", ylab = "Ratio")
