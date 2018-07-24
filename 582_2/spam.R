library(NLP)
library(tm)
stopWords <- stopwords()

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

typeIErrorRate(0, testLLR,testIsSpam)

typeIErrorRate(-20, testLLR,testIsSpam)

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