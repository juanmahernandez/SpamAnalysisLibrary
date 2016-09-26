#' Building model for Spam Analysis.
#'
#' @param spamData a data frame with the words specified, and one column named type where is stored if spam or not
#' @return a model for predicting the clasification based on the input
#' @author Juan Manuel Hernandez
#' @details
#' This function creates a model depending on the input, and save it in the file
#' model.rda
#' @import caret kernlab beepr randomForest
#' @export
buildModel <- function(spamData) {

    inTrain <- createDataPartition(y = spamData$type, p=0.6, list=FALSE)

    training <- spamData[inTrain, ]
    testing <- spamData[-inTrain, ]

    set.seed(1234)
    modfitSpam <- train(type ~ ., data = training, method = "rf",
                        trControl = trainControl(method = "cv"),
                        preProcess = c("center", "scale"))

    pr <- predict(modfitSpam, newdata = testing)
    confusionMatrix(pr, testing$type)

    save(modfitSpam, file = "model/spamModel.rda")
    beep(4)
}


#' Predicting if it's spam or not based on the string and model
#' @param stringToCheck a sentence to predict if it's spam or not
#' @return a string with the result
#' @author Juan Manuel Hernandez
#' @details
#' This function predict from the model, loading the model from a file named
#' model.rda, the classification of the sentence
#' @import dplyr stringi
#' @export
checkSpam <- function(stringToCheck) {

    if(nchar(stringToCheck) == 0) {
        "Please insert a sentence"
    }
    else if(!file.exists("model/spamModel.rda")) {
        "No model loaded"
    }
    else
    {
        load("model/spamModel.rda")

        set.seed(12345)
        getCharFreq <- function(strPattern, paramStringToCheck, ignoreCase = TRUE) {
            resultReg <- gregexpr(strPattern, paramStringToCheck, ignore.case = ignoreCase, perl = F, useBytes = F)
            wordList <- data.frame(word = unlist(regmatches(paramStringToCheck, resultReg)))
            wordList
        }

        stringToCheckWords <- tolower(stringToCheck)
        wordList <- getCharFreq("\\w+", stringToCheckWords)
        totalWords <- nrow(wordList)

        totalChars <- nchar(stringToCheck)

        listSentenceWords <- wordList %>% group_by(word) %>% summarize(count = (n()/totalWords) * 100)

        stringToPredict <- c()
        for (i in 1:length(modfitSpam$coefnames))
        {
            if(length(listSentenceWords[listSentenceWords$word == modfitSpam$coefnames[i],]$count) == 0)
                stringToPredict <- cbind(stringToPredict, 0)
            else
                stringToPredict <- cbind(stringToPredict, listSentenceWords[listSentenceWords$word == modfitSpam$coefnames[i],]$count)
        }
        colnames(stringToPredict)<- make.names(modfitSpam$coefnames)

        # Numbers
        # modfitSpam$coefnames[grep("^num*", modfitSpam$coefnames)]

        ## num3d
        stringToPredict[, "num3d"] = (nrow(getCharFreq("(3d)+", stringToCheckWords)) / totalWords) * 100

        ## num000
        stringToPredict[, "num000"] = (nrow(getCharFreq("(000)+", stringToCheckWords)) / totalWords) * 100

        ## num650
        stringToPredict[, "num650"] = (nrow(getCharFreq("(650)+", stringToCheckWords)) / totalWords) * 100

        ## num857
        stringToPredict[, "num857"] = (nrow(getCharFreq("(857)+", stringToCheckWords)) / totalWords) * 100

        ## num415
        stringToPredict[, "num415"] = (nrow(getCharFreq("(415)+", stringToCheckWords)) / totalWords) * 100

        ## num85
        stringToPredict[, "num85"] = (nrow(getCharFreq("(85)+", stringToCheckWords)) / totalWords) * 100

        ## num1999
        stringToPredict[, "num1999"] = (nrow(getCharFreq("(1999)+", stringToCheckWords)) / totalWords) * 100


        # Special Characters
        #modfitSpam$coefnames[grep("^char*", modfitSpam$coefnames)]

        # charSemicolon
        stringToPredict[, "charSemicolon"] = (nrow(getCharFreq("(;)+", stringToCheckWords)) / totalChars) * 100

        # charRoundbracket
        stringToPredict[, "charRoundbracket"] = (nrow(getCharFreq("(\\())+", stringToCheckWords)) / totalChars) * 100

        # charSquarebracket
        stringToPredict[, "charSquarebracket"] = (nrow(getCharFreq("(\\[)+", stringToCheckWords)) / totalChars) * 100

        # charExclamation
        stringToPredict[, "charExclamation"] = (nrow(getCharFreq("(!)+", stringToCheckWords)) / totalChars) * 100

        # charDollar
        stringToPredict[, "charDollar"] = (nrow(getCharFreq("(\\$)+", stringToCheckWords)) / totalChars) * 100

        # charHash
        stringToPredict[, "charHash"] = (nrow(getCharFreq("(#)+", stringToCheckWords)) / totalChars) * 100

        # Capital Characters
        # modfitSpam$coefnames[grep("^capital*", modfitSpam$coefnames)]
        capital <- getCharFreq("[A-Z]+", stringToCheck, FALSE)

        if(nrow(capital) > 0) {
            # capitalAve
            stringToPredict[, "capitalAve"] = mean(sapply(as.list(capital$word), stri_length))

            # capitalLong
            stringToPredict[, "capitalLong"] = max(sapply(as.list(capital$word), stri_length))

            # capitalTotal
            stringToPredict[, "capitalTotal"] = nrow(getCharFreq("[A-Z]", stringToCheck, FALSE))
        } else {
            # capitalAve
            stringToPredict[, "capitalAve"] = 0

            # capitalLong
            stringToPredict[, "capitalLong"] = 0

            # capitalTotal
            stringToPredict[, "capitalTotal"] = 0
        }

        pr <- predict(modfitSpam, newdata = stringToPredict)
        as.character(pr[1])
    }
}
