nGramModel <- fread(file = 'word-prediction-dataset.csv',
                    colClasses = c("integer", "character", "integer"))

score <- numeric(nrow(nGramModel))
names(score) <- "score"
nGramModel <- cbind(nGramModel, score)

#Calculate score as the frequency of a particular n-gram relative to the frequency of its (n-1)-gram
for (size in seq(1, 5)) {
    countNMinus1Gram <- if (size == 1) {
        nGramModel[nGramSize == size, sum(count)]
    } else {
        nMinus1Gram <- sub('(.*) .+$', '\\1', nGramModel[nGramSize == size, feature])
        sapply(nMinus1Gram, function(word) {
            nGramModel[nGramSize == (size - 1) & feature == word, count]
        })
    }
    nGramModel[nGramSize == size, ]$score <- nGramModel[nGramSize == size, count] / countNMinus1Gram
}

write.csv(nGramModel,
          file = 'word-prediction-dataset-scored.csv',
          row.names = FALSE)

nGramModel <- fread(file = 'word-prediction-dataset-scored.csv',
                    colClasses = c("integer", "character", "integer", "numeric"))

mostLikelyNextWords <- character(nrow(nGramModel))
names(mostLikelyNextWords) <- "mostLikelyNextWords"
nGramModel <- cbind(nGramModel, mostLikelyNextWords)


#Add nMinus1Gram to the model to facilitate the search
nMinus1Gram <- character(nrow(nGramModel))
names(nMinus1Gram) <- "nMinus1Gram"
nGramModel <- cbind(nGramModel, nMinus1Gram)
nGramModel$nMinus1Gram <- sub('(.*) .+$', '\\1', nGramModel$feature)
nGramModel[nGramSize == 1,]$nMinus1Gram <- ""

setkey(nGramModel, nGramSize, nMinus1Gram)

#Predict most likely next words of an n-gram based on the scores of its known (n+1)-grams
for (size in seq(1, 4)) {
    mostLikelyToFollow <- sapply(nGramModel[nGramSize == size, feature], function(word) {
        paste0(sub('.* (.+$)', '\\1',
                   na.omit(nGramModel[.((size + 1), word), feature, keyby = -score][1:5, feature])),
               collapse = " ")
    })
    nGramModel[nGramSize == size, ]$mostLikelyNextWords <- mostLikelyToFollow
}

write.csv(nGramModel,
          file = 'word-prediction-dataset-with-prediction.csv',
          row.names = FALSE)


#Keep only the variables that will be needed by the prediction algorithm.

#Remove count and nMinus1Gram variables
nGramModel <- nGramModel[, .(nGramSize, feature, score, mostLikelyNextWords)]

#Remove all n-grams that do not predict words, except for the five most frequent 1-grams
nGramModel <- nGramModel[mostLikelyNextWords != "", ]

#Remove scores except for 1-grams
nGramModel[nGramSize != 1, ]$score <- NA

write.csv(nGramModel,
          file = 'word-prediction-dataset-with-prediction-trimmed.csv',
          row.names = FALSE)
