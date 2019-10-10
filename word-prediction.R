library(stringr)

# Very basic text normalization to apply to the raw data.
normalizeText <- function (lines) {
    lines <- sapply(lines, replace_non_ascii)
    lines <- sapply(lines, strip)
    lines
}

preprocessInput <- function(inputText) {
    tokens(normalizeText(inputText), remove_punct = TRUE)[[1]]
}

getCandidateWords <- function (inputWords, nGramModel, scoreMultiplier) {
    candidateFeature <- character()
    candidateScore <- numeric()
    size <- length(inputWords)
    if (size == 0) {
        candidate1Grams <- nGramModel[nGramModel$nGramSize == 1, ]
        candidateFeature <- candidate1Grams$feature
        candidateScore <- candidate1Grams$count / sum(candidate1Grams$count)
    } else {
        inputNGram <- nGramModel[nGramModel$nGramSize == size
                                 & nGramModel$feature == paste(inputWords[1:size],
                                                               collapse = " "), ]
        candidateNPlus1Grams <- nGramModel[nGramModel$nGramSize == (size + 1)
                                      & startsWith(nGramModel$feature,
                                                   paste0(inputNGram$feature, " ")), ]
        candidateFeature <- candidateNPlus1Grams$feature
        candidateScore <- candidateNPlus1Grams$count / inputNGram$count
    }
    candidateWord <- if (length(candidateFeature) == 0) {
        character()
    } else {
        word(candidateFeature, -1)
    }
    data.frame(feature = head(candidateWord, 10),
               score = head(round(candidateScore * scoreMultiplier, 4), 10))
}

predictNextWord <- (function() {
    nGramModel <- read.csv(file = 'word-prediction-dataset.csv',
                           colClasses = c("factor", "integer", "character", "integer")) %>%
        group_by(nGramSize, feature) %>%
        summarize(count = sum(count)) %>%
        arrange(desc(count))

    function (previousWords) {
        input <- preprocessInput(previousWords)
        alpha <- 0.2
        if (length(input) > 4) {
            last4Words <- paste(input[(length(input) - 3):length(input)], collapse = " ")
            predictNextWord(last4Words, nGramModel)
        } else {
            candidateWords <- data.frame(feature = character(), score = numeric())
            for (i in seq(0, length(input))) {
                candidateInputSize <- length(input) - i
                candidateInput <- if (candidateInputSize == 0) {
                    character()
                } else {
                    input[(length(input) - candidateInputSize + 1):length(input)]
                }
                candidateWords <- rbind(candidateWords,
                                        getCandidateWords(candidateInput, nGramModel, (alpha ^ i)))
            }
            candidateWords %>%
                group_by(feature) %>%
                summarize(score = sum(score)) %>%
                arrange(desc(score)) %>%
                head(5)
        }
    }
})()

predictNextWordForBenchmark <- function(previousWords) {
    head(predictNextWord(previousWords)$feature, 3)
}
