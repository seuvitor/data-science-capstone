library(stringr)
library(data.table)
library(quanteda)
library(dplyr)

removeNonAsciiCharacters <- function(lines) {
  gsub('[^ -~]', '', lines)
}

# Very basic text normalization to apply to the raw data.
normalizeText <- function (lines) {
  char_tolower(removeNonAsciiCharacters(lines))
}

preprocessInput <- function(inputText) {
  tokens(normalizeText(inputText), remove_punct = TRUE)[[1]]
}

predictNextWord <- (function() {
  nGramModel <- fread(file = 'word-prediction-dataset-with-prediction-trimmed.csv',
                      colClasses = c("integer", "character", "numeric", "character"))
  setkey(nGramModel, nGramSize, feature)
  
  function (previousWords) {
    
    input <- preprocessInput(previousWords)
    if (length(input) > 4) {
      last4Words <- paste(input[(length(input) - 3):length(input)], collapse = " ")
      predictNextWord(last4Words)
    } else {
      
      candidateWords <- data.frame(feature = character(), score = numeric())
      for (i in seq(0, length(input))) {
        candidateInputSize <- length(input) - i
        
        likelyNextWords <- if (candidateInputSize == 0) {
          head(nGramModel[nGramSize == 1, feature, keyby=-score]$feature, 3)
        } else {
          candidateInput <- input[(length(input) - candidateInputSize + 1):length(input)]
          candidateInputCollapsed <- paste0(candidateInput, collapse = " ")
          likelyNextWordsList <- strsplit(nGramModel[.(candidateInputSize,
                                                       candidateInputCollapsed),
                                                     nomatch = NULL]$mostLikelyNextWords, " ")
          if (length(likelyNextWordsList) == 1) {
            likelyNextWords <- likelyNextWordsList[[1]]
            likelyNextWords[!grepl("NA", likelyNextWords)]
          } else {
            character(0)
          }
        }
        for (word in likelyNextWords) {
          if (all(!grepl(word, candidateWords$feature))) {
            candidateWords <- rbind(candidateWords,
                                    data.frame(feature = word, score = 0))
          }
        }
        if (nrow(candidateWords) >= 3) {
          break
        }
      }
      candidateWords[1:3, ]
    }
  }
})()

predictNextWordForBenchmark <- function(previousWords) {
  head(predictNextWord(previousWords)$feature, 3)
}
