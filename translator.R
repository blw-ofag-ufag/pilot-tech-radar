#' ---
#' title: Script to automatically translate a CSV file
#' author: Damian Oswald
#' date: 2023-11-20
#' ---

#' Function to print out progress
progressbar <- function (i, n, message = "") {
  w <- (options("width")$width - 6 - nchar(message))/n
  cat("\r", message, " [", strrep("=", ceiling(i * w)), ">", 
      strrep("-", floor((n - i) * w)), "] ", paste0(format(round(i/n * 100, 1), nsmall = 1), "%  "), sep = "")
}

#' Set script parameters
languages <- c("French", "German", "Italian")

#' Fix random processes
set.seed(1)

#' Attach packages to search path.
library(openai)

#' Set secret OpenAI API key
Sys.setenv(OPENAI_API_KEY = "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")

#' Read English data
data <- read.csv("data.csv", sep = ";")

#' Define the columns we want to translate
relevant_columns <- c(1,4)

#' define which variables are factors
is_factor <- c(2,3)

sector_translation <- data.frame(
  English = c("artificial intelligence", "communication", "data", "infrastructure", "smart farming"),
  German = c("künstliche intelligenz", "kommunikation", "daten", "infrastruktur", "smart farming"),
  French = c("intelligence artificielle", "communication", "données", "infrastructure", "smart farming"),
  Italian = c("intelligenza artificiale", "comunicazione", "dati", "infrastruttura", "smart farming"))
status_translation <- data.frame(
  English = c("launched on market", "maturity", "experimentation", "applied research", "basic research"),
  German = c("auf den markt gebracht", "reife", "experimentierung", "angewandte Forschung", "grundlagenforschung"),
  French = c("lancé sur le marché", "maturité", "expérimentation", "recherche appliquée", "recherche fondamentale"),
  Italian = c("lanciato sul mercato", "maturità", "sperimentazione", "ricerca applicata", "ricerca di base")
)

#' Loop through every chosen languange
for (language in "German") {
  
  # copy data frame and rename it
  X <- data
  
  # loop over columns to be translated
  for (j in relevant_columns) {
    
    # loop over every row
    for (i in 1:nrow(data)) {
      
      # save prompt
      prompt <- paste0("Please loosely translate the following expression to ", language, " such that it reads nicely. Make sure to only answer with the valid translation and nothing else. Keep words that are uncommon in ", language, "in the original, but make them italic.\n\n", X[i,j])
      
      # ask gpt-4 for translation
      translation <- openai::create_chat_completion(
        model = "gpt-4",
        messages = list(list(role = "user",
                             content = prompt))
      )
      
      # save translation in the data frame
      X[i,j] <- translation$choices$message.content
      
      # print out progress
      progressbar((which(j==relevant_columns)-1)*nrow(data)+i, length(relevant_columns)*nrow(data), paste("Translating to", language))
    }
  }
  
  # copy data frame and rename it
  assign(paste0("data_", language), X)
  
}





