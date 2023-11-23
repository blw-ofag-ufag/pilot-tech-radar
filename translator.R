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
Sys.setenv(OPENAI_API_KEY = readLines("my-secret-API-key"))

#' Read English data
data <- read.csv(file.path("English", "data-English.csv"), sep = ";")

#' Define the columns we want to translate
relevant_columns <- c(1,4)

#' define which variables are factors
is_factor <- c("Sector","Status")

#' read the prepared translations for factor levels
factor_names <- read.csv("factor-translations.csv")

#' Loop through every chosen languange
for (language in languages) {
  
  # copy data frame and rename it
  X <- data
  
  # assign factors and change their names
  for (i in is_factor) {
    X[,i] <- factor(tolower(X[,i]),
                    levels = tolower(factor_names[factor_names[,"Language"]=="English" & factor_names[,"Variable"]==i,"Translation"]),
                    labels = tolower(factor_names[factor_names[,"Language"]==language & factor_names[,"Variable"]==i,"Translation"]))
  }
  
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
  
  # create a directory for the specific language
  if(!dir.exists(language)) dir.create(language)
  
  # write translated data frame as a csv
  write.table(X, file = file.path(language,paste0("data-",language,".csv")), row.names = FALSE, sep = ";")

}





