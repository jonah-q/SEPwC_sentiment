suppressPackageStartupMessages({
  library(sentimentr)
  library(tidytext)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(argparse)
  library(ggpubr)
  library(stringr)            
})

library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(textclean)
library(textdata)
library(sentimentr)

# https://cran.r-project.org/web/packages/textclean/readme/README.html
# https://ladal.edu.au/sentiment.html

# Loads correct columns and cleans data #

load_data <- function(filename) {
  read.csv(filename) %>%
    select(id, created_at, language, content) %>%
    filter(language == "en") %>% 
    mutate(id = as.character(id),
           created_at = as_datetime(created_at),
           content = replace_html(content),
           content = str_replace_all(content, "# ", "#"),
           content = replace_hash(content),
           content = replace_white(content))
}

# Tokenising words from toots for analysis
# Method used from:
# https://www.stephaniehicks.com/jhustatcomputing2022/posts/2022-10-13-working-with-text-sentiment-analysis/ 

word_analysis <- function(toot_data, emotion) {
  toot_data %>%
    ungroup() %>% 
    unnest_tokens(output = word,
                  input = content,
                  token = "words") %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("nrc"), 
               by = "word", 
               relationship = "many-to-many") %>%      # Filters to words showing sentiment
    select(id, created_at, language, word, sentiment) %>% 
    filter(sentiment == emotion)
}

# Function to measure sentiment using "nrc", "bing", and "afinn" methods. Storing results as a list then binding.
sentiment_analysis <- function(toot_data, methods = c("afinn", "nrc", "bing")) {
  results <- list()
  for (method in methods) {
    method_result <- toot_data %>%
      ungroup() %>%
      unnest_tokens(output = word,
                    input = content,
                    token = "words") %>%
      anti_join(stop_words) %>%
      inner_join(get_sentiments(method),
                 by = "word",
                 relationship = "many-to-many") %>%
      mutate(method = method)
    results[[method]] <- method_result
  }
  bind_rows(results)
}

main <- function(args) {
  toot_data <- load_data(args$filename)
  word_data <- word_analysis(toot_data, args$emotion)
  top_10_words <- word_data %>% 
    count(word, sort = TRUE)
  print(head(top_10_words, no = 10))
  sentiment_data <- sentiment_analysis(toot_data)
}


if(sys.nframe() == 0) {

  # main program, called via Rscript
  parser = ArgumentParser(
                    prog="Sentiment Analysis",
                    description="Analyse toots for word and sentence sentiments"
                    )
  parser$add_argument("filename",
                    help="the file to read the toots from")
  parser$add_argument("--emotion",
                      default="anger",
                      help="which emotion to search for")
  parser$add_argument('-v', '--verbose',
                    action='store_true',
                    help="Print progress")
  parser$add_argument('-p', '--plot',
                    help="Plot something. Give the filename")
  
  args = parser$parse_args()  
  main(args)
}
