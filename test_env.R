library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(textclean)
library(lubridate)



test_data <- "data/toots.csv"

# Loads correct columns and cleans data

load_data <- function(filename) {
  read.csv(filename) %>%
    select(id, created_at, language, content) %>%
    filter(language == "en") %>% 
    mutate(id = as.character(id)) %>%
    mutate(created_at = as_datetime(created_at)) %>% 
    mutate(content = replace_html(content)) %>%
    mutate(content = str_replace_all(content, "# ", "#")) %>% 
    mutate(content = replace_hash(content)) %>%
    mutate(content = replace_white(content))
}

cleaned_data <- load_data(test_data)

head(cleaned_data)


# Tokenising words from toots for analysis
# Method used from:
# https://www.stephaniehicks.com/jhustatcomputing2022/posts/2022-10-13-working-with-text-sentiment-analysis/ 


word_analysis <- function(toot_data, emotion) {
  cleaned_data %>% 
     unnest_tokens(output = word,
                   input = content,
                   token = "words") %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("nrc"), 
               by = "word", 
               relationship = "many-to-many") # Filters to words showing sentiment
}

word_data <- word_analysis(cleaned_data, "anger")
head(word_data, n=10)

filter(sentiment == emotion) %>% 
  count(word, sort = TRUE)  

