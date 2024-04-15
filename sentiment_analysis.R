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

# https://cran.r-project.org/web/packages/textclean/readme/README.html

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

# https://ladal.edu.au/sentiment.html
  
word_analysis <- function(toot_data, emotion) {

    return()
}

sentiment_analysis <- function(toot_data) {

    return()

}

main <- function(args) {

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
