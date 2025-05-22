# contexualising sentiment analysis to account for negation
# igraph
# non-paired correlations

# generic
library(tidyverse)
library(tidytext)

# sources
library(janeaustenr)
library(gutenbergr)

# for graphs
library(igraph)
library(ggraph)
library(grid)

# non-paired correlations
library(widyr)

# data
austen_bgridausten_bigrams <- austen_books() |>
  unnest_tokens(output = bigram,
                input = text, 
                token = "ngrams",
                n = 2) |>
  filter(!is.na(bigram)) 

bigrams_separated <- austen_bigrams  |>
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated |>
  filter(!word1 %in% stop_words$word) |>
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered |>
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")
