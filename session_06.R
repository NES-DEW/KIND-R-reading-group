# session_06.R

# install.packages("widyr")
library(widyr)
library(tidyverse)
library(tidytext)
library(janeaustenr)

austen_section_words <- austen_books() |>
  filter(book == "Pride & Prejudice") |>
  mutate(section = row_number() %/% 10) |>
  filter(section > 0) |>
  unnest_tokens(word, text) |>
  filter(!word %in% stop_words$word)

word_pairs <- austen_section_words |>
  pairwise_count(word, section, sort = TRUE)
