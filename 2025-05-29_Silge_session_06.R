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

word_pairs <- austen_section_words |> # non-tidy data
  widyr::pairwise_count(word, section, sort = TRUE)

word_pairs |>
  ggplot() +
  geom_histogram(aes(x = n))

# does tiling the groups give more useful correlations?

austen_offset <- function(n){
  austen_books() |>
    filter(book == "Pride & Prejudice") |>
    slice_tail(n = -n) |>
    mutate(section = row_number() %/% 10) |>
    filter(section > 0) |>
    unnest_tokens(word, text) |>
    filter(!word %in% stop_words$word) |> # non-tidy data
    widyr::pairwise_count(word, section, sort = TRUE)
}

temp <- map(1:10, austen_offset)

windowed_word_pairs <- temp |>
  list_rbind() |>
  group_by(item1, item2) |>
  summarise(mean_n = mean(n)) |>
  arrange(-n)

windowed_word_pairs |>
  left_join(word_pairs) |>
  filter(is.na(n))
  

# section by tokening into sentences, then group to sections, then token to words = co-occurrence in a sentence means something

sentence_word_pairs <- austen_books() |>
  filter(book == "Pride & Prejudice") |>
  unnest_tokens(sentence, text, "sentences") |>
  mutate(section = row_number()) |>
  unnest_tokens(word, sentence) |>
  filter(!word %in% stop_words$word) |> # non-tidy data
  widyr::pairwise_count(word, section, sort = TRUE)


# back to the arbitrary 10-line version. Can we add a count of sections to show intensive/extensive use?

austen_books() |>
  filter(book == "Pride & Prejudice") |>
  mutate(section = row_number() %/% 10) |>
  filter(section > 0) |>
  unnest_tokens(word, text) |>
  filter(!word %in% stop_words$word)

word_pairs <- austen_section_words |> # non-tidy data
  widyr::pairwise_count(word, section, sort = TRUE)

# ?pairwise_count
# pairwise_count - no real help about estimating sections from the source code

austen_books() |>
  filter(book == "Pride & Prejudice") |>
  filter(str_detect(text, "(?i)elizabeth.*(?i)Darcy")) # out-of-line vs in-line

