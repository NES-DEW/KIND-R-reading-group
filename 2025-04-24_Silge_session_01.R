text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

# tokenize
library(dplyr)
library(tidytext)

# token = poet's line
text |>
  as_tibble() # nearly a dataframe

# token = words
text |>
  as_tibble() |>
  unnest_tokens(word, value) # specify token = word - and column = value

# token = case-sensitive words
text |>
  as_tibble() |>
  unnest_tokens(word, value, to_lower = F) # specify token = word - and column = value

# other tokens
text |>
  as_tibble() |>
  unnest_tokens(output = "tokens", 
                token = "character_shingles", # odd way of tokenising
                input = value) # name your arguments

# 1.3 example with stopwords
# install.packages("janeaustenr")
# install.packages("stringr")
library(janeaustenr)
library(stringr)

tibble(raw = prideprejudice) |>
  unnest_tokens(output = "token",
                token = "words",
                input = raw) |>
  anti_join(stop_words, by = join_by(token == word)) |>
  count(token, sort = T) 

# and visualisation word
library(ggplot2)

austen_books() |>
  group_by(book) |>
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) |>
  ungroup() |>
  unnest_tokens(letter, text, "characters") |>
  group_by(chapter) |> 
  count(letter) |>
  filter(letter %in% c("a", "e", "i", "o", "u")) |>
  ungroup() |>
  ggplot(aes(n, fill = letter)) +
  geom_density() +
  facet_wrap(~letter, ncol = 1)

# and with the stop words removed

austen_books() |>
  group_by(book) |>
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) |>
  ungroup() |>
  unnest_tokens(word, text, "words") |>
  anti_join(stop_words) |>
  unnest_tokens(letter, word, "characters") |>
  group_by(chapter) |> 
  count(letter) |>
  filter(letter %in% c("a", "e", "i", "o", "u")) |>
  ungroup() |>
  ggplot(aes(n, fill = letter)) +
  geom_density() +
  facet_wrap(~letter, ncol = 1)
