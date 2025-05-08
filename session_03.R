# austen tf-idf from the book ----
# based on chapter 3 of Silge and Robinson: https://www.tidytextmining.com/
library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() |>
  unnest_tokens(word, text) |>
  count(book, word, sort = TRUE)

total_words <- book_words |>
  group_by(book) |>
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

top_tf_idf <- book_words |>
  bind_tf_idf(word, book, n) |>
  group_by(book) |>
  slice_max(tf_idf, n = 20)

book_words |>
  # group_by(book) |>
  filter(n == 1) |>
  select(word) |>
  count(word, sort = T)


# prince lyrics ----

# there is this interesting lyrics analysis of the artist Prince to see if he had higher frequency of certain words in lyrics of songs before his death 
# https://www.datacamp.com/tutorial/R-nlp-machine-learning

library(tidyverse)
library(tidytext)

prince <- read_csv("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/prince_raw_data.csv")

prince |>
  select(text, artist, song) |>
  unnest_tokens(input = text,
               output = word,
               token = "words") |>
  count(song, word, sort = T) |>
  # filter(str_detect(song, "compares")) |>
  bind_tf_idf(term = word, 
              document = song, 
              n = n) |>
  slice_max(tf, n = 20)

prince |>
  select(text, artist, song) |>
  filter(song == "mr nelson") |>
  pull(text)
         
## looking at other sources ----

tibble(lyrics = read_lines("data/Prisencolinensinainciusol.txt")) |>
  unnest_tokens(input = lyrics,
                output = word,
                token = "words") |>
  mutate(song = "Prisencolinensinainciusol") |>
  count(song, word) |>
  bind_tf_idf(term = word,
              document = song, 
              n = n) |>
  arrange(tf_idf) # important! tf-idf assumes that you're looking at more than one document. It doesn't do anything sensible with single docs as it esimates log counts

(tibble(lyrics = read_lines("data/Prisencolinensinainciusol.txt"),
        song = "Prisencolinensinainciusol")) |>
  bind_rows(tibble(lyrics = read_lines("data/moby.txt"),
                   song = "moby")) |>
  unnest_tokens(input = lyrics,
                output = word,
                token = "words") |>
  count(song, word) |>
  bind_tf_idf(term = word,
              document = song, 
              n = n) |>
  slice_max(tf_idf, n = 20) # works as expected even with one short nonsense input, and one longer input with non-similar text
