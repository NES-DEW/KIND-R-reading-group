# packages ---
library(R.utils)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(hunspell)

# ROMANCE REVIEWS: NSFW. Do not mess with this data loading: it's enormous - 3+gB. ----

curl::curl_download("https://mcauleylab.ucsd.edu/public_datasets/gdrive/goodreads/byGenre/goodreads_reviews_romance.json.gz", "data/gr_rom.gz")

gunzip("data/gr_rom.gz", remove=FALSE, FUN = "data/gz_rom.json")

gr_rom <- file("data/gr_rom.json") |>
  stream_in() |>
  as_tibble()

gr_rom_lda <- gr_rom |>
  slice_max(n_votes, n = 5000) |>
  select(review_id, review_text) |>
  unnest_tokens(output = word, input = review_text) |>
  anti_join(get_stopwords()) |>
  filter(nchar(word) > 3) |>
  group_by(review_id) |>
  count(word) |>
  cast_dtm(document = review_id, term = word, value = n) |>
  LDA(k = 8) 

gr_rom_lda |>
  tidy(matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 10) |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot() +
  geom_col(aes(x = beta, y = term, fill = factor(topic))) +
  facet_wrap(~topic, scales = "free", ncol = 4) +
  scale_y_reordered() +
  theme(legend.position = "none")

## the poetry dataset is probably a better bet ----

curl::curl_download("https://mcauleylab.ucsd.edu/public_datasets/gdrive/goodreads/byGenre/goodreads_reviews_poetry.json.gz", "data/gr_poe.gz")

gunzip("data/gr_poe.gz", remove=FALSE)

file.rename("data/gr_poe", "data/gr_poe.json")

gr_poe <- file("data/gr_poe.json") |>
  stream_in() |>
  as_tibble()

gr_poe_lda <- gr_poe |>
  slice_max(n_votes, n = 500) |>
  select(review_id, review_text) |>
  unnest_tokens(output = word, input = review_text) |>
  filter(hunspell_check(word)) |> # lots of gibberish to strip away. Do students have to write a review for some school or other?
  anti_join(get_stopwords()) |>
  filter(nchar(word) > 3) |>
  group_by(review_id) |>
  count(word) |>
  cast_dtm(document = review_id, term = word, value = n) |>
  LDA(k = 8) 

gr_poe_lda |>
  tidy(matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 10) |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot() +
  geom_col(aes(x = beta, y = term, fill = factor(topic))) +
  facet_wrap(~topic, scales = "free", ncol = 4) +
  scale_y_reordered() +
  theme(legend.position = "none")