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
austen_bigrams <- austen_books() |>
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

# context in sentiment analysis

neg_bigrams <- bigrams_separated |>
  filter(word1 == "not") |>
  count(word1, word2, sort = TRUE)

not_words <- bigrams_separated |>
  filter(word1 == "not") |>
  inner_join(AFINN, by = c(word2 = "word")) |>
  count(word2, value, sort = TRUE)

not_words %>%
  mutate(contribution = n * value) |>
  arrange(desc(abs(contribution))) |>
  head(20) |>
  mutate(word2 = reorder(word2, contribution)) |> # reorder the factor
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

# trigrams - which you can generate easily enough

austen_trigrams <- austen_books() |>
  unnest_tokens(output = trigram,
                input = text, 
                token = "ngrams",
                n = 3) |>
  filter(!is.na(trigram)) 

austen_trigrams_sep <- austen_trigrams |>
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

# not_words_tri_w2 <- 
austen_trigrams_sep |>
  filter(word1 == "not") |>
  inner_join(AFINN, by = c(word2 = "word")) |>
  rename(word2_value = value) |>
  inner_join(AFINN, by = c(word3 = "word")) |>
  rename(word3_value = value) |>
  mutate(overall_value = word2_value + word3_value)
  filter(!is.na(value))

austen_trigrams_sep |> # or a pivoted version?
    filter(word1 == "not") |>
  mutate(id = row_number()) |>
  pivot_longer(!c(id, book, word1), names_to  = "position", values_to = "word") |>
  inner_join(AFINN, by = c(word = "word"))
    
not_words_tri_w3 <- austen_trigrams_sep |> # but how does negation work in trigrams??
  filter(word1 == "not") |>
  inner_join(AFINN, by = c(word3 = "word")) |>
  count(word3, value, sort = TRUE)

# network graph ----
bigram_graph <- bigram_counts |>
  filter(n > 20) |>
  graph_from_data_frame()

bigram_graph

set.seed(2017)

# the simple, undirected version
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# the fancier, directed, version: basically a Markov chain
a <- grid::arrow(type = "closed", length = unit(.15, "inches")) # nice arrows

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
