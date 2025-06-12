# topic modelling = find the semantic structure in text
# semantic = about meanings. Complex relationship to actual words!
# standard example: finding cat and dog topics in c("barking, scratching, biting, fur, basket", "eye, ball, biscuit, bluetit")
# documents contain several topics, topics contain many words, topics overlap, and have vague edges/boundaries

# LDA [Latent Dirichlet allocation] = a kind of unguided topic modelling using Bayesian networks
# developed in genomics, but exported more widely

# "LDA is a mathematical method for estimating...the mixture of words that is associated with each topic, while also determining the mixture of topics that describes each document."

library(topicmodels)
library(tidyverse)
library(tidytext)
data("AssociatedPress")
AssociatedPress # that'll be a DTM

#topicmodels::LDA(k = number of topics)

ap_lda <- AssociatedPress |>
  topicmodels::LDA(k = 2, control = list(seed = 1234)) # computationally spicy

ap_lda # LDA-VEM?? As before, chuck it into tidy(). BUT!! really flipping confusingly, the tidy function in tidytext overloads broom's tidy - so the code below is tidytext only, rather than broom::tidy

ap_lda |>
  tidytext::tidy(matrix = "beta") |>
  arrange(-beta)  # beta is an word-topic probability
 
## beta graph example of word-topic probabilities ----
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## greatest differences in beta between topics ----

ap_lda |>
  tidytext::tidy(matrix = "beta") |>
  pivot_wider(names_from = topic, values_from = beta, names_prefix ="topic_") |>
  mutate(diff = abs(topic_1 - topic_2)) |>
  slice_max(diff, n = 20)

## Document-topic probabilities ----
