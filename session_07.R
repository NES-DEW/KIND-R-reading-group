# session_07.R = non-tidy formats

# CRAN task view: https://cran.r-project.org/web/views/NaturalLanguageProcessing.html

# setup ----
library(tm)
library(tidytext)
library(tidyverse)

# corpus ----

# corpus objects are lists with metadata and content

data("acq")

acq[[1]]

acq[[1]]$meta
acq[[1]]$meta$heading
acq[[1]]$content

acq |>
  tidy() # tidy the list to a tibble

# DTM = document term matrix ----

# |  document         | whale | cotillion |
# |-------------------|-------|-----------|
# | moby dick         |  91   |      0    |
# | pride & prejudice |   0   |     76    |
# | ...               |   m   |      n    |

data("AssociatedPress", package = "topicmodels")
?topicmodels::AssociatedPress

dim(AssociatedPress)

Docs(AssociatedPress)
Terms(AssociatedPress)

AssociatedPress |>
  tidy() |>
  arrange(-count) |> # or whatever actual analysis you wanted
  slice_max(count, n = 100, with_ties = F) |>
  cast_dtm(document, term, count)

# DFM = document-feature matrix -----

library(quanteda)
data("data_corpus_inaugural", package = "quanteda")

data_corpus_inaugural |>
  class() # corpus object

inaug_dfm <- data_corpus_inaugural |>
  quanteda::tokens() |>
  quanteda::dfm(verbose = FALSE)

inaug_dfm # structurally similar to dtm

inaug_dfm |>
  tidy() |>
  slice_max(count, n = 100, with_ties = F) |>
  cast_dfm(document, term, count)

# matrix ----

library(Matrix)

# cast into a Matrix object
AssociatedPress |>
  tidy() |>
  arrange(-count) |> # or whatever actual analysis you wanted
  slice_max(count, n = 100, with_ties = F) |>
  cast_sparse(document, term, count)
