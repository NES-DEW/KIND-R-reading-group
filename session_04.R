# this session was about the first part of chapter 4: tokenising to n-word chunks (n-grams)

library(tidyverse)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() |>
  unnest_tokens(output = bigram,
                input = text, 
                token = "ngrams",
                n = 2) |>
  filter(!is.na(bigram))

austen_bigrams |>
  count(bigram, sort = T)

austen_bigrams |> 
  separate(bigram, into = c("word1", "word2"), sep = " ") |>
  filter(!if_any(contains("word"), ~ . %in% stop_words$word)) # using if_any
         
bigrams_filtered <- austen_bigrams |> 
  separate(bigram, into = c("word1", "word2"), sep = " ", remove = F) |>
  filter(!word1 %in% stop_words$word) |>
  filter(!word2 %in% stop_words$word)

bigrams_filtered |>
  count(word1, word2, sort = T)

bigrams_filtered |> # miss detector
  filter(word1 == "miss") |>
  count(word2, sort = TRUE)

bigrams_filtered |> # miss detector
  filter(word2 == "miss") |>
  count(word1, sort = TRUE)

## for next time

## James's textbook plot
bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) |> 
  arrange(book, desc(tf_idf)) |> 
  group_by(book) |> 
  slice_head(n = 10) |> 
  ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = book)) +
  geom_col() +
  facet_wrap(~book, scales = "free_y")

## James's medical notes code
# Load necessary library

# Define a list of drug names
drugs <- c("paracetamol", "ibuprofen", "aspirin", "amoxicillin", "metformin", "lisinopril", "atorvastatin", "omeprazole")

# Define a list of negations
negations <- c("not", "no", "without", "never")

# Function to generate medical notes
generate_notes <- function(n) {
  notes <- character(n)
  for (i in 1:n) {
    # Randomly decide the length of the note (between 2 and 10 words)
    note_length <- sample(2:10, 1)
    
    # Randomly decide if the note will contain a negation
    contains_negation <- sample(c(TRUE, FALSE), 1)
    
    # Generate the note
    note <- character(note_length)
    for (j in 1:note_length) {
      if (contains_negation && j == 1) {
        note[j] <- sample(negations, 1)
      } else {
        note[j] <- sample(drugs, 1)
      }
    }
    
    # Combine the words into a single string
    notes[i] <- str_c(note, collapse = " ")
  }
  return(notes)
}

# Generate 10 medical notes
set.seed(123)  # For reproducibility
medical_notes <- generate_notes(10)
print(medical_notes)
