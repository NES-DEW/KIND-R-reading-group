# session_02.R = plans

# word freqs and gutenbergr ----

library(gutenbergr)
hgwells <- gutenberg_download(
  c(35, 36, 5230, 159),
  mirror = "https://gutenberg.pglaf.org/"
)

# filter vs anti-join for stop word performance ----
# install.packages("bench")
library(bench)

tokenised <- tibble(text = janeaustenr::sensesensibility) |> 
  unnest_tokens(output = "tokens", input = "text") 

bench::mark(
  filter = tokenised |> 
  filter(!tokens %in% c(stop_words$word)),
  anti_join = tokenised |>  
  anti_join(stop_words, by = c("tokens" = "word")))

# prince lyrics ----

# there is this interesting lyrics analysis of the artist Prince to see if he had higher frequency of certain words in lyrics of songs before his death 
# https://www.datacamp.com/tutorial/R-nlp-machine-learning