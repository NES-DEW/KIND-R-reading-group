# session 02 bits and pieces
library(tidyverse)
library(tidytext)
library(gutenbergr)

# word freqs and gutenbergr ----
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

# sentiment analysis
afinn <- get_sentiments("afinn")

afinn

# afinn |>
#   readr::write_rds("afinn.rds")
# 
# afinn <- readr::read_rds("afinn.rds")

tibble(text = janeaustenr::sensesensibility) |> 
  unnest_tokens(output = "word", input = "text") |>
  anti_join(stop_words, by = join_by(word == word)) |>
  inner_join(afinn) |>
  count(word, wt = value) |>
  slice_max(n = 20, abs(n))

tibble(text = janeaustenr::sensesensibility) |> 
  unnest_tokens(output = "word", input = "text") |>
  anti_join(stop_words, by = join_by(word == word)) |>
  filter(word == "swoon")

sentisense<- tibble(text = janeaustenr::sensesensibility) |> 
  unnest_tokens(output = "word", input = "text") |>
  anti_join(stop_words, by = join_by(word == word)) |>
  inner_join(afinn) %>% group_by(word, value) %>% summarise(count = n()) %>% 
  mutate(weight = value * count)

tibble(text = janeaustenr::sensesensibility) |> 
  unnest_tokens(output = "word", input = "text") |>
  anti_join(stop_words, by = join_by(word == word)) |>
  inner_join(afinn) |>
  group_by(word) |> 
  summarise(n = n(), value = first(value)) |> 
  ungroup()

# non-word sentiments
bingnegative <- get_sentiments("bing") |>
  filter(sentiment == "negative") |>
  pull(word)

tibble(text = janeaustenr::sensesensibility) |> 
  unnest_tokens(token = "ngrams", output = "word", input = "text") |>
  tidyr::drop_na() ## much more complicated!


## chapter sentiments, pulled from the book
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

bingnegative <- get_sentiments("bing") %>% # negative words
  filter(sentiment == "negative")

wordcounts <- tidy_books %>% # chapter word counts
  group_by(book, chapter) %>%
  summarize(words = n())

chapter_sentiments <- tidy_books %>% # ratio of neg words per chapter
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) 

chapter_sentiments |> # plotting
  ungroup() |>
  ggplot() +
  geom_line(aes(x = chapter, y = ratio, group = book)) +
  facet_wrap(~book, scales = "free", ncol = 2)

# more developed sentiment analysis across Jane Austen's books, thanks to James Kilgour from PHS

lexicon = c("bing", "loughran", "nrc")

sentiment_comparison <- function(x){
  
  data <- tidy_books %>%
    inner_join(get_sentiments(x)) %>%
    count(book, index = linenumber %/% 80, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
    mutate(sentiment = positive - negative)
  
  
  plot <- ggplot(data, aes(index, sentiment, fill = book, colour = book)) +
    geom_hline(yintercept = 0, colour = "black") + # Null sentiment intercept
    geom_point(show.legend = FALSE, alpha = 0.5, size = 0.5) +
    geom_smooth(show.legend = FALSE) + # Loess to show localised chage throughout book
    labs(title = paste0("Sentiment source: ", x)) +
    facet_wrap(~book, ncol = 2, scales = "free_x")
  
  print(plot)
  
}

cowplot::plot_grid(plotlist = map(lexicon, sentiment_comparison), nrow = 1)

