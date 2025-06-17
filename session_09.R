# packages ---
library(R.utils)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(hunspell)
library(viridis)
library(textdata)

# three failed attempts to topic model (romance reviews, poetry, movie dialogue) -----

## ROMANCE REVIEWS: NSFW.   ----
## Do not mess with this data loading: it's enormous - 3+gB.

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

## cornell movie dialogues ----

url <- "https://www.cs.cornell.edu/~cristian/data/cornell_movie_dialogs_corpus.zip"

curl::curl_download(url, "data/movie_dialogue.zip")

utils::unzip("data/movie_dialogue.zip", exdir = "./data")

list.files("data/cornell movie-dialogs corpus/", pattern = "*.txt", full.names = T)


mov_lines <- tibble(text = read_lines("data/cornell movie-dialogs corpus/")) |>
  separate_wider_delim(text, delim = " +++$+++ ", names = c("lineID", "characterID", "movieID", "char_name", "text"))



mov_lines_dtm <- mov_lines |>
  select(text, movieID) |>
  unnest_tokens(output = "word",
                input = "text", 
                token = "words") |>
  anti_join(get_stopwords()) |>
  filter(nchar(word) > 3,
         !str_detect(word, "[^a-z]")) |>
  count(word, movieID) |>
  cast_dtm(document = movieID, 
           term = word,
           value = n)



mov_lines_lda <- LDA(mov_lines_dtm, k = 5, control = list(seed = 1234))

mov_lines_topics <- tidy(mov_lines_lda, matrix = "beta")

mov_lines_topics |>
  group_by(topic) |>
  slice_max(beta, n = 20) |> 
  ungroup() |>
  arrange(topic, -beta) |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# Twitter example ----
## many thanks to Dr Lynn Brown for this code and example

#### 1. Data loading  #### ----

url <- "https://archive.ics.uci.edu/static/public/438/health+news+in+twitter.zip"

curl::curl_download(url, "data/health_tweets.zip")

utils::unzip("data/health_tweets.zip", exdir = "./data")

all_tweets <- map(list.files("data/Health-Tweets/", pattern = "*.txt", full.names = T), \(x) as_tibble(read_lines(x)) ) |>
  `names<-`(list.files("data/Health-Tweets/", pattern = "*.txt")) |>
  list_rbind(names_to = "Channel") |>
  mutate(Channel = str_remove(Channel, ".txt")) |>
  separate_wider_delim(value, 
                       delim = "|", 
                       names = c("id", "dt", "tweet"), 
                       too_many = "merge", 
                       too_few = "align_start") |>
  separate(dt, into=c("dayname", "Month", "Day", "time", "plus", "Year"), sep=" ") |>
  mutate(Date=dmy(paste0(Day,Month,Year))) |>
  filter(!is.na(Date)) |>
  select(ID = id, Date, Channel, Tweet = tweet) |>
  mutate(Tweet=str_remove_all(Tweet, regex("(@\\w+)")), # an @ followed by any word character
         Tweet=str_remove_all(Tweet, regex("(@)")),  # an @ on its own
         Tweet=str_remove_all(Tweet, regex("#")), #remove hashtags, but not the words 
         Tweet=str_remove_all(Tweet, regex("\\S+.co\\S+")),  # any non whitespace surrunding.co
         Tweet=str_remove_all(Tweet, regex("http\\S+"))) # any no whit space surrounding http 

#### 2. Tweets per Channel #### ----
tweetsperchannel <- all_tweets %>% 
  group_by(Channel) %>% 
  summarise(n=n())

p1 <- ggplot(tweetsperchannel, aes(x=Channel, y=n, fill=Channel )) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_fill_viridis_d(option="D")+
  theme(legend.position = "none")
p1 

#### 3. Sentiment Analysis #### -----

#remove stop words, sentiment calculated by word and summed per tweet
all_tweets_sentiments <- all_tweets %>%
  unnest_tokens(word, Tweet) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(ID) %>%
  summarise(Tweet_sentiment=sum(value)) %>%
  left_join(all_tweets)

p2 <-  ggplot(all_tweets_sentiments, aes(x=Tweet_sentiment, fill=Channel)) +
  geom_histogram() +
  facet_wrap(~Channel) +
  scale_fill_viridis_d(option="D") +
  theme(legend.position = "none")

p2

#similar distrubution across channels - goodhealth looks to have biggest positive skew 
# sum of sentiments doesnt necessarily reflect the over all meaning 

#### 4. Document term frequency #### -----

tweet_words <- all_tweets %>%
  unnest_tokens(word, Tweet) %>%
  anti_join(stop_words) %>%
  count(Channel, word, sort = TRUE)

total_tweet_words <- tweet_words %>% 
  group_by(Channel) %>% 
  summarize(total = sum(n))

tweet_words <- left_join(tweet_words, total_tweet_words)

p3 <- ggplot(tweet_words, aes(n/total, fill = Channel)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~Channel) +
  scale_fill_viridis_d(option="D")

p3

#inverse document term frequency
channel_tf_idf <- tweet_words |>
  bind_tf_idf(word, Channel, n)
#observe NHS ect which more liekly to appear in UK channels, the names of each news channel ect have high tf-idf

p4 <- channel_tf_idf %>%
  group_by(Channel) %>%
  slice_max(tf_idf, n = 8) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Channel)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Channel, ncol=4, scales="free") +
  labs(x = "tf-idf", y = NULL) +
  scale_fill_viridis_d(option="D")
p4

#### 5. Document Term Matrix ####

channel_dtm  <- channel_tf_idf %>%
  cast_dtm(Channel, word, n)

channel_lda <- LDA(channel_dtm, k = 2, control = list(seed = 1234))

channel_topics <- tidy(channel_lda, matrix = "beta")

channel_topic_top_terms <- channel_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

p5 <- channel_topic_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  scale_fill_viridis_d(option="D")

p5
.
##started with 6 topics but too much redundancy, 3 topics also showed redundnacy, 2 topics made more sense with a dates topic and a healthcare topic

#### 5.B Document Term Matrix - by ID rather than channel ####

tweet_words <- all_tweets %>%
  unnest_tokens(word, Tweet) %>%
  anti_join(stop_words) %>%
  count(ID, word, sort = TRUE)

#inverse document term frequency
ID_tf_idf <- tweet_words |>
  bind_tf_idf(word, ID, n)

ID_dtm  <- ID_tf_idf %>%
  cast_dtm(ID, word, n)

ID_lda <- LDA(ID_dtm, k = 6, control = list(seed = 1234))

ID_topics <- tidy(ID_lda, matrix = "beta")

ID_topic_top_terms <- ID_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

p6 <- ID_topic_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  scale_fill_viridis_d(option="D")

p6

##Counting by tweet rather than channel gave a better understanding of topics, 6 topics produced here have far less redundancy. 