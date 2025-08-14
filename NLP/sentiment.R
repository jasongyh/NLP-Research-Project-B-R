
get_sentiments("afinn")
afinn <- get_sentiments("afinn")

afinn_sentiment <- book_tokens %>%
  inner_join(afinn, by = "word") %>%
  group_by(book, chapter) %>%
  summarise(sentiment_score = sum(value), .groups = "drop")

ggplot(afinn_sentiment, aes(x = chapter, y = sentiment_score)) +
  geom_line() +
  facet_wrap(~ book, scales = "free_x") +
  labs(title = "Chapter Sentiment Intensity Trend（AFINN）", y = "Sentiment score", x = "chapter")


sentiment_words <- book_tokens %>%
  inner_join(afinn, by = "word") %>%
  mutate(polarity = if_else(value > 0, "positive", "negative"))

word_counts <- sentiment_words %>%
  count(book, polarity, word, sort = TRUE)


# Draw positive words for Treasure Island
word_counts %>%
  filter(book == "20000_Leagues_ Under_ the_ Sea", polarity == "positive") %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(8, "Greens")))

# Draw negative words for Treasure Island
word_counts %>%
  filter(book == "20000_Leagues_ Under_ the_ Sea", polarity == "negative") %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(8, "Reds")))


book_list <- unique(word_counts$book)

# Draw a word cloud for each book (one positive and one negative)
for (book_name in book_list) {
  cat("book name：", book_name, "\n")

  word_counts %>%
    filter(book == book_name, polarity == "positive") %>%
    with(wordcloud(word, n, max.words = 100, 
                   colors = brewer.pal(8, "Greens"), 
                   main = paste("Positive words in", book_name)))
  
 
  readline(prompt = "Press Enter to view the negative word cloud:")
  
  # Negative word cloud
  word_counts %>%
    filter(book == book_name, polarity == "negative") %>%
    with(wordcloud(word, n, max.words = 100, 
                   colors = brewer.pal(8, "Reds"), 
                   main = paste("Negative words in", book_name)))
  
  readline(prompt = "Press Enter to view the negative word cloud:")
}

sentiment_record <- afinn_sentiment %>%
  arrange(book, chapter) %>%
  group_by(book) %>%
  mutate(
    delta_sentiment = sentiment_score - lag(sentiment_score),
    topic_shift = NA_real_,
    entity_shift = NA_real_
  ) %>%
  ungroup()




