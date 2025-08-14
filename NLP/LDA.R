
# Create a chapter ID
book_tokens_id <- book_tokens %>%
  mutate(doc_id = paste(book, chapter, sep = "_"))

# Generate DTM: Each document is a chapter 
chapter_dtm <- book_tokens_id %>%
  count(doc_id, word) %>%
  cast_dtm(document = doc_id, term = word, value = n)

result_k <- FindTopicsNumber(
  chapter_dtm,
  topics = seq(2, 12, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 123),
  mc.cores = 1L
)

# Visualize selection results
FindTopicsNumber_plot(result_k)

lda_model <- LDA(chapter_dtm, k = 12, control = list(seed = 123))

chapter_topics <- tidy(lda_model, matrix = "gamma")  


chapter_topics <- chapter_topics %>%
  extract(document, into = c("book", "chapter"),
          regex = "^(.*)_(\\d+)$", convert = TRUE)

chapter_topics %>%
  filter(book == "20000_Leagues_Under_the_Sea", chapter == 1)

chapter_topics %>%
  filter(book == "Around_the_World_in_80_Days") %>%
  ggplot(aes(x = chapter, y = gamma, color = factor(topic), group = topic)) +
  geom_line() +
  labs(title = "Chapter topic distribution", y = "Topic Probability", x = "chapter", color = "topic") +
  theme_minimal()

chapter_topics %>%
  filter(book == "20000_Leagues_ Under_ the_ Sea") %>%
  count(topic)

chapter_topics %>%
  ggplot(aes(x = chapter, y = gamma, color = factor(topic), group = topic)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  facet_wrap(~ book, scales = "free_x") +
  labs(
    title = "Chapter distribution of the 8 themes in each novel",
    x = "chapter",
    y = "Topic Probability",
    color = "Topic Number"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
###################################################################
topic_terms <- tidy(lda_model, matrix = "beta")

# The top 10 most important words for each topic
top_terms <- topic_terms %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualization: The most important words for each topic
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top 10 Terms in Each Topic", x = "Beta (Term Probability)", y = NULL)

topic_labels <- tibble(
  topic = 1:12,
  topic_label = c(
    "Boys' Adventure & Island Life",        # 1
    "Pirates & Maritime Conflict",          # 2
    "Island Survival & Natives",            # 3
    "Global Voyage & Adventure",            # 4
    "Underwater Exploration & Nautilus",    # 5
    "Engineering & Collective Work A",      # 6
    "Engineering & Collective Work B",      # 7
    "Gutenberg Metadata Noise",             # 8
    "Sea Survival & the Captain",           # 9
    "Lost World & Dinosaur Expedition",     #10
    "Shipwreck & Character Relations",      #11
    "African Exploration & Tribal Conflict" #12
  )
)

# Join the labels to chapter_topics
chapter_topics_labeled <- chapter_topics %>%
  left_join(topic_labels, by = "topic")
##############################################################################

topic_probs <- chapter_topics %>%
  pivot_wider(
    names_from = topic,
    values_from = gamma,
    names_prefix = "topic_"
  )

# Calculate the change in each topic (chapter difference)
topic_deltas <- topic_probs %>%
  arrange(book, chapter) %>%
  group_by(book) %>%
  mutate(across(starts_with("topic_"), ~ .x - lag(.x), .names = "delta_{.col}")) %>%
  ungroup()

# Merge topic + delta_topic back into sentiment_record
sentiment_record <- sentiment_record %>%
  left_join(topic_deltas, by = c("book", "chapter"))

