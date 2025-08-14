# Load necessary libraries
library(dplyr)
library(stringr)
library(tibble)
library(readr)
library(syuzhet)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(scales)
library(wordcloud)
library(RColorBrewer)
library(purrr)
library(ldatuning)
library(topicmodels)
library(purrr)
library(rBayesianOptimization)


file_paths <- list.files("data/raw_data", pattern = "\\.txt$", full.names = TRUE)
book_titles <- tools::file_path_sans_ext(basename(file_paths))

raw_books <- map2_df(file_paths, book_titles, function(path, title) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  tibble(
    book = title,
    text = lines
  )
})

# Defining regular and alternate regular
chapter_regex <- regex(
  "^\\s*(chapter\\s+(\\d+|[ivxlc]{1,6})[.]?(\\s+[^\\n]{1,80})?\\s*)$|^\\s*(\\d+|[ivxlc]{1,6})\\s*$",
  ignore_case = TRUE
)
chapter_regex_2 <- regex(
  "^\\s*(chapter\\s+(\\d+|[ivxlc]{1,6})[.]?(\\s+[a-zA-Z0-9,'\"\\- ]{1,50})?\\s*)$|^\\s*(\\d+|[ivxlc]{1,6})\\s*$",
  ignore_case = TRUE
)
chapter_regex_1 <- regex("^\\s*(chapter\\s+)?(\\d+|[ivxlc]{1,6})\\s*$", ignore_case = TRUE)
part_regex <- regex("^\\s*part\\s+([ivxlc]+|\\d+|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|twenty)\\b", ignore_case = TRUE)
contents_regex <- regex("^\\s*contents\\s*$", ignore_case = TRUE)

# General chapter identification function
process_one_book <- function(df, chapter_regex) {
  df <- df %>% mutate(linenumber = row_number())
  
  # Identify "directory" lines and only look for them in the first 100 lines
  content_line <- which(df$linenumber <= 100 & str_detect(df$text, contents_regex))[1]
  
  # Defines whether it is a chapter
  df <- df %>%
    mutate(
      is_chapter = str_detect(text, chapter_regex),
      is_content_zone = if (!is.na(content_line)) linenumber > content_line & linenumber <= content_line + 100 else FALSE,
      is_year_like = str_detect(text, "^\\s*\\d{4}\\s*$"),
      is_chapter = if_else(is_content_zone | is_year_like, FALSE, is_chapter),
      chapter = cumsum(is_chapter)
    ) %>%
    select(-is_chapter, -is_content_zone)
  
  return(df)
}

original_books <- raw_books %>%
  group_by(book) %>%
  group_modify(~ process_one_book(.x, chapter_regex)) %>%
  ungroup()


original_books %>%
  filter(chapter > 0) %>%
  distinct(book, chapter) %>%
  count(book, name = "n_chapters") %>%
  arrange(desc(n_chapters)) %>%
  print(n = Inf)

original_books %>%
  filter(chapter > 0) %>%
  group_by(book, chapter) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(book, linenumber, text, chapter) %>%
  print(n = Inf)

end_trimmed_books <- original_books %>%
  group_by(book) %>%
  group_modify(~ {
    end_line <- which(str_detect(.x$text, fixed("*** END OF THE PROJECT GUTENBERG EBOOK", ignore_case = TRUE)))[1]
    if (!is.na(end_line)) {
      .x <- .x %>% filter(linenumber < end_line)
    }
    return(.x)
  }) %>%
  ungroup()

end_trimmed_books <- end_trimmed_books %>%
  select(-any_of("is_year_like"))

# Output the detection log to check whether each book is successfully truncated
trim_check <- original_books %>%
  group_by(book) %>%
  summarise(
    end_line = which(str_detect(text, fixed("*** END OF THE PROJECT GUTENBERG EBOOK", ignore_case = TRUE)))[1],
    total_lines_before = n()
  ) %>%
  left_join(
    end_trimmed_books %>%
      group_by(book) %>%
      summarise(total_lines_after = n()),
    by = "book"
  ) %>%
  mutate(
    was_trimmed = !is.na(end_line),
    lines_removed = total_lines_before - total_lines_after
  ) %>%
  select(book, was_trimmed, end_line, lines_removed) %>%
  arrange(desc(lines_removed))

print(trim_check, n = Inf)

#######################################################################
saveRDS(end_trimmed_books, file = "data/processed/final_books.rds")

  
cleaned_books <- end_trimmed_books %>%
  filter(text != "", !str_detect(text, "^\\s*$"), !str_detect(text, "^[-–—\\*\\s]*$"))

cleaned_books <- cleaned_books %>%
  mutate(
    text = str_squish(text),                          
    text = str_replace_all(text, "[‘’]", "'"),       
    text = str_replace_all(text, '[“”]', '"'),        
    text = str_to_lower(text)                        
  )

book_tokens <- cleaned_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]")) 
##############################################################################
saveRDS(book_tokens, file = "data/processed/book_tokens.rds")

book_tokens %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  slice_max(n, n = 5) %>%
  ungroup() %>%
  print(n = Inf)

top_n <- 10

# Calculate word frequency by book group and extract the top N
top_words_per_book <- book_tokens %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  slice_max(n, n = top_n) %>%
  ungroup()

# Generate a word frequency graph for each book and save it as a list
plots <- top_words_per_book %>%
  group_split(book) %>%
  map(~ {
    book_title <- unique(.x$book)
    
    ggplot(.x, aes(x = n, y = fct_reorder(word, n))) +
      geom_col(fill = "steelblue") +
      labs(
        title = paste("Top", top_n, "Words in", book_title),
        x = "Frequency", y = NULL
      ) +
      theme_minimal(base_size = 12)
  })

plots[[10]] 

tfidf_by_book <- book_tokens %>%
  count(book, word, sort = TRUE) %>%             
  bind_tf_idf(word, book, n) %>%                
  arrange(desc(tf_idf))                          

top_tfidf_words <- tfidf_by_book %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup()

ggplot(top_tfidf_words, aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book, scales = "free", ncol = 2) +
  labs(
    title = "Top 10 TF-IDF Words per Book",
    x = "TF-IDF Score", y = NULL
  ) +
  theme_minimal(base_size = 12)


