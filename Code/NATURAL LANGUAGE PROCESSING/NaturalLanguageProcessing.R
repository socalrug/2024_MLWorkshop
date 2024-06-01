#TEXT MINING (NATURAL LANGUAGE PROCESSING (NLP))
#install.packages(c("gutenbergr", "stringr", "dplyr", "tidytext", "stopwords", "tibble", 
#"ggraph", "wordcloud")) 

library(gutenbergr)
library(stringr)
library(dplyr)
library(tidytext)
library(stopwords) 
library(tibble)
library(ggplot2)
library(wordcloud)

book<- gutenberg_download(19033, meta_fields="author")
#48320, 42671, 74, 24022, 1777, 1597, 19033
#puts text into tibble format
book<- as_tibble(book) %>% 
  mutate(document = row_number()) %>% 
  select(-gutenberg_id)

#creates tokens (words)
#tokenization is the process of splitting text into tokens
tidy_book <- book %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

#identifying and removing stopwords (prepositions, articles)
stopword <- as_tibble(stopwords::stopwords("en")) 
stopword <- rename(stopword, word=value)
tb <- anti_join(tidy_book, stopword, by='word')

#calculating word frequency
word_count <- count(tb, word, sort = TRUE)
print(word_count, n=30)

#plotting bar graph
tb %>%
  count(author, word, sort = TRUE) %>%
  filter(n > 150) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(aes(fill=author)) +
  xlab(NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  theme_classic(base_size = 12) +
  labs(fill= "Author", title="Word frequency", subtitle="n > 50")+
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  scale_fill_brewer() 

#plotting word cloud
tb %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=50, colors=brewer.pal(8, "Dark2")))

#PLOTTING FANCIER WORDCLOUDS
library(wordcloud2)
library(RColorBrewer)

wordcloud2(word_count, shape='cardioid', size=0.5)
#options for shapes: circle, cardioid, diamond, triangle-forward, triangle, pentagon, star

 
