## SEntiment Analysis
# load required packages
library(tidyverse)
library(syuzhet)
library(tidytext)
# import text dataset
wine_reviews <- read.csv("C:/Users/Yiwen/Downloads/wine_reviews.csv")
text.wine_reviews <- tibble(text = str_to_lower(wine_reviews$Reviews.Text))
# analyze sentiments using the syuzhet package based on the NRC sentiment dictionary
emotions <- get_nrc_sentiment(text.wine_reviews$text)
View(emotions)
emo_bar <- colSums(emotions)
emo_bar
emo_sum <- data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum
# create a graph showing the counts for each of eight different emotions and positive/negative rating
ggplot(emo_sum, aes(x = reorder (emotion, -count), y = count))+
  geom_bar (stat = "identity", fill = "#0C4C8A")+
  theme_bw()

# sentiment analysis with the tidytext package using the "bing" lexicon 
bing_word_counts <- text.wine_reviews %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("bing"))%>%
  count(word, sentiment, sort = TRUE)

# select the top 10 words by sentiment
bing_top_10_words_by_sentiment <- bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(order_by = n, n=10) %>%
  ungroup() %>%
 mutate(word = reorder(word, n))
bing_top_10_words_by_sentiment

# create barport
bing_top_10_words_by_sentiment %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

# sentiment analysis with the tidytext package
loughran_word_counts <- text.wine_reviews %>% unnest_tokens(output = word, input = text) %>% 
  inner_join(get_sentiments("loughran")) %>%
  count(word, sentiments, sort = TRUE)






