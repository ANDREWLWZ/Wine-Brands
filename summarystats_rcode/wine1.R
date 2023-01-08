install.packages("tidyverse")
library(dplyr)
library(ggplot2)
library(esquisse)
library(tidyverse)
library(modeldata)
library(NLP)

# Read Data
wine_reviews <- read.csv("C:/Users/Yiwen/Downloads/wine_reviews.csv")
dim(wine_reviews)
str(wine_reviews)
summary(wine_reviews)

# generate graph for sl.No.
ggplot(wine_reviews) +
  aes(x = "", y = Sl.No.) +
  geom_boxplot(shape = "circle", fill = "#3A73A4") +
  theme_bw()

# generate graph for Reviews.do.Recommend
ggplot(wine_reviews) +
  aes(x = Reviews.do.Recommend) +
  geom_bar(fill = "#0C4C8A") +
  labs(x = "Reviews do Recommend", 
       y = "Count") +
  theme_bw()

# generate graph for Reviews.Num.Helpful
# removed 1040 rows which contain non-finite values
wine_reviews %>%
  filter(Sl.No. >= 580L & Sl.No. <= 1980L) %>%
  filter(Reviews.Num.Helpful >= 8.4 & Reviews.Num.Helpful <= 
           13.4 | is.na(Reviews.Num.Helpful)) %>%
  ggplot() +
  aes(x = "", y = Reviews.Num.Helpful) +
  geom_boxplot(shape = "circle", fill = "#4682B4") +
  theme_bw()

# generate graph for Reviews.Rating
# removed 445 rows which contain non-finite values
ggplot(wine_reviews) +
  aes(x = Reviews.Rating) +
  geom_histogram(bins = 12L, fill = "#4682B4") +
  theme_bw()








