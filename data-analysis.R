library(tidyverse)
library(data.table)

df <- read_csv("https://github.com/joshua-feldman/resident-advisor/raw/master/resident-advisor.csv")

df <- df %>% 
  mutate(house = as.factor(ifelse(genre == "house" | genre %like% "\\bhouse\\b", 1, 0))) %>% 
  mutate(techno = as.factor(ifelse(genre == "techno" | genre %like% "\\btechno\\b", 1, 0))) %>% 
  mutate(experimental = as.factor(ifelse(genre == "experimental" | genre %like% "\\bexperimental\\b", 1, 0))) %>%
  mutate(ambient = as.factor(ifelse(genre == "ambient" | genre %like% "\\bambient\\b", 1, 0))) %>%
  mutate(year = substring(date, 1, 4)) %>% 
  mutate(yearmon = zoo::as.yearmon(date))

# Typical words for each genre

library(tidytext)
library(tidylo)
library(SnowballC)


df_house_techno <- df %>% 
  mutate(review = str_remove_all(review, "’")) %>% 
  mutate(review = removePunctuation(review)) %>% 
  filter(genre %in% c("house", "techno", "experimental", "ambient", "pop", "dubstep")) %>% 
  unnest_tokens(word, review) %>% 
  mutate(word = wordStem(word)) %>% 
  filter(!word %in% c("hous", "techno", "experimental", "ambient", "pop", "dubstep")) %>% 
  anti_join(stop_words) %>% 
  group_by(genre) %>% 
  count(word, sort = TRUE) %>% 
  bind_log_odds(genre, word, n) %>% 
  arrange(-log_odds)

df_house_techno %>% 
  group_by(genre) %>% 
  top_n(10, log_odds) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, log_odds)) %>% 
  ggplot(aes(word, log_odds, fill = genre)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~genre, scales = "free_y") +
  coord_flip() +
  labs(title = "What are the most specific words to each subgenre of electronic music?",
       subtitle = "Words have been stemmed where appropriate",
       caption = "Source: Resident Advisor",
       x = NULL,
       y = "Log odds ratio, weighted by uninformative Dirichlet prior") +
  theme_minimal(base_size = 18) +
  theme(text = element_text(family = "Raleway"),
        plot.title = element_text(face = "bold"))

# Distribution of review scores

library(grDevices)
library(RColorBrewer)
library(colorRamps)

summary(df$rating)

x <- df %>% 
  mutate(bin = cut(rating,
                   breaks = c(-Inf, 0.9, 1.4, 1.9, 2.4, 2.9, 3.4, 3.9, 4.4, 4.9, Inf),
                   labels = c("0.5 to 0.9", "1.0 to 1.4", "1.5 to 1.9",
                              "2.0 to 2.4", "2.5 to 2.9", "3.0 to 3.4",
                              "3.5 to 3.9", "4.0 to 4.4", "4.5 to 4.9", "5.0")))

df %>% 
  mutate(bin = cut(rating,
                   breaks = c(-Inf, 0.9, 1.4, 1.9, 2.4, 2.9, 3.4, 3.9, 4.4, 4.9, Inf),
                   labels = c("0.5 to 0.9", "1.0 to 1.4", "1.5 to 1.9",
                              "2.0 to 2.4", "2.5 to 2.9", "3.0 to 3.4",
                              "3.5 to 3.9", "4.0 to 4.4", "4.5 to 4.9", "5.0"))) %>% 
  count(bin) %>% 
  complete(bin, fill = list(n = 0)) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(label = paste(round(prop * 100, 2), "%", sep = "")) %>% 
  ggplot(aes(bin, prop, label = label)) +
  geom_col(fill = rev(green2red(10)))  +
  geom_text(family = "Raleway", fontface = "bold", vjust = -0.5, size = 6) +
  labs(title = "More than two-thirds of RA ratings are between 3.5 and 4.4",
       subtitle = "Based on 15k+ reviews from Feb 2001 to May 2019",
       x = NULL,
       y = NULL,
       caption = "Source: Resident Advisor") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 18) +
  theme(text = element_text(family = "Raleway"),
        plot.title = element_text(face = "bold"))

df %>% 
  filter(!is.na(ambient)) %>% 
  mutate(bin = cut(rating,
                   breaks = c(-Inf, 0.9, 1.4, 1.9, 2.4, 2.9, 3.4, 3.9, 4.4, 4.9, Inf),
                   labels = c("0.5-0.9", "1.0-1.4", "1.5-1.9",
                              "2.0-2.4", "2.5-2.9", "3.0-3.4",
                              "3.5-3.9", "4.0-4.4", "4.5-4.9", "5.0"))) %>% 
  group_by(ambient) %>% 
  count(bin) %>% 
  complete(bin, fill = list(n = 0)) %>% 
  mutate(prop = n / sum(n)) %>% 
  mutate(label = paste(round(prop * 100), "%", sep = "")) %>% 
  ggplot(aes(bin, prop, label = label, fill = ambient)) +
  geom_col()  +
  geom_text(family = "Raleway", fontface = "bold", vjust = -0.5, size = 6) +
  labs(title = "More than two-thirds of RA ratings are between 3.5 and 4.4",
       subtitle = "Based on 15k+ reviews from Feb 2001 to May 2019",
       x = NULL,
       y = NULL,
       caption = "Source: Resident Advisor") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ambient) +
  guides(fill = FALSE)

df %>% 
  ggplot(aes(rating)) +
  geom_histogram(bins = 10, fill = rev(green2red(10))) +
  labs(title = "More than two-thirds of RA ratings are between 3.5 and 4.0",
       subtitle = "Based on 15,695 reviews from Feb 2001 to May 2019",
       x = NULL,
       y = NULL,
       caption = "Source: Resident Advisor") +
  theme_minimal(base_size = 18) +
  theme(text = element_text(family = "Raleway"),
        plot.title = element_text(face = "bold"))

# Most commonly reviewed genres

df_expanded <- df %>% 
  filter(!is.na(genre)) %>% 
  separate_rows(genre)

genre_count <- df_expanded %>% 
  count(genre) %>% 
  top_n(20) %>%
  mutate(genre = reorder(genre, n))

genre_count %>% 
  ggplot(aes(genre, n, fill = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "House and techno are the most commonly reviewed genres on Resident Advisor",
    subtitle = "Based on 15k+ reviews from Feb 2001 to May 2019",
    x = NULL,
    y = NULL,
    caption = "Source: Resident Advisor") +
  theme_minimal(base_size = 18) +
  theme(text = element_text(family = "Raleway"),
        plot.title = element_text(face = "bold")) +
  guides(fill = FALSE)

# Difference between writers

df %>% 
  group_by(author) %>% 
  summarise(n = n(),
            avg_rating = mean(rating)) %>% 
  top_n(20, n) %>% 
  mutate(author = reorder(author, avg_rating)) %>% 
  ggplot(aes(author, avg_rating)) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 18) +
  theme(text = element_text(family = "Raleway"),
        plot.title = element_text(face = "bold"))

# Testing the hypothesis that RA prefers ambient
ggplot(df, aes(ambient, rating)) +
  geom_boxplot() +
  labs(title = "Example title",
       subtitle = "Example subtitle",
       x = "Example x",
       y = "Example y",
       caption = "Example caption")

ggplot(df, aes(ambient, rating)) +
  geom_violin() +
  theme_minimal() +
  labs(title = "Example title",
       subtitle = "Example subtitle",
       x = "Example x",
       y = "Example y",
       caption = "Source: Resident Advisor")