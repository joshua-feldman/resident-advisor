# Load libraries
library(tidyverse)
library(data.table) # For 'like' function
library(XML)

# Create function to convert HTML codes
html2txt <- function(str) {
  xpathApply(htmlParse(str, asText=TRUE),
             "//body//text()", 
             xmlValue)[[1]] 
}

# Set working directory
setwd("~/resident-advisor/reviews")

# Load review files
files <- list.files()
review_files <- lapply(files, read.delim, stringsAsFactors = FALSE, quote = "")

# Initialise data frame
df <- data.frame(matrix(ncol = 7, nrow = length(review_files)))
colnames(df) <- c("title", "author", "date", "genre", "rating", "review", "ra_recommends")

# Iterate over review files to populate the data frame
for(i in 1:length(review_files)) {
  review <- review_files[[i]]
  review <- lapply(review, as.character)
  review <- unlist(review, use.names = FALSE)
  review <- paste(review, collapse = " ")
  
  title <- str_match(review, "<span itemprop=\"itemreviewed\" style=\"display: none;\">(.*?)</span>")[,2]
  author <- str_match(review, "creator\": \\[\"(.*?)\"\\]")[,2]
  dt <- str_match(review, "<span itemprop=\"dtreviewed\" datetime=\"(.*?)\">")[,2]
  genre <- I(list(str_match_all(review, "<a href=\"/music/genre/(.*?)\">")[[1]][,2]))
  rating <- str_match(review, "<span class=\"rating\" itemprop=\"rating\">(.*?)<")[,2]
  ra_recommends <- ifelse(str_detect(review, "RA Recommends"), 1, 0)
  review <- str_match(review, "<span class=\"reading-line-height\" itemprop=\"description\">(.*?)</span>")[,2]
  review <- str_remove_all(review, "<.*?>")
  
  df$title[i] <- title
  df$author[i] <- author
  df$date[i] <- dt
  df$genre[i] <- genre
  df$rating[i] <- rating
  df$ra_recommends[i] <- ra_recommends
  df$review[i] <- review
  
}

# Convert HTML codes to text
df$title <- map(df$title, html2txt)
df$author <- map(df$author, html2txt)

# Remove character(0) values from genre
df$genre <- lapply(df$genre, function(x) if(identical(x, character(0))) NA_character_ else x)

# Make final adjustments to dataset
df <- df %>% 
  filter(!is.na(rating)) %>% 
  filter(!is.na(review)) %>% 
  mutate(title = str_replace_all(title, "\\u0080", " ")) %>% 
  mutate(title = str_replace_all(title, "\u0093", " ")) %>% 
  mutate(title = str_replace_all(title, " â ", " - ")) %>% 
  mutate(title = str_replace_all(title, "â \u0099", "'")) %>% 
  mutate(rating = as.numeric(rating))

# Clean known examples of messy titles (e.g. due to special characters)
unicode_pattern <- "â\u0084¢|â\u0088\u0086 â\u0088\u0086|\u008e- â\u0088\u009e"

df$title[df$title %like% unicode_pattern] <- iconv(df$title[df$title %like% unicode_pattern],
                                                   from = "UTF8", to = "LATIN1")

df$title[df$title %like% "email" & df$title %like% "protected"] <- "Z@P"
df$title[df$title %like% "Z@P"][1] <- "Z@P - Sonic Utopia EP"
df$title[df$title %like% "Z@P"][2] <- "Z@P - Sendas EP"
df$title[df$title %like% "Z@P"][3] <- "Z@P - Mayday EP"

df$title[df$title %like% "Astrosuka"] <- "Astrosuka  - с|т|р|у|к|т|у|р|ы"

df$title <- str_replace(df$title, " \x8e-", "")
df$review <- str_replace_all(df$review, "\\[email&#160;protected\\]", "Z@P")

df$title <- str_remove_all(x$title, "â \u008e|\u008eâ   |â \u008b")

# Convert from lists to vectors
df$title <- as.vector(df$title)
df$author <- as.vector(df$author)
df$genre <- as.vector(df$genre)

# Order data frame by date
df <- arrange(df, date)

# Remove duplicates
df <- df[!duplicated(df[c(1, 3)]),]

# Final adjustments
df$author <- vapply(df$author, paste, character(1L))
df$genre <- vapply(df$genre, paste, collapse = ", ", character(1L))

# Save RDS and CSV
saveRDS(df, "~/resident-advisor/resident-advisor.RDS")
write.csv(df, "~/resident-advisor/resident-advisor.csv", row.names = FALSE)
