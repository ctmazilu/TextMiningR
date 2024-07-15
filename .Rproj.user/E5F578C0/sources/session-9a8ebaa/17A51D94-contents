# Written by CM
# Jul 15th 2024
# Gutenberg Text Analysis

#install.packages("tidyverse")
library(tidyverse)
#install.packages("gutenbergr")
library(gutenbergr)
#install.packages("tidytext")
library(tidytext)

#View(gutenberg_metadata)

# Create a tibble
gutenberg_metadata %>% 
  # double equals sign will find exactly the name
    filter(author=="Gibran, Kahlil") %>% 
    filter(title=="The Prophet") %>% 

gutenberg_metadata %>% 
  # double equals sign will find exactly the name
  filter(author=="Lawrence, of the Resurrection, Brother")
    
proph <- gutenberg_download(58585)
presence <- gutenberg_download(5657)


# Inspect and clean the data
str(proph)
head(proph)
tail(proph)
proph$text[500:550]

proph_clean <- proph[72:3089, ]
tail(proph_clean)
head(proph_clean)

tail(presence)
presence_clean <- presence[72:1038, ]
tail(presence_clean)

# Tokenize data
# Dividing data into specified units, each word will have it's own line 
# and removes punctuation converts everything to lowercase
# This is a bag of words
proph_tidy <- proph_clean %>% 
  unnest_tokens(word, text)

presence_tidy <- presence_clean %>% 
  unnest_tokens(word, text)

#View(proph_tidy %>% 
#       arrange(word))

#View(proph_tidy %>% 
#       count(word, sort = TRUE))

#data("stop_words")
SMART <- stop_words %>% 
  filter(lexicon == "SMART")

proph_smart <- proph_tidy %>% 
  anti_join(SMART)

presence_smart <- presence_tidy %>% 
  anti_join(SMART)

proph_smart %>%
  count(word, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

presence_smart %>%
  count(word, sort = TRUE) %>%
  filter(n > 17) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

View(proph_smart %>% 
       count(word, sort = TRUE))

View(presence_smart %>% 
       count(word, sort = TRUE))

library(scales)
library(tidyr)


frequency <- bind_rows(mutate(proph_smart, author = "Gibran, Kahlil"),
                       mutate(presence_smart, author = "Lawrence, of the Resurrection, Brother")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer('Lawrence, of the Resurrection, Brother',
               names_to = "author", values_to = "proportion")


# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Gibran, Kahlil`, 
                      color = abs(`Gibran, Kahlil` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Gibran, Kahlil", x = NULL)


cor.test(data = frequency[frequency$author == "Lawrence, of the Resurrection, Brother",],
         ~ proportion + `Gibran, Kahlil`)

#proph_total <- nrow(proph)


