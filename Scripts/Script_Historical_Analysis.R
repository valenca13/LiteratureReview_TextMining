#Import libraries

library(tm)
library(NLP)
library(tidyverse)
library(tidytext)
library(readxl)
library(textstem)

#Import dataset 

systematicreview <- read_excel("Data/table_systematic_review.xlsx")

#Transform dataset into dataframe

df <- data.frame(systematicreview)

# Filter only papers that were considered relevant for the analysis. "Frequent abstract" = 1. 
df <- subset(df, df[2]=="1") 

# Select papers from 1978 - 1985
A78_85 <- df[df$Year >= 1978 & df$Year<=1985,]

# Select only the abstract
A78_85 <- A78_85[,9]

#Data Cleaning 
#Remove punctuation
text <- gsub(pattern = "\\W", replace = " ", A78_85)
#Remove Numbers (digits)
text2 <- gsub(pattern = "\\d", replace = " ", text)
#Lowercase words
text3 <- tolower(text2)
#remove single words 
text4 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", text3) 
#Remove whitespace
text5 <- stripWhitespace(text4)
#Lematize terms in its dictionary form
A78_85 <- lemmatize_strings(text5, dictionary = lexicon::hash_lemmas)

A78_85final <- data.frame(A78_85)
colnames(A78_85final) <- c("abstracts")

#Tokenize words from abstracts
tokenizing_abstract <- A78_85final %>%
  mutate(id = row_number()) %>% #Create new id for each abstract
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)

adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "base", "CUSTOM",
  "result", "CUSTOM",
  "information","CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) 

#Add new stopwords and repeat the tokenization process
tokenizing_abstract <- A78_85final %>%
  mutate(id = row_number()) %>% 
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) 

#Frequency of words

word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>1) %>% #Filter words that appear more than "n" times
  mutate(word2 = fct_reorder(word, n)) %>% 
  arrange(desc(n)) 


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/max(n))
) +
  geom_col(fill = "gray75") + 
  coord_flip() + 
  labs(
    title = "Most frequent words of filtered abstracts: 1978 - 1985",
    x = "Words", y ="Normalized frequency - Total of 2 papers"
  )


#################################################################################


# Select papers from 1986 - 1990

A86_90 <- df[df$Year >= 1986 & df$Year<=1990,]

# Select only the abstract
A86_90 <- A86_90[,9]

#Data Cleaning 
#Remove punctuation
text <- gsub(pattern = "\\W", replace = " ", A86_90)
#Remove Numbers (digits)
text2 <- gsub(pattern = "\\d", replace = " ", text)
#Lowercase words
text3 <- tolower(text2)
#remove single words 
text4 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", text3) 
#Remove whitespace
text5 <- stripWhitespace(text4)
#Lematize terms in its dictionary form
A86_90 <- lemmatize_strings(text5, dictionary = lexicon::hash_lemmas)

A86_90final <- data.frame(A86_90)
colnames(A86_90final) <- c("abstracts")

#Tokenize words from abstracts
tokenizing_abstract <- A86_90final %>%
  mutate(id = row_number()) %>% #Create new id for each abstract
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "base", "CUSTOM",
  "result", "CUSTOM",
  "discussion", "CUSTOM",
  "information", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) 

#Add new stopwords and repeat the tokenization process
tokenizing_abstract <- A86_90final %>%
  mutate(id = row_number()) %>% 
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) 

#Frequency of words
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>1) %>% #Filter words that appear more than "n" times
  mutate(word2 = fct_reorder(word, n)) %>% 
  arrange(desc(n)) 


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/max(n))
) +
  geom_col(fill = "gray65") + 
  coord_flip() + 
  labs(
    title = "Most frequent words of filtered abstracts: 1986 - 1990",
    x = "Words", y ="Normalized frequency - Total of 1 paper"
  )

#################################################################################

# Select papers from 1991 - 1995
A91_95 <- df[df$Year >= 1991 & df$Year<=1995,]

# Select only the abstract
A91_95 <- A91_95[,9]

#Data Cleaning 
#Remove punctuation
text <- gsub(pattern = "\\W", replace = " ", A91_95)
#Remove Numbers (digits)
text2 <- gsub(pattern = "\\d", replace = " ", text)
#Lowercase words
text3 <- tolower(text2)
#remove single words 
text4 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", text3) 
#Remove whitespace
text5 <- stripWhitespace(text4)
#Lematize terms in its dictionary form
A91_95 <- lemmatize_strings(text5, dictionary = lexicon::hash_lemmas)

A91_95final <- data.frame(A91_95)
colnames(A91_95final) <- c("abstracts")

#Tokenize words from abstracts
tokenizing_abstract <- A91_95final %>%
  mutate(id = row_number()) %>% #Create new id for each abstract
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "base", "CUSTOM",
  "information", "CUSTOM",
  "discussion", "CUSTOM",
  "result", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) 


#Add new stopwords and repeat the tokenization process
tokenizing_abstract <- A91_95final %>%
  mutate(id = row_number()) %>% 
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) 

#Frequency of words
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>3) %>% #Filter words that appear more than "n" times
  mutate(word2 = fct_reorder(word, n)) %>% 
  arrange(desc(n)) 


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/max(n))
) +
  geom_col(fill = "gray65") + 
  coord_flip() + 
  labs(
    title = "Most frequent words of filtered abstracts: 1991 - 1995",
    x = "Words", y ="Normalized frequency - Total of 3 papers"
  )

##################################################################################################


# Select papers from 1996 - 2000
A96_00 <- df[df$Year >= 1996 & df$Year<=2000,]

# Select only the abstract
A96_00 <- A96_00[,9]

#Data Cleaning 
#Remove punctuation
text <- gsub(pattern = "\\W", replace = " ", A96_00)
#Remove Numbers (digits)
text2 <- gsub(pattern = "\\d", replace = " ", text)
#Lowercase words
text3 <- tolower(text2)
#remove single words 
text4 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", text3) 
#Remove whitespace
text5 <- stripWhitespace(text4)
#Lematize terms in its dictionary form
A96_00 <- lemmatize_strings(text5, dictionary = lexicon::hash_lemmas)

A96_00final <- data.frame(A96_00)
colnames(A96_00final) <- c("abstracts")

#Tokenize words from abstracts
tokenizing_abstract <- A96_00final %>%
  mutate(id = row_number()) %>% #Create new id for each abstract
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "base", "CUSTOM",
  "information", "CUSTOM",
  "discussion", "CUSTOM",
  "result", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) 


#Add new stopwords and repeat the tokenization process
tokenizing_abstract <- A96_00final %>%
  mutate(id = row_number()) %>% 
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) 

#Frequency of words
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>8) %>% #Filter words that appear more than "n" times
  mutate(word2 = fct_reorder(word, n)) %>% 
  arrange(desc(n)) 


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/max(n))
) +
  geom_col(fill = "gray55") + 
  coord_flip() + 
  labs(
    title = "Most frequent words of filtered abstracts: 1996 - 2000",
    x = "Words", y ="Normalized frequency - Total of 19 papers"
  )

########################################################################################

# Select papers from 2001 - 2005
A01_05 <- df[df$Year >= 2001 & df$Year<=2005,]

# Select only the abstract
A01_05 <- A01_05[,9]

#Data Cleaning 
#Remove punctuation
text <- gsub(pattern = "\\W", replace = " ", A01_05)
#Remove Numbers (digits)
text2 <- gsub(pattern = "\\d", replace = " ", text)
#Lowercase words
text3 <- tolower(text2)
#remove single words 
text4 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", text3) 
#Remove whitespace
text5 <- stripWhitespace(text4)
#Lematize terms in its dictionary form
A01_05 <- lemmatize_strings(text5, dictionary = lexicon::hash_lemmas)

A01_05final <- data.frame(A01_05)
colnames(A01_05final) <- c("abstracts")

#Tokenize words from abstracts
tokenizing_abstract <- A01_05final %>%
  mutate(id = row_number()) %>% #Create new id for each abstract
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "base", "CUSTOM",
  "result", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) 


#Add new stopwords and repeat the tokenization process
tokenizing_abstract <- A01_05final %>%
  mutate(id = row_number()) %>% #Create new id for each abstract
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) 


#Frequency of words
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>7) %>% #Filter words that appear more than "n" times
  mutate(word2 = fct_reorder(word, n)) %>% 
  arrange(desc(n)) 


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/max(n))
) +
  geom_col(fill = "gray45") + 
  coord_flip() +
  labs(
    title = "Most frequent words of filtered abstracts: 2001 - 2005",
    x = "Words", y ="Normalized frequency - Total of 13 papers"
  )


################################################################################


#Select papers from 2006 - 2010
A06_10 <- df[df$Year >= 2006 & df$Year<=2010,]

# Select only the abstract
A06_10 <- A06_10[,9]

#Data Cleaning 
#Remove punctuation
text <- gsub(pattern = "\\W", replace = " ", A06_10)
#Remove Numbers (digits)
text2 <- gsub(pattern = "\\d", replace = " ", text)
#Lowercase words
text3 <- tolower(text2)
#remove single words 
text4 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", text3) 
#Remove whitespace
text5 <- stripWhitespace(text4)
#Lematize terms in its dictionary form
A06_10 <- lemmatize_strings(text5, dictionary = lexicon::hash_lemmas)

A06_10final <- data.frame(A06_10)
colnames(A06_10final) <- c("abstracts")

#Tokenize words from abstracts
tokenizing_abstract <- A06_10final %>%
  mutate(id = row_number()) %>% #Create new id for each abstract
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "base", "CUSTOM",
  "result", "CUSTOM",
  "process", "CUSTOM",
  "research", "CUSTOM",
  "datum", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) 

#Add new stopwords and repeat the tokenization process
tokenizing_abstract <- A06_10final %>%
  mutate(id = row_number()) %>% 
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) 

#Frequency of words
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>10) %>% #Filter words that appear more than "n" times
  mutate(word2 = fct_reorder(word, n)) %>% 
  arrange(desc(n)) 


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/max(n))
) +
  geom_col(fill = "gray40") + 
  coord_flip() + 
  labs(
    title = "Most frequent words of filtered abstracts: 2006 - 2010",
    x = "Words", y ="Normalized frequency - Total of 31 papers"
  )

####################################################################################


# Select papers from 2011 - 2015
A11_15 <- df[df$Year >= 2011 & df$Year<= 2015,]
# Select only the abstract
A11_15 <- A11_15[,9]

#Data Cleaning 
#Remove punctuation
text <- gsub(pattern = "\\W", replace = " ", A11_15)
#Remove Numbers (digits)
text2 <- gsub(pattern = "\\d", replace = " ", text)
#Lowercase words
text3 <- tolower(text2)
#remove single words 
text4 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", text3) 
#Remove whitespace
text5 <- stripWhitespace(text4)
#Lematize terms in its dictionary form
A11_15 <- lemmatize_strings(text5, dictionary = lexicon::hash_lemmas)

A11_15final <- data.frame(A11_15)
colnames(A11_15final) <- c("abstracts")

#Tokenize words from abstracts
tokenizing_abstract <- A11_15final %>%
  mutate(id = row_number()) %>% #Create new id for each abstract
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "base", "CUSTOM",
  "result", "CUSTOM",
  "research", "CUSTOM",
  "datum", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) 


#Add new stopwords and repeat the tokenization process
tokenizing_abstract <- A11_15final %>%
  mutate(id = row_number()) %>% 
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) 

#Frequency of words
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>44) %>% #Filter words that appear more than "n" times
  mutate(word2 = fct_reorder(word, n)) %>% 
  arrange(desc(n)) 


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/max(n))
) +
  geom_col(fill = "gray35") + 
  coord_flip() + 
  labs(
    title = "Most frequent words of filtered abstracts: 2011 - 2015",
    x = "Words", y ="Normalized frequency - Total of 103 papers"
  )

###############################################################################

# Select papers from 2016 - 2020
A16_20 <- df[df$Year >= 2016 & df$Year<=2020,]

# Select only the abstract
A16_20 <- A16_20[,9]

#Data Cleaning 
#Remove punctuation
text <- gsub(pattern = "\\W", replace = " ", A16_20)
#Remove Numbers (digits)
text2 <- gsub(pattern = "\\d", replace = " ", text)
#Lowercase words
text3 <- tolower(text2)
#remove single words 
text4 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", text3) 
#Remove whitespace
text5 <- stripWhitespace(text4)
#Lematize terms in its dictionary form
A16_20 <- lemmatize_strings(text5, dictionary = lexicon::hash_lemmas)

A16_20final <- data.frame(A16_20)
colnames(A16_20final) <- c("abstracts")

#Tokenize words from abstracts
tokenizing_abstract <- A16_20final %>%
  mutate(id = row_number()) %>% #Create new id for each abstract
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "base", "CUSTOM",
  "information","CUSTOM",
  "analysis", "CUSTOM",
  "discussion","CUSTOM",
  "result", "CUSTOM",
  "research", "CUSTOM",
  "datum", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) 


#Add new stopwords and repeat the tokenization process
tokenizing_abstract <- A16_20final %>%
  mutate(id = row_number()) %>% 
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) 


#Frequency of words
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>130) %>% #Filter words that appear more than "n" times
  mutate(word2 = fct_reorder(word, n)) %>% 
  arrange(desc(n)) 


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/max(n))
) +
  geom_col(fill = "grey20") + 
  coord_flip() + 
  labs(
    title = "Most frequent words of filtered abstracts: 2016 - 2020",
    x = "Words", y ="Normalized frequency - Total of 326 papers"
  )

