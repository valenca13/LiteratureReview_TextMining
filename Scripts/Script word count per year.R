#Import libraries

library(tm)
library(NLP)
library(tidyverse)
library(tidytext)
library(readxl)

#Import dataset 

systematicreview <- read_excel("Data/systematicreview.xlsx")

df <- data.frame(systematicreview)


#Remove column (authors)
df <- df[,-c(7)] 

# Filter only papers that were considered relevant for the analysis. "Frequent abstract" = 1. 
df <- subset(df, df[2]=="1") 


# Select papers from 1978 - 1985
A78_85 <- df[df$Year >= 1978 & df$Year<=1985,]

# Select only the abstract
A78_85 <- A78_85$Abstract

A78_85 <- tm_map(A78_85, PlainTextDocument)
strings <- c("neighborhoods")
abstracts <- str_replace_all(A78_85, strings, "neighborhood")
A78_85final <- data.frame(abstracts)

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
  "based", "CUSTOM",
  "results", "CUSTOM",
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
  word_counts, aes (x = word2, y = n/max(n), fill = "red")
) +
  geom_col() + 
  coord_flip() + 
  labs(
    title = "Most frequent words of filtered abstracts: 1978 - 1985",
    x = "Words", y ="Normalized frequency - Total of 2 papers",
    fill = NULL
  )


#################################################################################


# Selecionar artigos entre 1986 - 1990
A86_90 <- df[df$Year >= 1986 & df$Year<=1990,]
# Vetor das palavras
A86_90 <- A86_90$Abstract


A86_90 <- tm_map(A86_90, PlainTextDocument)
strings <- c("roads")
abstracts <- str_replace_all(A86_90, strings, "streets")
A86_90final <- data.frame(abstracts)

#Transformar abstract em palavras
tokenizing_abstract <- A86_90final %>%
  mutate(id = row_number()) %>% #Create new id for each abstract
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "based", "CUSTOM",
  "results", "CUSTOM",
  "discussion", "CUSTOM",
  "information", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) 

#Add new stopwords and repeat the tokenization process
tokenizing_abstract <- A86_90final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) #Remove stopwords (english package of stopwords)

#Frequency of words
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>1) %>% #Filter words that appear more than "n" times
  mutate(word2 = fct_reorder(word, n)) %>% 
  arrange(desc(n)) 


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/max(n), fill = "red")
) +
  geom_col() + 
  coord_flip() + 
  labs(
    title = "Most frequent words of filtered abstracts: 1986 - 1990",
    x = "Words", y ="Normalized frequency - Total of 1 paper"
  )

#################################################################################

# Selecionar artigos entre 1991 - 1995
A91_95 <- df[df$Year >= 1991 & df$Year<=1995,]
# Vetor das palavras
A91_95 <- A91_95$Abstract


A91_95 <- tm_map(A91_95, PlainTextDocument)
strings <- c("roads")
abstracts <- str_replace_all(A91_95, strings, "streets")
A91_95final <- data.frame(abstracts)


view(A91_95final)

library(tidytext)
#Transformar abstract em palavras
tokenizing_abstract <- A91_95final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "based", "CUSTOM",
  "information", "CUSTOM",
  "discussion", "CUSTOM",
  "results", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) #Acrescentar essas stopwords nas originais (linhas) e nao juntar colunas com o anti-join.


#Adicionar stopwords2
tokenizing_abstract <- A91_95final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) #Remove stopwords (english package of stopwords)

library(tidytext)
#Transformar abstract em palavras
#Contar o n?mero de palavras
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>3) %>% #Filtrar palavras que aparecem apenas mais de 100 vezes
  mutate(word2 = fct_reorder(word, n)) %>% #Ordenar no gr?fico por ordem decrescente
  arrange(desc(n)) #Colocar em ordem decrescente nas contagens


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/8, fill = "red")
) +
  geom_col() + #Criar gr?fico
  coord_flip() + #inverter gr?fico pra mostrar as palavras
  labs(
    title = "Most frequent words of filtered abstracts: 1991 - 1995",
    x = "Words", y ="Normalized frequency - Total of 3 papers"
  )

##################################################################################################


# Selecionar artigos entre 1996 - 2000
A96_00 <- df[df$Year >= 1996 & df$Year<=2000,]
# Vetor das palavras
A96_00 <- A96_00$Abstract


A96_00 <- tm_map(A96_00, PlainTextDocument)
strings <- c("cities")
abstracts <- str_replace_all(A96_00, strings, "city")
A96_00final <- data.frame(abstracts)


view(A96_00final)

library(tidytext)
#Transformar abstract em palavras
tokenizing_abstract <- A96_00final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "based", "CUSTOM",
  "information", "CUSTOM",
  "discussion", "CUSTOM",
  "results", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) #Acrescentar essas stopwords nas originais (linhas) e nao juntar colunas com o anti-join.


#Adicionar stopwords2
tokenizing_abstract <- A96_00final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) #Remove stopwords (english package of stopwords)

library(tidytext)
#Transformar abstract em palavras
#Contar o n?mero de palavras
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>8) %>% #Filtrar palavras que aparecem apenas mais de 100 vezes
  mutate(word2 = fct_reorder(word, n)) %>% #Ordenar no gr?fico por ordem decrescente
  arrange(desc(n)) #Colocar em ordem decrescente nas contagens


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/47, fill = "red")
) +
  geom_col() + #Criar gr?fico
  coord_flip() + #inverter gr?fico pra mostrar as palavras
  labs(
    title = "Most frequent words of filtered abstracts: 1996 - 2000",
    x = "Words", y ="Normalized frequency - Total of 19 papers"
  )

########################################################################################

# Selecionar artigos entre 2001 - 2005
A01_05 <- df[df$Year >= 2001 & df$Year<=2005,]
# Vetor das palavras
A01_05 <- A01_05$Abstract


A01_05 <- tm_map(A01_05, PlainTextDocument)
strings <- c("cities")
abstracts <- str_replace_all(A01_05, strings, "city")
A01_05final <- data.frame(abstracts)


library(tidytext)
#Transformar abstract em palavras
tokenizing_abstract <- A01_05final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "based", "CUSTOM",
  "results", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) #Acrescentar essas stopwords nas originais (linhas) e nao juntar colunas com o anti-join.


#Adicionar stopwords2
tokenizing_abstract <- A01_05final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) #Remove stopwords (english package of stopwords)


library(tidytext)
#Transformar abstract em palavras
#Contar o n?mero de palavras
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>7) %>% #Filtrar palavras que aparecem apenas mais de 100 vezes
  mutate(word2 = fct_reorder(word, n)) %>% #Ordenar no gr?fico por ordem decrescente
  arrange(desc(n)) #Colocar em ordem decrescente nas contagens


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/30, fill = "red")
) +
  geom_col() + #Criar gr?fico
  coord_flip() + #inverter gr?fico pra mostrar as palavras
  labs(
    title = "Most frequent words of filtered abstracts: 2001 - 2005",
    x = "Words", y ="Normalized frequency - Total of 13 papers"
  )


################################################################################


# Selecionar artigos entre 2006 - 2010
A06_10 <- df[df$Year >= 2006 & df$Year<=2010,]
# Vetor das palavras
A06_10 <- A06_10$Abstract


A06_10 <- tm_map(A06_10, PlainTextDocument)
strings <- c("cities")
abstracts <- str_replace_all(A06_10, strings, "city")
A06_10final <- data.frame(abstracts)


view(A06_10final)

library(tidytext)
#Transformar abstract em palavras
tokenizing_abstract <- A06_10final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "based", "CUSTOM",
  "results", "CUSTOM",
  "processes", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) #Acrescentar essas stopwords nas originais (linhas) e nao juntar colunas com o anti-join.


#Adicionar stopwords2
tokenizing_abstract <- A06_10final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) #Remove stopwords (english package of stopwords)


library(tidytext)
#Transformar abstract em palavras
#Contar o n?mero de palavras
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>10) %>% #Filtrar palavras que aparecem apenas mais de 10 vezes
  mutate(word2 = fct_reorder(word, n)) %>% #Ordenar no gr?fico por ordem decrescente
  arrange(desc(n)) #Colocar em ordem decrescente nas contagens


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/73, fill = "red")
) +
  geom_col() + #Criar gr?fico
  coord_flip() + #inverter gr?fico pra mostrar as palavras
  labs(
    title = "Most frequent words of filtered abstracts: 2006 - 2010",
    x = "Words", y ="Normalized frequency - Total of 31 papers"
  )

####################################################################################


# Selecionar artigos entre 2011 - 2015
A11_15 <- df[df$Year >= 2011 & df$Year<= 2015,]
# Vetor das palavras
A11_15 <- A11_15$Abstract


A11_15 <- tm_map(A11_15, PlainTextDocument)
strings <- c("cities")
abstracts <- str_replace_all(A11_15, strings, "city")
A11_15final <- data.frame(abstracts)


view(A11_15final)

library(tidytext)
#Transformar abstract em palavras
tokenizing_abstract <- A11_15final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "based", "CUSTOM",
  "results", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) #Acrescentar essas stopwords nas originais (linhas) e nao juntar colunas com o anti-join.


#Adicionar stopwords2
tokenizing_abstract <- A11_15final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) #Remove stopwords (english package of stopwords)


library(tidytext)
#Transformar abstract em palavras
#Contar o n?mero de palavras
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>40) %>% #Filtrar palavras que aparecem apenas mais de 100 vezes
  mutate(word2 = fct_reorder(word, n)) %>% #Ordenar no gr?fico por ordem decrescente
  arrange(desc(n)) #Colocar em ordem decrescente nas contagens


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/216, fill = "red")
) +
  geom_col() + #Criar gr?fico
  coord_flip() + #inverter gr?fico pra mostrar as palavras
  labs(
    title = "Most frequent words of filtered abstracts: 2011 - 2015",
    x = "Words", y ="Normalized frequency - Total of 103 papers"
  )

###############################################################################

# Selecionar artigos entre 2016 - 2020
A16_20 <- df[df$Year >= 2016 & df$Year<=2020,]
# Vetor das palavras
A16_20 <- A16_20$Abstract


A16_20 <- tm_map(A16_20, PlainTextDocument)
strings <- c("cities")
abstracts <- str_replace_all(A16_20, strings, "city")
A16_20final <- data.frame(abstracts)


view(A16_20final)

library(tidytext)
#Transformar abstract em palavras
tokenizing_abstract <- A16_20final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words) #Remove stopwords (english package of stopwords)


adicional_stopwords <- tribble(
  ~word,~lexicon,
  "study", "CUSTOM",
  "paper", "CUSTOM",
  "article", "CUSTOM",
  "based", "CUSTOM",
  "information","CUSTOM",
  "analysis", "CUSTOM",
  "discussion","CUSTOM",
  "results", "CUSTOM",
  "research", "CUSTOM")

stop_words2 <- stop_words %>%
  bind_rows(adicional_stopwords) #Acrescentar essas stopwords nas originais (linhas) e nao juntar colunas com o anti-join.


#Adicionar stopwords2
tokenizing_abstract <- A16_20final %>%
  mutate(id = row_number()) %>% #Criar novo ID para cada paper
  unnest_tokens(word, abstracts) %>%
  anti_join(stop_words2) #Remove stopwords (english package of stopwords)


library(tidytext)
#Transformar abstract em palavras
#Contar o n?mero de palavras
word_counts <- tokenizing_abstract %>%
  count(word) %>%
  filter(n>130) %>% #Filtrar palavras que aparecem apenas mais de 100 vezes
  mutate(word2 = fct_reorder(word, n)) %>% #Ordenar no gr?fico por ordem decrescente
  arrange(desc(n)) #Colocar em ordem decrescente nas contagens


#Plot word count
ggplot(
  word_counts, aes (x = word2, y = n/680, fill = "red")
) +
  geom_col() + #Criar gr?fico
  coord_flip() + #inverter gr?fico pra mostrar as palavras
  labs(
    title = "Most frequent words of filtered abstracts: 2016 - 2020",
    x = "Words", y ="Normalized frequency - Total of 326 papers"
  )

