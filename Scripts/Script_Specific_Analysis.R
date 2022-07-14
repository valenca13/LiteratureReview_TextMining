
#Import libraries
library(tm)
library(NLP)
library(stringr)
library(topicmodels)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(SnowballC)
library(textstem)


#Import dataset

#After downloading the papers we converted to ".txt" files and manually removed: 
#information of the authors and journal, acknowledgments, funding, supplemental material, disclosure statement, 
#references.This data cleaning ensures that the information provided by the model was only from the paper's text body.


folder <- "Data\\Full papers for LDA_bigrams"

filelist <- list.files(folder, pattern = ".txt") #select only documents ".txt"

filelist <- paste(folder, "\\", filelist, sep="") #Join documents.  

x <- lapply(filelist, FUN = readLines) #Considers each line as a different document. 

docs <- lapply(x, FUN = paste, collapse = " ")

#Data cleaning

#Remove punctuation
text <- gsub(pattern = "\\W", replace = " ", docs)
#Remove Numbers (digits)
text2 <- gsub(pattern = "\\d", replace = " ", text)
#Lowercase words
text3 <- tolower(text2)
#remove single words 
text4 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", text3) 
#Remove whitespace
text5 <- stripWhitespace(text4)
#Lematize terms in its dictionary form
text6 <- lemmatize_strings(text5, dictionary = lexicon::hash_lemmas)

adicional_stopwords <- c("good","represent", "present", "different","london", "may","datum","taipei", "numb", "much", "one", "two", "can", "fig", "will", "arm", "along", "xpj", "figure", "thus","aviv", "tel", "dsc","dscs","traf","also","study", "hackney", "wick", stopwords("en"))

#remove stopwords
text7 <- removeWords(text6, adicional_stopwords)

#A Good practice is to visualize the corpus every now and then.
writeLines(as.character(text7[[1]]))

# Remove words for bigrams
new_stopwords <- c("ow","exible", "cantly", "exibility", "uence", "uences", "ned")
text_bigram <- removeWords(text7, new_stopwords)

# Create corpus from vector
corpus <- Corpus(VectorSource(text7))

#Topic Modelling - Latent Dirichlet Allocation (LDA)

# Create document term matrix
dtm <- DocumentTermMatrix(corpus) 

#str(dtm)
#count top ten words
dtm.matrix <- as.matrix(dtm)
wordcount <- colSums(dtm.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 10)
print(topten)

#Number of topics (k) is defined prior to the model.
k <- 6

#Run LDA using Gibbs sampling
ldaOut <- LDA(dtm,
             k, 
             method="Gibbs", 
             control=list(seed = 42)) 
          
lda_topics <- ldaOut %>%
  tidy(matrix = "beta") %>%
          arrange(desc(beta))


# select 15 most frequent terms in each topic
word_probs <- lda_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  #Create term2, a factor ordered by word probability
  mutate(term2 = fct_reorder(term, beta)) 

  
# Plot term2 and the word probabilities
ggplot(
  word_probs,
  aes(term2,beta,fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  # Facet the bar plot by topic
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(x = "term")

##############################################################################

#Bigrams

library(quanteda)
library(igraph)
library(ggraph)
library(tidyverse)
library(tidyr)
library(broom)

#Create dataframe
df_corpus <- data.frame(text_bigram)


#Create bigrams by separating words in sequences of 2. 
bigrams_df <- df_corpus %>%
  unnest_tokens(output = bigram,
                input = text_bigram,
                token = "ngrams",
                n = 2)

#Count bigrams
bigrams_df %>%
  count(bigram, sort = TRUE)

#Remove stopwords in case it wasn't in the beginning.
#data("stop_words") 

#Separate words into two columns
bigrams_separated <- bigrams_df %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#Remove stopwords
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#Count the number of times two words are always together
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

#Create network of bigrams

bigram_network <- bigram_counts %>%
  filter(n > 15) %>% #filter for the most common combinations of bigrams that appear at least 15 times.
  graph_from_data_frame()

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.07, "inches"))

ggraph(bigram_network, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.03, 'inches')) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), vjust = .7, hjust = 0.3, size = 2.5) +
  theme_void()

