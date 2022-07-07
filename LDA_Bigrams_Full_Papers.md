Untitled
================

## Import libraries

``` r
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
```

## Import dataset

After downloading the papers we converted to “.txt” files and manually
removed: information of the authors and journal, acknowledgments,
funding, supplemental material, disclosure statement, references.

> Note: This prior data cleaning ensures that the information provided
> by the model was only from the paper’s text body.

``` r
folder <- "Data\\Full papers for LDA_bigrams"

filelist <- list.files(folder, pattern = ".txt") #select only documents ".txt"

filelist <- paste(folder, "\\", filelist, sep="") #Join documents.  

x <- lapply(filelist, FUN = readLines) #Considers each line as a different element (document). 

docs <- lapply(x, FUN = paste, collapse = " ")
```

#### Data cleaning

#### Create corpus

``` r
corpus <- Corpus(VectorSource(text7))
```

### Topic modelling - Latent Dirichlet Allocation (LDA)

#### Create Document-Term-Matrix (DTM)

``` r
dtm <- DocumentTermMatrix(corpus) 
str(dtm)
```

    ## List of 6
    ##  $ i       : int [1:12703] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ j       : int [1:12703] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ v       : num [1:12703] 1 5 1 1 6 1 1 1 4 1 ...
    ##  $ nrow    : int 12
    ##  $ ncol    : int 5153
    ##  $ dimnames:List of 2
    ##   ..$ Docs : chr [1:12] "1" "2" "3" "4" ...
    ##   ..$ Terms: chr [1:5153] "ability" "able" "abstract" "accept" ...
    ##  - attr(*, "class")= chr [1:2] "DocumentTermMatrix" "simple_triplet_matrix"
    ##  - attr(*, "weighting")= chr [1:2] "term frequency" "tf"

##### count ten words that appear more frequently

``` r
dtm.matrix <- as.matrix(dtm)
wordcount <- colSums(dtm.matrix)
topten <- head(sort(wordcount, decreasing=TRUE), 10)
print(topten)
```

    ##            bus            use          space           time           lane 
    ##            867            652            612            478            425 
    ##         system        traffic           city transportation          urban 
    ##            408            397            330            327            320

#### Define the number of topics (k)

``` r
k <- 6
```

> Note: The number of topics is defined prior to the model.

#### Run LDA using Gibbs sampling

``` r
ldaOut <- LDA(dtm,
             k, 
             method="Gibbs", 
             control=list(seed = 42)) 
```

``` r
lda_topics <- ldaOut %>%
  tidy(matrix = "beta") %>%
          arrange(desc(beta))
```

``` r
#lda_topics <- LDA(corpus,
              #k, 
              #method="Gibbs", 
              #control=list(seed = 42))
```

``` r
# select 15 most frequent terms in each topic
word_probs <- lda_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  #Create term2, a factor ordered by word probability
  mutate(term2 = fct_reorder(term, beta))
```

``` r
# Plot term2 and the word probabilities
ggplot(
  word_probs,
  aes(term2,beta,fill = as.factor(topic))
) + geom_col(show.legend = FALSE) +
  # Facet the bar plot by topic
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(x = "term")
```

![](LDA_Bigrams_Full_Papers_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
