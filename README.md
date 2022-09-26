Using text mining in systematic literature reviews
================

This page was created to reproduce the results of the paper:

> *Using text mining in systematic literature reviews: Emerging road
> space allocation solutions for smart cities*. Paper submitted to the
> International Journal of Information Management Data Insights. 2022.

Also, this material provides the code and guidelines that explain step
by step on performing text mining in systematic literature reviews. The
code is reproducible and can be replicated to other literature reviews
regardless of the field of study. Please **cite** the paper above, in
case you use any of the material.

#### 1. Dataset

-   [Excel sheet with main information of all collected papers and
    filtering process.](/Data/table_systematic_review.xlsx)

-   [Full eligible papers used for the topic modelling and bigrams in
    \*.txt](/Data/Full_papers/)

#### 2. Guidelines

-   [Historical Context Analysis](Historical_Context_Abs_TM.md):
    Analysis of how the concepts and topics evolve throughout time by
    using word frequency counts in abstracts.

-   [Specific Paper Analysis](LDA_Bigrams_Full_Papers.md): Detecting the
    main topics expressed in the literature by using topic modeling and
    bigrams in full papers.

#### 3. Code

The full scripts used for producing all the results are found in:

-   [Historical Context Analysis](/Scripts/Script_Historical_Analysis.R)

-   [Specific Paper Analysis](/Scripts/Script_Specific_Analysis.R)
