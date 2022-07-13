Using text mining in systematic literature reviews
================

This repository was created for the replication of the results of the
paper:

> Valença, G.; Moura, F.; Morais de Sá. A. *Using text mining in
> systematic literature reviews: Methodology proposal and application to
> road space allocation.* Paper submitted to Communications in
> Transportation Research. 2022.

Additionally, the code presented in this repository can be adapted and
is reproducible to perform other literature reviews. This repository
also provides guidelines to use text mining in literature reviews.

If you use any of the material in this repository, **please cite the
reference above**.

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
