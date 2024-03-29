Second part of online appendix: Preprocessing and Comparison
================

  - [Matching the GTD events to the news
    articles](#matching-the-gtd-events-to-the-news-articles)
  - [Additional preprocessing for better
    matching](#additional-preprocessing-for-better-matching)
      - [Cluster terms with similar
        spelling](#cluster-terms-with-similar-spelling)
      - [Compute term combinations](#compute-term-combinations)
  - [Performing the comparison](#performing-the-comparison)

``` r
install.packages('RNewsflow')
```

# Matching the GTD events to the news articles

Our task is to find out whether a news article covers a **new**
terrorist attack (i.e. that happened recently), and if so match it to
the corresponding GTD event. To do this, we measure whether news
articles are sufficiently similar to GTD events that occured within
seven days before the article publication.

We measure the similarity of a GTD event and news article by looking at
overlapping words and word combinations, weighted for how informative
these words and word combinations are for distinguishing events. We
adopt an established approach for measuring document similarity, in
which documents are positioned in a vector space based on the occurence
of word and word combinations, and the similarity of their positions is
calculated.

In this section we describe additional preprocessing steps for creating
a good (and memory-efficient) vector space, and our method for
calculating the similarity. The required functions are published in the
`RNewsflow` package that we developed, and that we have updated for this
event matching task.

``` r
library(RNewsflow)
```

``` r
## load data as prepared in first part
gtd = readRDS('GTD_matching/gtd_dtm.rds')
gua = readRDS('GTD_matching/gua_dtm.rds')
```

# Additional preprocessing for better matching

The DTMs are prepared with based preprocessing techniques (stemming,
lowercasing, removing english stopwords). We use several additional
preprocessing steps to prepare the DTMs for the similarity calculation.
While some techniques have more state-of-the-art alternatives that might
yield better results on the matching task, we have purposefully
restricted ourselves to methods that can be performed directly within R
and with DTMs as input.

## Cluster terms with similar spelling

In a DTM, terms that have very similar spelling are nonetheless regarded
as completely separate columns. To some extent, this is accounted for by
preprocessing techniques such as stemming (applied in our case) or
lemmatization. For instance, with stemming, “attacking”, “attack” and
“attacks” are all transformed to “attack”. However, some words–in
particular namems–can have different spellings, such as “Gaddafi” or
“Gadhafi”. Moreover, some datasets have spelling mistakes. In the GTD
data, there are quite many spelling mistakes and slightly different
spellings for words, often related to differences between countries
(though the prescribed language is English).

We therefore first use a technique for clustering words with similar
spelling together as single columns. The steps are as follows. We first
extract all the unique column names (i.e. words) from the gtd and news
DTMs, which constitutes our *vocabulary*. We then apply the
`term_char_sim` function to calculate which terms are sufficiently
similar. By default, `term_char_sim` looks at overlapping tri-grams
(i.e. three consequtive characters) with some additional restrictions.

``` r
voc = union(colnames(gtd), colnames(gua))  
simmat = term_char_sim(voc, type = 'tri', min_overlap=2/3)
```

The result is an adjacency matrix of the words in the vocabulary, which
is 1 if words are sufficiently similar to be merged. We can now use this
matrix to cluster all similar terms together, for which we provide the
`term_union` function. Note that words are also clustered together if
they are related indirectly. That is, if A and B are related, and B and
C are related, Then a cluster ABC is created even if A is not related to
C.

``` r
gtd = term_union(gtd, simmat)               
gua = term_union(gua, simmat)               
```

This clustering approach is intentionally greedy. The vector space that
we’re creating is extremely sparse (as detailed below), so the chances
that a different but similarly spelled concept causes a false positive
are slim.\[1\] Still, it is always good practice to manually check how
the clustering performs by looking over some of the new columns. The new
column names contain all unique terms, connected with the | (OR)
operator.

``` r
cnames = colnames(gua)    
head(cnames, 20)    
```

## Compute term combinations

For our matching task, we want to use the features (i.e. terms) that are
most informative, i.e. that most clearly distinguish events. Very common
terms do not tell us much about whether news articles cover the same
event as described in a GTD entry. However, some terms such as “London”
and “attack” are not very informative on their own, but very informative
combined: the London attacks.

Common vector space calculations of similarity do not take into account
that the combination of “London” and “attack” together in the same
article is more informative than the sum of their individual
information. We therefore provide a function to add term combinations to
the DTM. This is not a common thing to do, because it greatly increases
the number of unique terms in the DTM (a quadratic increase) which is
often not feasible due to memory limitations. However, it is possible if
we impose a few selection criteria:

  - We drop all columns of which the document probability (max\_docprob)
    is higher than 0.01. That is, we ignore all terms and term
    combinations that occur in more than 1/100 documents.
  - We only keep terms and term combinations that occur in the GTD data.
    This does not affect our similarity score (which is calculated as
    the dot product).
  - We only keep combinations of terms if their observed frequency is
    more than 1.2 times the expected frequency. The expected frequency
    is the number of times we would expect two terms to co-occur based
    on sheer probability. We are not interested in term combinations
    that only occur due to sheer probability of co-occurence.

<!-- end list -->

``` r
sf = create_queries(dtm = gtd, ref_dtm = gua, 
                    max_docprob = 0.01, min_obs_exp = 1.2, 
                    min_docfreq = 1, verbose=T, weight = 'tfidf')
```

The output of `create_queries` is a list with the revamped gtd and gua
DTMs, now called `query_dtm` and `ref_dtm` (reference). Note that we use
term frequency (tf) inverse document frequency (idf) weighting.
Actually, since we only look at whether or not terms occur (tf is 0 or
1), this means that the weight of each term is its idf. By default, only
the idf scores of the reference dtm (ref\_dtm) are used. In our case,
this means that we only weight down common terms in the news articles,
which makes sense because we do not want to weight down common terms in
the GTD data, as this would weight down terrorism related terms.

# Performing the comparison

The `compare_documents` function is a specialized function for
calculating the inner product of DTMs. It has several important
benefits, but two are of particular importance for the task of matching
event data to news articles.

  - Events and news occur over time. We can leverage this to only
    compare events and news within a given time window. For large
    datasets this is critical, because it greatly reduces the number of
    comparisons. Even if time is not an issue, you would need a huge
    amount of memory. The `compare_documents` function uses a custom
    built matrix multiplication algorithm that incorporates an efficient
    sliding window approach. The `hour_window` function takes a numeric
    vector of length two that specifies the left and right borders of
    the window.
  - Even with fewer comparisons, the total number of comparisons quickly
    amounts to a number of non-zero results that would require a huge
    amount of memory. For our current task we can set a threshold for
    the minimum similarity score, which cuts of the vast majority of
    these results. The `compare_documents` function performs the
    calculation of the similarity measure and applies the given
    threshold (`min_similarity`) within the matrix multiplication
    algorithm.

The first two arguments are the two DTMs that are to be compared, in our
case the `query_dtm` (GTD) and `ref_dtm` (Guardian). We also specify the
time window in hours. Although we are only interested in matches within
seven days after the GTD event, we also look for all matches within a
window of 28 days before the event, because with this information we can
apply our unsupervised validation method. The `min_similarity` of 1 is a
very low threshold, which ensures a high recall. As the measure we use
the regular dot product.

``` r
g = compare_documents(sf$query_dtm, sf$ref_dtm, copy_meta=T, 
                      date_var='date', hour_window = c(-4*7*24,7*24), 
                      min_similarity=1, measure = 'dot_product', verbose=T)
```

The output `g` is a list that contains an edgelist data.table `d` in
which each row is a match, with columns indicating the GTD event id,
Guardian article id, weight of the match, and the hour difference. It
also contains the document meta information and certain additional
attributes of the document similarity comparison. This is stored
separately for from and to matches, in our case GTD events (`from_meta`)
and Guardian articles (`to_meta`).

1.  Note that later on we will weight the DTM based on overall term
    frequencies, so terms in very greedy clusters with and/or with
    common words will mostly become less informative, rather than result
    in false positives.
