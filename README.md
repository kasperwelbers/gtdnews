
<!-- README.md is generated from README.Rmd. Please edit that file -->

# About this repository

This repository serves as an online appendix for \[DOI forthcoming\]. It
contains the code and data to replicate the analysis.

The appendix is composed of three parts.

  - The [first part](online_appendix/1_data_sources.md) describes how we
    download the GTD and The Guardian data, and prepare the Document
    Term Matrices (DTMs).
  - The [second part](online_appendix/2_preprocessing_and_comparison.md)
    contains the preprocessing of the DTMs and the calculation of the
    similarity scores.
  - The [third part](online_appendix/3_validation_and_analysis.md)
    describes the validation and analysis.

If you are only interested in replicating the analysis you can skip
straight to the third part, for which all required data is directly
included in the package. To replicate the study from scratch, you will
need to start at the first part, because we cannot include this data in
the package due to copyright restrictions. If you are interested in
applying our method for other data, you can start from the second part
using your own DTMs.

# Appendix helper package

This online appendix contains quite a lot of code, and requires several
packages We have therefore created a helper package that contains the
code in documented functions, and has the required packages as
dependencies. The easiest way to install this package (without cloning
the repository) is to use the `remotes` package, that allows you to
directly install it from github.

``` r
library(remotes)
install_github('kasperwelbers/gtdnews')
```
