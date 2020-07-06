First part of online appendix: Data sources
================

# Data

Both the GTD data and the news articles from the Guardian are available
online. Although we cannot publish the data, we provide instructions and
code to download and prepare the data as we did. Notwithstanding
possible changes in the GTD data or the API of The Guardian, the entire
analysis can be replicated from scratch.

``` r
library(gtdnews)
```

Replicating the full analysis require a fair amount of memory, because
we will be working with fairly large Document Term Matrices (DTMs). It
is recommended to use a computer with at least 16Gb. This vignette can
be performed step-by-step, but it will take a while to download the
required data via the Guardian API. Furthermore, some steps take a while
to compute, so it is advised to make back ups. We therefore first make a
subdirectory (by default in the current working directory) to download
the data and prepare the text corpora.

``` r
dir.create('GTD_matching')
```

## GTD

We use the GTD data from 2006 to 2018. While the GTD data is openly
available, it has to be requested from the [GTD
website](https://www.start.umd.edu/gtd/contact/), which involves filling
out a form with basic usage information.

The data is provided as an .xlsx file, which can be read into R with the
`read.xlsx` function from the `openxlsx` package.

``` r
library(openxlsx)
gtd = read.xlsx(xlsxFile = '[path to GTD file]')
gtd = gtd[gtd$iyear >= 2006 & gtd$iyear <= 2018,]
```

The data can now be preprocessed into a Document Term Matrix (DTM). Here
we use the `dfm` (document feature matrix) format from the `quanteda`
package. The function `prepare_gtd` is provided in this online appendix
package to take all the necessary steps.

``` r
gtd = prepare_gtd(gtd, fromdate = '2006-01-01', todate='2018-12-31')
saveRDS(gtd, 'GTD_matching/gtd_dtm.rds')
```

## The Guardian

The Guardian has an API that provides free access to full texts. To
access this API you first need to [register for an API
key](https://bonobo.capi.gutools.co.uk/register/developer).

``` r
api.key = "api key here"
```

We can access the API through the `GuardianR` package. For convenience
we have made a wrapper function that downloads the data in batches to a
given folder. This way, the process (which can take around a day for
this study) can also be interupted. As long as the same query and
from/to date are used, it will resume from the last downloaded batch.

The `download_guardian` function requires a query, or a character vector
of query terms. For matching the GTD data we provide a list of terrorism
query terms. The list is intentionally very broad. The goal is to have
high recall (i.e. find all events that mention terrorist events) and we
are not that concerned about precision (i.e. finding only events that
mention terrorist events) in the data collection phase.

``` r
gua = download_guardian(terrorism_terms, api.key, 
                        fromdate = '2006-01-01', todate='2018-12-31', 
                        path = 'GTD_matching')
```

Sometimes `GuardianR` repeatedly fails to get the data for a particular
day due to an error in parsing the JSON. It is not entirely clear what
goes wrong, and it cannot be fixed without opening up `GuardianR`. A
quick (though less than ideal) fix is to skip these days by passing a
character vector with the dates to the `skip` argument in
`download_guardian`.

The data can now be preprocessed into a DTM with the `prepare_guardian`
function. Here we also have the `first_n_words` argument, which can be
used to only include the first `n` words of each articles in the DTM. We
used n = 200 in our analysis, assuming that articles that cover new
terrorist events mention the event early due to the inverted pyramid
structure of news (i.e. core message on top). This also mitigates issues
related to article length.

``` r
gua = prepare_news(gua, doc_col='id', date_col = 'webPublicationDate', 
                   text_cols = c('headline','body'), 
                   docvars = c('url','sectionName','publication'), 
                   first_n_words = 200)
saveRDS(gua, 'GTD_matching/gua_dtm.rds')
```
