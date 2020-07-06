Third part of online appendix: Validation and Analysis
================

# Load data

The data for this part can either be created in the second part, or the
exact data used in the paper can be downloaded from the online appendix
github page.

``` r
library(gtdnews)
library(data.table)

file_url = 'https://github.com/kasperwelbers/gtdnews/raw/master/GTD_matching/sim_data.rds'
download.file(file_url, 'sim_data.rds')
g = readRDS('sim_data.rds')
```

The object `g` is a list with 3 data.frames (in data.table format), that
contain the results of the document comparison (GTD events to news
articles) in a network format. The `g$d` data.frame is the edgelist with
event-article pairs. `g$from_meta` and `g$to_meta` are the meta data for
the from (GTD events) and to (Guardian articles) nodes.

``` r
g$d           ## the matches
g$from_meta   ## gtd meta data
g$to_meta     ## guardian meta data
```

The edgelist has already been filtered to only have event-article paris
where the similarity (weight) was at least 1. Otherwise, the data would
have been much larger, and we don’t use this information. However, note
that for the unsupervised validation method we do need to know the total
number of news article to which each event has been compared, seperately
for articles published before the event (\(N_{before}\)) and after the
event (\(N_{after}\)). These numbers can be obtained from the
`from_meta` data, which has the column `from_n` (how many comparisons in
total) and `lag_n` (how many comparison with negative time difference).

Also important to know is that for events near the start and end of the
period of analysis we do not have valid results because they do not have
a complete comparison window. Since our comparison window ranges from 30
days before the event to 7 days after the event, this means that events
in the first month and last week of our data cannot be used. So, we only
use events where the `.complete_window` column in `g$from_meta` is TRUE.

# Validation

## Unsupervised validation

### figure 2

To produce figure 2, we plot histograms of the `hourdiff` column in the
edgelist, which gives the time difference between the event and news
article in hours. We divide this by 24 to get the difference in days.
Three plots side by side (`mfrow = c(1,3)`) are presented, that show
this histogram for the similarity thresholds 2, 7 and 14. There is no
particular reason for these threshold, other than that it nicely shows
high recall (left), high precision (right), and a more balanced
thresholds with presumably high F1 (mid).

``` r
par(mar=c(4.5,4,1,4), mfrow=c(1,3))  ## plot side by side
for (thres in c(2,7,14)) 
  x=hist(g$d$hourdiff[g$d$weight > thres] / 24, right = F, main='', xlab='Day difference')
```

For reference, the `right = FALSE` parameter means that histogram cells
are left-closed. This is required so that all results for a day
difference of zero or higher are in the bar on the right of the zero
point the x-axis.

### figure 3

This function uses the method for estimating precision (P), recall (R)
and F1 for a range of similarity thresholds. The start and end of the
range is specified in the `weight_range` argument, and the `steps`
argument gives the number of points into which this range is divided.
Here we use 200 steps, meaning that we get 200 estimations. The
recall\_precision\_thres determines which of these estimations is used
for our approximation of \(TP_{recall\approx100}\) (see paper for
details). Here we say that we use the lowest similarity threshold that
has an estimated precision of at least 5%.

``` r
est_pr = estimate_validity(g, weight_range = c(1,20), steps=200, 
                           recall_precision_thres = 0.05)
```

est\_pr contains the estimated precision and recall (and F1) scores. We
will use this below to compare with the gold standard validation.

Reviewer note: A more generalized version of this function will be added
to the RNewsflow R package.

## Gold standard validation

### gold standard data

The gold standard data is included as a (lazy) data object in the R
package for this appendix.

``` r
gold_matches$description
head(gold_matches$articles)
head(gold_matches$matches)
```

### figure 4

This function computes the P, R and F1 scores based on the gold
standard. The `weight_range` and `steps` arguments work in the same way
as in the `estimate_validity` function.

``` r
gold_pr = gtd_pr(g, weight_range = c(1,20), steps = 200)
```

## Comparison of validation methods

Here we compare the est\_pr and gold\_pr results. Note that this only
works if the weight\_range and steps arguments for the functions that
generated this data are identical.

### figure 5

Plot the F1 scores

``` r
par(mfrow=c(1,1), mar=c(4,4,1,0), xpd=F)
plot(est_pr$threshold, est_pr$F1, type='l', 
     ylim = c(min(est_pr$F1, gold_pr$F1m), max(est_pr$F1, gold_pr$F1m)+10), 
     xlab='Similarity threshold', ylab='F1', lwd=2, col='grey', bty='l')
lines(gold_pr$weight, gold_pr$F1m, lty=1, lwd=1)
```

Add the vertical bars for the highest F1 scores

``` r
top_est = round(est_pr$threshold[est_pr$F1 == max(est_pr$F1)][1], 2)
top_gold = round(gold_pr$weight[gold_pr$F1m == max(gold_pr$F1m)][1], 2)

graphics::abline(v=top_gold, lty=2)
graphics::abline(v=top_est, lty=2, col='darkgrey')
graphics::text(x=top_gold-0.4, y=19, labels=top_gold, 
               srt=0, adj=1, font=3, cex=1, family='mono')
graphics::text(x=top_est+1.85, y=19, labels=top_est, 
               srt=0, adj=1, font=3, cex=1, family='mono')

legend('topright', bty='n', legend = c('weak supervision', 'gold standard'), 
       lty=c(1,1), lwd=c(2,1), col=c('grey', 'black'))
```

# Analysis

To analyze which events are covered in the news, we transform `g` so
that each row is a unique event, with variables for whether the event
matches at least one news article (has\_news) and the number of news
articles (N\_news).

For the analysis in the paper we used a threshold of 6.54 (`weight_thres
= 6.54`) based on our validation analysis. Also, we limited our focus to
events with at least one fatal victim (`killed > 0`).

``` r
e = gtd_event_coverage(g, weight_thres = 6.54)
e = subset(e, killed > 0) ## only include fatal GTD events
```

As reported in the paper, results of the statistical analysis are quite
robust for small to moderate changes in the threshold. Also, if GTD
events without fatal victims (around half of all events) are not
removed, we mostly find the same effects, but with different effect
sizes, and the effect of cultural values distance (one of the two
cultural distance variables) disappears. We decided to focus only on
fatal events because we belief that for the non-fatal attacks the
difference between an event and non-event (or at least non-terrorist
event) is more ambiguous. However, there could still be relevant
information pertaining to the gatekeeping function in the coverage of
non-fatal events. A more detailed analysis to untangle how predictors of
coverage differ for fatal and non fatal attacks did not fit the scope of
our paper, but it can be explored here by skipping the subset.

It should also be pointed out that the GTD contains more event
characteristics than we included here. This can be merged to the data to
explore other predictors of coverage.

## Country level variation

To create the worldmaps (figure 6 and 7) we need to locate the region of
the GTD events. We use the latitude and longitude of events and find the
closest regions.

``` r
e$region = get_region_id(e$lon, e$lat, e$country)
```

Now we can aggregate to
region.

``` r
geo = e[,list(sum_news=sum(N_news),     ## total number of news articles 
              mean_news=mean(N_news)),    ## average news articles per event
              by='region']
```

### Figure 6 and 7

The `plot_worldmap` function wraps the code (mainly using ggplot2) for
visualizing the worldmap. The input is the region name and a numeric
score for that region. THe difference between figure 6 and 7 is only
whether the sum or mean of news articles per region is used.

``` r
plot_worldmap(geo$region, geo$sum_news)   ## figure 6
plot_worldmap(geo$region, geo$mean_news)  ## figure 7
```

## Multilevel regression analysis

For the multilevel analysis we use the lme4 package to fit the model,
and the sjPlot package to display the results.

``` r
library(lme4)
library(sjPlot)
```

### table 2

For the analysis of table 2 we used only the events for which we have
the cultural distance. Here we merge the event data `e` (as created
above) to the UK\_dist\_scores data, which is a data frame with distance
measures of the UK to other countries. Since this data only contains
countries for which we have the cultural distance (UN countries that
participated in the World Values Survey wave 5), this merge only returns
a subset of events.

``` r
e2 = merge(e, UK_dist_scores, by='country')
```

Now we fit the generalized linear models (glmer) with a binomial error
distribution (by default uses the logistic link function). Note that if
non-fatal GTD events are included in the data, the natural log
transformation of `killed` is undefined for non-fatal events. A solution
would be to use a log plus one transformation, for which you can simply
replace the `log(killed)` with `log1p(killed)`.

``` r
m1 = glmer(has_news ~ 1 + (1| country), 
           data=e2, family = binomial)
m2 = glmer(has_news ~ 1 + log(killed) + suicide + (1| country), 
           data=e2, family = binomial)
m3 = glmer(has_news ~ 1 + log(killed) + suicide + scale(geo_dist) + (1 | country), 
           data=e2, family = binomial)
m4 = glmer(has_news ~ 1 + log(killed) + suicide + scale(geo_dist) + value_dist + UN_disagree + (1 | country), 
           data=e2, family = binomial)

anova(m1,m2,m3,m4)
tab_model(m1,m2,m3,m4, show.se = T)
```

### table 2: all countries

For the all countries model in table 2 we perform the same analysis, but
without merging to the UK\_dist\_scores. Instead, we merge with
UK\_geo\_dist, which only contains the geographic distance but for all
countries in the data.

``` r
e3 = merge(e, UK_geo_dist, by='country')

m1 = glmer(has_news ~ 1 + (1| country), 
           data=e3, family = binomial)
m2 = glmer(has_news ~ 1 + log(killed) + suicide + (1| country), 
           data=e3, family = binomial)
m3 = glmer(has_news ~ 1 + log(killed) + suicide + scale(geo_dist) + (1 | country), 
           data=e3, family = binomial)

anova(m1,m2,m3)
tab_model(m2,m3, show.se=T)
```

### figure 8

To create figure 8, we first loop over different weight thresholds, fit
the full model for this threshold, and store the coefficients for each
model.

``` r
l = list()
for (weight_thres in seq(1, 20, by=0.5)) {
  print(weight_thres)
  e = gtd_event_coverage(g, weight_thres = weight_thres)
  e = subset(e, killed > 0)
  e = merge(e, UK_dist_scores, by='country')
  full_model = glmer(has_news ~ 1 + log(killed) + suicide + scale(geo_dist) + value_dist + UN_disagree + (1 | country), 
                     data=e, family = binomial)
  
  x = plot_model(full_model)
  x = x$data
  x$threshold = weight_thres
  l[['']] = x
}

d = rbindlist(l)
```

For the following visualization we also need to install the facetscales
package. This is not a dependency of the gtdnews package because it is
only on github. The following code installs the package directly from
github (Note that you need to have the remotes package installed)

``` r
remotes::install_github("zeehio/facetscales")
```

``` r
library(ggplot2)
library(facetscales)

## Set nicer labels for the independent variables
d$term = as.character(d$term)
d$term[d$term == 'log(killed)'] = 'fatal victims'
d$term[d$term == 'suicide'] = 'suicide'
d$term[d$term == 'scale(geo_dist)'] = 'geographical dist.'
d$term[d$term == 'value_dist'] = 'cultural values dist.'
d$term[d$term == 'UN_disagree'] = 'UN voting diff.'

## make factor, so that ggplot puts the results in the right order
cnames = unique(d$term)
d$term = factor(as.character(d$term), levels=cnames)

## use custom y-axis limits
scales_y <- list(
  `fatal victims` = scale_y_continuous(limits = c(0,3)),
  `suicide` = scale_y_continuous(limits = c(0, 30)),
  `geographical dist.` = scale_y_continuous(limits = c(0,2)),
  `cultural values dist.` = scale_y_continuous(limits = c(0,2)),
  `UN voting diff.` = scale_y_continuous(limits = c(0,2))
)

ggplot(data = d, aes(threshold, estimate)) +
  geom_line(color = "steelblue", size = 1) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), colour="black", width=.1, ) +
  geom_point(color="steelblue") + 
  theme(plot.margin = unit(c(-1,0.4,0.4,0.4), "cm")) +
  geom_hline(yintercept=1, color='red', linetype=2) +
  labs(title = "", subtitle = "",
       y = "odds ratio", x = "similarity threshold") + 
  facet_grid_sc(term~., scales = list(y = scales_y))
```
