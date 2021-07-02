

#' Extract a sample of edges to validate precision
#'
#' Makes it easier to inspect whether matches past a certain similarity threshold (positives) are correct matches (true positives)
#'
#' @param g            The RNewsflow edgelist
#' @param min_weight   The weight threshold for when an edge should be considered a match
#' @param n            The number of matches to sample
#' @param min_daydiff  The minimum number of days between the event and the news article
#' @param max_daydiff  The maximum number of days between the event and the news article
#'
#' @return
#' @export
#'
#' @examples
precision_validation_sample <- function(g, min_weight, n=100, min_daydiff=0, max_daydiff=6) {
  positives = g$d[g$d$weight > min_weight,]
  positives = positives[!positives$from %in% gold_matches$matches$gtd_id,]
  positives = positives[positives$daydiff>=min_daydiff & positives$daydiff <=max_daydiff,]
  s = positives[sample(1:nrow(positives), n),]

  s$gtd_url = sprintf("https://www.start.umd.edu/gtd/search/IncidentSummary.aspx?gtdid=%s", s$from)
  s$guardian_url = paste0('https://theguardian.com/', s$to)
  s[,c('gtd_url','guardian_url','weight','daydiff')]
}

