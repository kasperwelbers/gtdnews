#' Estimate weight threshold by estimating precision and recall
#'
#' This function estimates a weight threshold for a document comparison network that serves as an "event matching" task.
#' The "from" documents in the edgelist need to be events, or other types of documents of which you can be sure that the date of the "to" documents cannot precede them.
#'
#' For the estimation to work best, the following settings should be considered in the newsflow.compare function (that creates the g input).
#' \itemize{
#'   \item The similarity threshold should be low enough that you are confident that all true positives are in the data
#'   \item The right hand side of the comparison window should be the time frame in which 'real' matches are most likely to occur. This
#'         estimation was designed for the task of finding out if an event is covered in the news while the event is still news.
#'         We cannot guarantee that it also works for long term matching.
#'   \item The left side of the comparison window can be longer than the right side to get good false positive estimates
#'         (see details), but should not be too long. Twice the size of the right side of the window is sufficient in our experience.
#'   \item Note (!!) that this function requires the direct output from the newsflow.compare function. If the data is manipulated in between
#'         (e.g., increasing the weight threshold, changing the time window) the estimation won't work.
#' }
#'
#' See details for more information on how the estimation works.
#'
#' @param g         The edgelist output of newsflow.compare (use the argument: return_as = "edgelist").
#' @param weight_range   A vector of length 2, with the min and max weight threshold
#' @param steps   The number of observations for which to calculate the weight threshold
#' @param min_weight Optionally, set a minimum weight only for this calculation
#' @param do_plot   IF set to FALSE, do not plot results (results are also returned as a data.frame)
#' @param from_subset Optionally, a logical vector of the same length as nrow(g$from_meta) to look only as specific cases
#' @param weight_col the name of the column with the weight scores
#' @param n_sample  Draw a random sample of events. Overrides from_subset
#' @param recall_precision_thres To estimate the recall we need to estimate the number of true positives given a low precision (see details). Here you can specify the precision threshold.
#'
#' @details
#' We define a true positive as a match between a news document and event document where the news document indeed covers the event.
#' Accordingly, without actually looking at the news coverage, we can be sure that if the news document was published before the actual occurence of the event,
#' it is a false positive. We can use this information to get an estimate of the precision, recall and F1 scores. While the exact values of theses scores
#' will not be accurate, they can be used to see if certain differences in preparing or comparing the data (in particular, using different weight thresholds)
#' improves the results.
#'
#' To calculate these estimates we make the assumption that the probability of false positives in the matches for a given event
#' is the same before and after the event. We can then calculate the probability of false positives as the number of matches
#' divided by the number of news articles before the event to which the event has been compared (for the edgelist output of newsflow.compare,
#' the total number of comparisons is included in the "from_meta" attribute). We can then estimate the number of true positives as the observed
#' number of matches after the event minus the expected number of matches. To estimate the number of false negatives, we assume that the number of
#' true positives estimated with a low precision is an estimate of the real number of true positives. The precision level used can be specified in the recall_precision_threshold argument
#'
#'
#' @return A plot of the estimation, and the data.frame with estimates can be assigned
#' @export
estimate_validity <- function(g, weight_range, steps, min_weight=NA, do_plot=T, from_sample=NULL, weight_col='weight', n_sample=NA, recall_precision_thres=0.05) {
  if (!is.na(n_sample)) {
    from_sample = rep(F, nrow(g$from_meta))
    if (n_sample > length(from_sample)) stop('n_sample must be smaller than number of events')
    from_sample[sample(1:length(from_sample), size = n_sample)] = T
  }
  if (!is.null(from_sample)) {
    g$from_meta = subset(g$from_meta, from_sample)
    g$d = subset(g$d, from %in% g$from_meta$document_id)
  }

  before_n = sum(g$from_meta$lag_n, na.rm=T)
  after_n = sum(g$from_meta$from_n - g$from_meta$lag_n, na.rm=T)

  thresholds = seq(from=weight_range[1], to = weight_range[2], length.out=steps)
  if (!is.na(min_weight)) thresholds = thresholds[thresholds > min_weight]
  res = data.frame(threshold=thresholds, N_after=NA, M_after=NA, FP_prob=NA)

  before_event = g$d$hourdiff < 0
  for (i in 1:length(thresholds)) {
    above_thres = g$d[[weight_col]] >= thresholds[i]
    if (sum(above_thres) == 0) break

    res$N_after[i] = after_n
    res$M_after[i] = sum(above_thres & !before_event)
    res$FP_prob[i] = sum(above_thres & before_event) / before_n
  }
  res = res[!is.na(res$FP_prob) &! res$N_after == 0,]

  res$FP = res$FP_prob * res$N_after
  res$TP = res$M_after - res$FP
  res$P = ifelse(res$N_after == 0, NA, res$TP / res$M_after)

  res$R = ifelse(res$TP < 0, NA, res$TP / max(res$TP[res$P >= recall_precision_thres], na.rm=T))
  res$R[res$R > 1] = 1

  res$F1 = (2 * res$P * res$R) / (res$P + res$R)

  res$P = res$P * 100
  res$R = res$R * 100
  res$F1 = res$F1 * 100

  if (do_plot) {
    par(mar=c(4,3,4,2), mfrow=c(1,2), xpd=F)
    plot(res$threshold, res$P, type='l', lty=1, lwd=2,
         xlab='Similarity threshold', ylab='',
         ylim=c(0,100), bty='l', main='Threshold that optimizes F1')
    lines(res$threshold, res$R, type='l', lty=2, lwd=2)
    lines(res$threshold, res$F1, type='l', lty=1, lwd=3, col='darkgrey')

    top = which.max(res$F1)
    graphics::abline(v=res$threshold[top], lty=2)
    graphics::text(x=res$threshold[top], y=95, labels=paste(' weight =',round(res$threshold[top],2)), adj=0, font=3, cex=0.9, family='mono')
    graphics::legend('right', legend = c(expression(hat("P")), expression(hat("R")), expression(hat("F1"))), bty='n', lty=c(1,2,1), col=c('black','black','darkgrey'), lwd=2)

    g$d$daydiff = floor(g$d$hourdiff / 24)
    hist(g$d$daydiff[g$d[[weight_col]] >= res$threshold[top]], xlab='Day difference', main='Matches for this threshold', right=F, breaks=20)
  }

  invisible(res)
}


