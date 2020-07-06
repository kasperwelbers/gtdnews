#' Plot Precision and Recall scores
#'
#' Uses the gold standard in the gold_matches data to calculate Precision and Recall scores.
#' Results are calculated separately for the article level (does the article have at least one GTD match) and
#' match level (each individual match of an article to a GTD event). Results are also calculated for different
#' Similarity thresholds, which will be plotted on the x-axis.
#'
#' The results are also returned as a list with data.frames.
#' See gold_matches$description for details on the gold standard.
#'
#' @param g               An edgelist, as created with newsflow.compare
#' @param weight_range    The range of weight to plot on the x axis
#' @param steps           The number of ticks on the x axis
#' @param weights         Optionally, provide a vector of weights. This overrides the weight_range and steps arguments
#' @param weight_col the name of the column with the weight scores
#' @param filter          Optionally, a logical vector indicating which rows of g$d to use.
#'
#' @return A plot an a list with data.frames
#' @export
#'
#' @examples
gtd_pr <- function(g, weight_range, steps, weights=NULL, weight_col='weight', filter=NULL, gtd_id_filter=NULL) {
  ## filter d on time and hourdiff beforehand for quicker computation
  hourdiff_range=c(0,7*24)
  d = g$d
  if (!is.null(filter)) d = d[filter,]
  d = d[d$hourdiff >= hourdiff_range[1] & d$hourdiff < hourdiff_range[2],]
  a = gold_matches$articles
  d = d[d$to %in% a$guardian_id,]

  if (is.null(weights)) {
    minw = weight_range[1]
    maxw = weight_range[2]
    weights = seq(minw, maxw, length.out = steps)
  }

  res = data.table::rbindlist(sapply(weights, function(w) calculate_gtd_pr(d, w, weight_col, gtd_id_filter=gtd_id_filter), simplify = F))

  m <- matrix(c(1,2,3,3), nrow = 2,ncol = 2,byrow = TRUE)
  minx = min(c(res$Pa, res$Pm))
  maxx = max(c(res$Pa, res$Pm))
  miny = min(c(res$Ra, res$Rm))
  maxy = max(c(res$Ra, res$Rm))
  layout(mat = m,heights = c(0.8,0.2))
  par(mar=c(4,3,4,2), xpd=F)
  plot(res$weight, res$Pa, type='l', lty=1, lwd=2,
       xlab='Similarity threshold', ylab='',
       ylim=c(0,100), bty='l', main='Article level')
  lines(res$weight, res$Ra, type='l', lty=2, lwd=2)
  lines(res$weight, res$F1a, type='l', lty=3, lwd=3, col='darkgrey')
  top = which.max(res$F1a)
  graphics::abline(v=res$weight[top], lty=2)
  graphics::text(x=res$weight[top], y=2, labels=paste(' weight =',round(res$weight[top],2)), adj=0, font=3, cex=0.9, family='mono')


  plot(res$weight, res$Pm, type='l', lty=1, lwd=2,
       xlab='Similarity threshold', ylab='',
       ylim=c(0,100), bty='l', main='Match level')
  lines(res$weight, res$Rm, type='l', lty=2, lwd=2)
  lines(res$weight, res$F1m, type='l', lty=3, lwd=3, col='darkgrey')
  top = which.max(res$F1m)
  graphics::abline(v=res$weight[top], lty=2)
  graphics::text(x=res$weight[top], y=2, labels=paste(' weight =',round(res$weight[top],2)), adj=0, font=3, cex=0.9, family='mono')


  par(mar=c(0,0,1,0), xpd=T)
  plot(1, type = "n", axes=FALSE, xlab="", ylab="")
  legend('top', legend=c('Precision','Recall','F1'), lty=c(1,2,3), lwd=c(2,2,3), col=c('black','black','darkgrey'),
         horiz=T, bty='n', cex=1.2)

  par(mar=c(4,4,4,4), mfrow=c(1,1))
  invisible(res)
}

#' Calculate precision, recall and F1 score for an edgelist
#'
#' This function calculates the precision, recall and F1 score for an edgelists containing matches
#' of GTD events and news articles from The Guardian
#'
#' See gold_matches$description for more information on the gold standard
#'
#' @param e  An edgelist in which the from column contains GTD event ids, and the to column contains
#'           The Guardian news article ids
#' @param min_weight Optionally, specify minimum weight to filter e before calculation
#'
#' @return A list with P, R and F1 scores for both the match and article level.
#' @export
calculate_gtd_pr <- function(e, min_weight=NA, weight_col='weight', hourdiff_range=c(0,Inf), gtd_id_filter=NULL) {
  a = gold_matches$articles
  m = gold_matches$matches
  if (!is.null(gtd_id_filter)) {
    m = m[m$gtd_id %in% gtd_id_filter,]
    a$has_match[!a$guardian_id %in% m$guardian_id] = F
  }
  m$real = T
  e = e[e$to %in% a$guardian_id,]
  if (!is.na(min_weight)) e = e[e[[weight_col]] >= min_weight,]

  e = e[e$hourdiff >= hourdiff_range[1] & e$hourdiff < hourdiff_range[2],]

  e$from = as.character(e$from)
  e$to  = as.character(e$to)
  e = merge(e,m, by.x=c('from','to'), by.y=c('gtd_id','guardian_id'), all=T)
  e$real[is.na(e$real)] = F
  e$hit = !is.na(e[[weight_col]])
  e$hit_f = factor(e$hit, levels=c(F,T))
  e$real_f = factor(e$real, levels=c(F,T))

  tab = table(hit=e$hit_f, real=e$real_f)
  Pm = round(tab[2,2] / sum(tab[2,]),5) * 100
  Rm = round(tab[2,2] / sum(tab[,2]),5) * 100
  F1m = round(2*(Rm * Pm) / (Rm + Pm),3)

  e2 = e[order(-e$real),]
  e2 = e2[!duplicated(e2$to),]
  tab = table(hit=e2$hit_f, real=e2$real_f)
  Pa = round(tab[2,2] / sum(tab[2,]),5) * 100
  Ra = round(tab[2,2] / sum(tab[,2]),5) * 100
  F1a = round(2*(Ra * Pa) / (Ra + Pa),3)

  data.frame(weight=min_weight, Pm=Pm, Rm=Rm, F1m=F1m, Pa=Pa, Ra=Ra, F1a=F1a)
}


#' Compare results of gtd_pr
#'
#' Calculates the difference of the precision and recall scores in pr1 and pr2.
#' This is calculated as pr2 minus pr1. So if precision in pr2 is 20, and precision in pr1 is 15,
#' the difference indicates an improvement of 5.
#'
#' @param pr1 Results of gtd_pr
#' @param pr2 Results of gtd_pr
#'
#' @return A plot an a list with data.frames
#' @export
#'
#' @examples
gtd_compare_pr <- function(pr1, pr2) {
  if (!identical(pr1$weight, pr2$weight)) stop('compare_pr can only be used if weight columns are identical')

  art_res = data.frame(weight = pr1$weight,
                       Precision = pr2$Pa - pr1$Pa,
                       Recall = pr2$Ra - pr1$Ra,
                       F1 = pr2$F1a - pr1$F1a)
  match_res = data.frame(weight = pr1$weight,
                       Precision = pr2$Pm - pr1$Pm,
                       Recall = pr2$Rm - pr1$Rm,
                       F1 = pr2$F1m - pr1$F1m)

  m <- matrix(c(1,2,3,3), nrow = 2,ncol = 2,byrow = TRUE)

  layout(mat = m,heights = c(0.8,0.2))
  par(mar=c(4,3,4,2), xpd=T)
  plot(art_res$weight, art_res$Precision, type='l', lty=1, lwd=2,
       xlab='Similarity threshold', ylab='',
       ylim=c(min(c(art_res$Precision,art_res$Recall,art_res$F1)),max(c(art_res$Precision,art_res$Recall,art_res$F1))),
       bty='l', main='Article level P/R')
  lines(art_res$weight, art_res$Recall, type='l', lty=2, lwd=2)
  lines(art_res$weight, art_res$F1, type='l', lty=3, lwd=3, col='darkgrey')

  plot(match_res$weight, match_res$Precision, type='l', lty=1, lwd=2,
       xlab='Similarity threshold', ylab='',
       ylim=c(min(c(match_res$Precision,match_res$Recall,match_res$F1)),max(c(match_res$Precision,match_res$Recall,match_res$F1))),
       bty='l', main='Match level P/R')
  lines(match_res$weight, match_res$Recall, type='l', lty=2, lwd=2)
  lines(match_res$weight, match_res$F1, type='l', lty=3, lwd=3, col='darkgrey')

  par(mar=c(0,0,1,0))
  plot(1, type = "n", axes=FALSE, xlab="", ylab="")
  legend('top', legend=c('Precision difference','Recall difference', 'F1 difference'), lty=c(1,2,3), lwd=c(2,2,3), col=c('black','black','darkgrey'),
         horiz=T, bty='n', cex=1.2)

  par(mar=c(4,4,4,4), mfrow=c(1,1))
  invisible(list(articles=art_res, matches=match_res))

}

