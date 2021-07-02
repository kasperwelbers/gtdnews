#' Extract GTD events with news article annotations
#'
#' Create a data.frame in which each row is a unique event, with variables indicating whether
#' the event matches at least one news article (has_news) and the number of news articles (N_news)
#'
#' @param g               The graph data, as returned by the compare_documents function.
#' @param weight_thres    The threshold for the similarity score
#'
#' @return
#' @export
#'
#' @examples
gtd_event_coverage <- function(g, weight_thres) {
  g$d$.use = g$d$weight > weight_thres

  meta = subset(g$from_meta, .complete_window)
  e = subset(g$d, .use)

  e = merge(e, g$to_meta[,c('document_id'),drop=F], by.x='to', by.y='document_id', all.x=T)
  e = data.table::as.data.table(e)
  e = e[, list(N_news=length(weight), has_news=T), by='from']

  e = merge(meta, e, by.x='document_id', by.y='from', all=T)
  e$N_news[is.na(e$N_news)] = 0
  e$has_news[is.na(e$has_news)] = 0

  e$killed[is.na(e$killed)] = 0
  e$wounded[is.na(e$wounded)] = 0
  e
}
