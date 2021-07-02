#' Terrorism terms
#'
#' A set of query terms for finding news items that potentially mention terrorist events.
#' The list is intentially very broad. The goal is to have high recall (i.e. find all events
#' that mention terrorist events) and we are not that concerned about precision (i.e. finding only
#' events that mention terrorist events) in the data collection phase.
#'
#' @docType data
#' @usage data(terrorism_terms)
#' @format A character vector
#' @examples
#' terrorism_terms
"terrorism_terms"

#' Gold standard for GTD event matches
#'
#' Gold standard for GTD events matches in 500 random articles from the Guardian between
#' 2000 and 2015. Articles were manually coded, by looking whether a new terrorist event
#' was reported in the headline and lead (i.e. was the main topic of the news). If so, all
#' GTD events within 7 days before the article publication date and in the same country as
#' mentioned in the article were manually checked for a match. All matches are reported in
#' the $matches slot, which contains pairs of guardian article ids and GTD event ids."
#'
#' @docType data
#' @usage data(gold_matches)
#' @format A list with three slots: $description, $articles and $matches
#' @examples
#' gold_matches$description
#' head(gold_matches$articles)
#' head(gold_matches$matches)
"gold_matches"

#' Manually check positive matches
#'
#' 100 event - article pairs that are considered as matches in the analysis presented in the paper.
#' That is, using the preprocessing and document similarity calculation settings used in this
#' online appendix, and using the similarity threshold of 6.54 as obtained by the threshold estimation procedure.
#' The purpose of this data is to provide additional validation.
#'
#' The other gold standard is a completely random sample of articles for which all possible matches were coded,
#' which is required for getting a good indication of recall, but at the cost that only a limited number of
#' positive matches (113) is obtained, and only for a limited number of articles (27). This additional validation
#' of a random sample of 100 positive matches, for 100 unique guardian articles and 98 unique gtd events, provides a better
#' indication of the precision. In addition, comments are provided for all matches where it's not immediately obvious
#' that the article does or does not cover the GTD event.
#'
#' @docType data
#' @usage data(precision_check)
#' @format A data.frame
#' @examples
#' head(precision_check)
"precision_check"

#' Matches of different country names in GTD data and UN votes
#'
#' @docType data
#' @usage data(country_name_matches)
#' @format A data.table
#' @examples
#' head(country_name_matches)
"country_name_matches"

#' The political proximity, value distance and geographic distance of the UK to other countries
#'
#' @docType data
#' @usage data(UK_dist_scores)
#' @format A data.table
#' @examples
#' head(UK_dist_scores)
"UK_dist_scores"


#' The geographic distance of UK to other countries. Contains more countries than UK_dist_scores because it does not contain the cultural distances
#'
#' @docType data
#' @usage data(UK_geo_dist)
#' @format A data.table
#' @examples
#' head(UK_geo_dist)
"UK_dist_scores"
