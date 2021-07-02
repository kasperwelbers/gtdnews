#' Prepare news data
#'
#' @param d             A data.frame in which each row is a news article.
#' @param doc_col       The name of the column with unique article ids
#' @param date_col      The name of the column with the date, in a format that can be coerced to a Date object with as.Date()
#' @param text_cols     The name(s) of the column(s) with article text. e.g. c('headline','byline','body')
#' @param docvars       The columns to include as docvars (article meta such as headline, url)
#' @param first_n_words Optionally, only include the first n words of the article text in the DTM
#' @param rm_time       If TRUE, remove time related terms, such as monday, januari and yesterday.
#'
#' @return A quanteda style DTM
#' @export
#'
#' @examples
prepare_news <- function(d, doc_col, date_col, text_cols, docvars=NULL, first_n_words=NA, rm_time=T) {
  message('Preparing texts')
  texts=list()
  for (col in text_cols) {
    text = as.character(d[[col]])
    text[is.na(text)] = ''
    texts[[col]] = text
  }

  ds = subset(d, select = unique(c(doc_col, docvars)))
  ds$text = do.call(paste, args = c(texts, list(sep='\n\n')))

  if (!is.na(first_n_words)) ds$text = first_n_words(ds$text, first_n_words)

  ds$date = as.POSIXct(d[[date_col]])

  message('Creating DTM')
  prepare_dtm(ds, doc_col=doc_col, first_n_words=NA, rm_time=rm_time)
}


## like get_guardian, but with a backup plan
really_get_guardian <- function(query, fromdate, todate, api.key, skip) {
  log <- capture.output({
    d = tryCatch(GuardianR::get_guardian(query, from.date = fromdate, to.date=todate, api.key = api.key),
                 error=function(e) NULL)
  })
  if (is.null(d)) {
    ## try to get data day by day
    dlist = list()
    getday = fromdate
    while (getday <= todate) {
      if (!getday %in% skip) {
        log <- capture.output({
          d = tryCatch(GuardianR::get_guardian(query, from.date = getday, to.date=getday, api.key = api.key),
                       error=function(e) {
                         message(sprintf('\n\nError on day %s: %s', getday, e))
                         NULL
                         })
        })
        if (is.null(d)) return(NULL)
        dlist[['']] = d
      }
      getday = getday + as.difftime(1, units='days')
    }
    d = data.table::rbindlist(dlist, fill = T)
  }
  d
}


#' Download articles from the Guardian
#'
#' This is a wrapper for the \code{\link[GuardianR]{get_guardian}} function from the GuardianR package.
#' Given a query, or character vector of query terms, and a from and to date, it will download the data
#' in batches, and once downloaded return all articles as a data.frame. This makes it possible to collect
#' all the data. If the download is interrupted (e.g., internet issue, leaving work, tornado), it can
#' simply be resumed  by running the function again with the same arguments.
#'
#' @param query_terms  A character vector with Guardian queries. All terms are concatenated with OR operators
#'                     (grouped in parentheses).
#' @param api.key      An API key for the Guardian API, which can be obtained for free by filling in
#'                     \href{https://bonobo.capi.gutools.co.uk/register/developer}{this webform}.
#' @param fromdate     The starting date, in format "YYYY-MM-DD". e.g. "2010-01-01"
#' @param todate       The ending date, in format "YYYY-MM-DD". e.g. "2015-12-31"
#' @param skip         Optionally, a vector of days to skip, as a last resort.
#' @param path      The path to a directory in which the download directory is created.
#'                     Default is current working directory
#' @param stepsize     The number of days per batch.
#' @param verbose      If TRUE, report progress
#'
#' @return             A data.frame with guardian articles.
#' @export
#'
#' @examples
#' \donttest{
#' api.key = "[your own api key]"
#' query_terms = terrorism_news    ## (terrorism_news is data included in this package)
#' d = download_guardian(query_terms, api.key, fromdate = '2012-01-01', todate='2012-01-20')
#' }
download_guardian <- function(query_terms, api.key, fromdate, todate, skip=NULL, path=getwd(), stepsize=7, verbose=T) {
  if (!is.null(skip)) skip=as.Date(skip)
  query = paste(paste0('(',query_terms,')'), collapse=' OR ')
  query = gsub(' ', '+', query)

  # create dir
  query_hash = digest::digest(paste(stepsize, fromdate, 'query', query), algo = 'xxhash32')
  path = file.path(path, paste0('guardian_data_', query_hash))
  if (!dir.exists(path)) dir.create(path)
  message(paste("Directory: ", path))

  readme_path = file.path(path, 'readme.txt')
  if (!file.exists(readme_path)) {
    readme = paste0('From date: ', fromdate, '\n', 'To date: ', todate, '\n\n', query)
    writeLines(readme, con=readme_path)
  }

  date_steps = seq.Date(as.Date(fromdate), as.Date(todate), by = stepsize)

  needs_download=F
  for (i in 1:length(date_steps)) {
    fromdate_step = date_steps[i]
    todate_step = fromdate_step + as.difftime(stepsize-1, unit='days')
    if (todate_step > todate) todate_step = todate
    fname = file.path(path, sprintf('guardian_%s', fromdate_step))
    if (!file.exists(fname)) {
      if (needs_download == F) {
        message('Downloading Guardian data')
        if (verbose && length(date_steps) > 1) {
          pb = utils::txtProgressBar(min = 0, max = length(date_steps), style = 3)
          pb$up(i-1)
        }
        needs_download = T
      }

      ## here follows a rigorous to the point of silly approach to deal with Guardian API failure
      ## If an api call messes up, it is first attempted to get the data by getting it per day
      ## if that fails, we wait for 50 seconds and try the whole dance again.
      ## if that fails, we wait for 100 seconds... and so on until we tried 10 times.
      for (attempt in 1:10) {
        d = really_get_guardian(query, fromdate = fromdate_step, todate=todate_step, api.key = api.key, skip=skip)

        if (!is.null(d)) {
          break
        } else {
          waitfor = attempt*50   ## increment with attempt iteration
          warnmes = sprintf("Errors sometimes occur due to connection/api issues. We'll try again in %s seconds!!", waitfor)
          if (attempt == 1) message(paste0('\n', warnmes)) else message(warnmes)
          Sys.sleep(waitfor)
          next
        }

        stop("If you see this message, we tried making the API request for the current batch TEN TIMES, and it all failed.
              Clearly, something is wrong, and I hope it's just your internet connection, or that you reached your daily
              API limit of 5000 calls. It could also be that your query is invalid (weird symbols?). Otherwise it's
              possibly a bug in the Guardian API or the GuardianR package")

      }
      saveRDS(d, file=fname)
    }
    if (needs_download && verbose && length(date_steps) > 1) pb$up(i)
  }

  clean_text <- function(x) {
    x = gsub('<p>', '\n\n', x)
    x = gsub('<[^>]*>', '', x)
    x = gsub('\u0095', '', x)
    x = iconv(x, "", "cp1252", sub=' ')
    x = gsub('\xe2\x80', '”', x)
    iconv(x, "", "UTF8", sub = ' ')
  }

  files = list.files(path, full.names = T)
  files = files[!grepl('readme\\.txt$', files)]
  files = lapply(files, readRDS)
  d = data.table::rbindlist(files, fill = T)
  data.table::setnames(d, old='webUrl', new='url')
  d$headline = clean_text(as.character(d$headline))
  d$byline = clean_text(as.character(d$byline))
  d$body = clean_text(as.character(d$body))
  d$trailText = clean_text(as.character(d$trailText))
  d
}

#' Prepare the GTD data
#'
#' Creates a document term matrix in the \code{\link[quanteda]{dfm}} class.
#' Includes only the meta data used for the analysis presented in the vignette.
#'
#' @param d         A data.frame with the GTD data. You can download the GTD data for free
#'                  by filling out \href{https://www.start.umd.edu/gtd/contact/}{this form}.
#'                  You can read the .xlsx file with \code{\link[openxlsx]{read.xlsx}}.
#' @param fromdate  A Date (or character value that can be coerced to Date)
#' @param todate    A Date (or character value that can be coerced to Date)
#' @param rm_time       If TRUE, remove time related terms, such as monday, januari and yesterday.
#'
#' @return A quanteda style DTM
#' @export
#'
#' @examples
#' \donttest{
#' gtd_data = openxlsx::read.xlsx('GTD FILE AS DOWNLOADED FROM GTD WEBSITE')
#' gtd_dtm = prepare_gtd(gtd_data, fromdate='2010-01-01')
#' }
prepare_gtd <- function(d, fromdate=NULL, todate=NULL, rm_time=T) {
  d$imonth[d$imonth == 0] = 1
  d$iday[d$iday == 0] = 1
  d$date = strptime(paste(d$iyear, d$imonth, d$iday), '%Y %m %d')
  d$date = as.POSIXct(d$date)

  if (!is.null(fromdate)) d = d[as.Date(d$date) >= as.Date(fromdate),]
  if (!is.null(todate)) d = d[as.Date(d$date) < as.Date(todate),]

  ## for matching we need text, so add victim column that contain text based on numbers (wounded / killed)
  d$victims = ifelse(d$nwound == 0 | is.na(d$nwound), '', 'injured wounded hospital harmed hurt')
  d$victims = ifelse(d$nkill == 0 | is.na(d$nkill), d$victims, paste(d$victims, 'killed died dead casualties'))

  ####### prepare Document Term Matrix
  ## gtd has several text fields denoted with _txt and several text fields without _txt
  ## here we select the fields, and concatenate related fields and numbered fields (gname, gname1, gname2, etc.)
  cols = 'summary|^city|country_txt|attacktype[0-9]\\_txt|targtype[0-9]\\_txt|targsubtype[0-9]\\_txt|natlty[0-9]\\_txt|gname[0-9]|gsubname[0-9]|weaptype[0-9]\\_txt|weapdetail|victims'
  cols = grep(cols, colnames(d), value=T)

  ds = d[,cols,drop=F]
  for (col in cols) {
    ds[[col]] = as.character(ds[[col]])
    ds[[col]] = iconv(ds[[col]], from = 'latin1', to = 'UTF-8')
  }
  text = do.call(paste, args=as.list(ds))

  d = data.frame(id = d$eventid, date = d$date,
                 text = text,
                 url = paste0('https://www.start.umd.edu/gtd/search/IncidentSummary.aspx?gtdid=', as.character(d$eventid)),
                 country = d$country_txt,
                 city = d$city,
                 lat = d$latitude,
                 lon = d$longitude,
                 success = d$success,
                 suicide = d$suicide,
                 type = d$attacktype1_txt,
                 target = d$targtype1_txt,
                 weapon = d$weaptype1_txt,
                 killed = d$nkill,
                 wounded = d$nwound,
                 stringsAsFactors = F)
  d = d[!is.na(d$id),]
  prepare_dtm(d, doc_col='id', text_col = 'text', rm_time=rm_time)
}

first_n_words <- function(txt, n, batchsize=50000) {
  txt = as.character(txt)
  batch_i = get_batch_i(length(txt), batchsize=batchsize, return_list=T)

  for (i in seq_along(batch_i)) {
    x = stringi::stri_split(txt[batch_i[[i]]], fixed=' ')
    x = lapply(x, head, n)
    txt[batch_i[[i]]] = stringi::stri_paste_list(x, sep = ' ')
  }
  txt
}

get_batch_i <- function(n, n.batches=NA, batchsize=NA, return_list=F, return_vector=F, for_meta=F){
  if (!is.na(batchsize)) n.batches = ceiling(n / batchsize)

  if (!n.batches == 1){
    splits = split((1:n), cut(seq_along(1:n), n.batches, labels = FALSE))
    splits = sapply(splits, FUN=function(x) cbind(min(x), max(x)))
    batch_i = data.frame(start=splits[1,], end=splits[2,])
  } else batch_i = data.frame(start=1, end=n)

  if (return_list | return_vector) batch_i = mapply(seq, from=batch_i$start, to=batch_i$end, SIMPLIFY=F)
  if (return_vector) batch_i = rep(1:length(batch_i), sapply(batch_i, length))
  batch_i
}


prepare_dtm <- function(d, doc_col='id', text_col='text', first_n_words=NA, rm_time=T, grep_terroris=T) {
  if (!is.na(first_n_words)) {
    d[[text_col]] = first_n_words(d[[text_col]], 200)
  }

  if (grep_terroris) d$mentions_terrorism = grepl('terroris', d[[text_col]], ignore.case = T)

  ## we use text as the text field. Summary is added as meta info for inspecting results later on
  corp = quanteda::corpus(d, docid_field = doc_col, text_field = 'text')
  dtm = quanteda::dfm(corp, stem=T, remove=quanteda::stopwords(), tolower=T,  remove_punct=T)

  ## filter out whitespace and NA
  select = stringi::stri_detect_regex(colnames(dtm), "^\\P{Z}+\\p{Z}+") & !is.na(colnames(dtm))
  dtm = dtm[,!select]

  ## filter out terms without alpha and remove stopwords
  dtm = quanteda::dfm_keep(dtm, '[a-zA-Z]', valuetype='regex')

  if (rm_time) {
    tday = grepl('monday|tuesday|wednesday|thursday|friday|saturday|sunday', colnames(dtm), ignore.case = T)
    tmonth = grepl("januari|februari|march|april|may|june|juli|august|septemb|octob|novemb|decemb", colnames(dtm), ignore.case = T)
    tmisc = grepl('yesterd}|week|day|month|recent|year|today', colnames(dtm), ignore.case = T)
    dropcol = colnames(dtm)[tday | tmonth | tday]
    dtm = quanteda::dfm_remove(dtm, pattern=dropcol)
  }

  dtm
}

