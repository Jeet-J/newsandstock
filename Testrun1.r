install.packages('devtools')
devtools::install_github("hrbrmstr/webhose")
library(webhose)
library(httr)
library(jsonlite)
library(tidyverse)

WEBHOSE_API_TOKEN = '569012ab-39d4-444b-882b-e8563f283187'

filter_web_content <- function(query, sort = "relevancy",
                               ts = (Sys.time() - (3 * 24 * 60 * 60)),
                               order = "asc", size = 100, from = 0,
                               token = Sys.getenv("569012ab-39d4-444b-882b-e8563f283187")) {
  
  params <- list(
    token = token,
    format = "json",
    q = query,
    sort = sort,
    order = order,
    size = size,
    ts = ts
  )
  
  res <- httr::GET(
    url = "https://webhose.io/filterWebContent",
    query = params
  ) 
  
  httr::stop_for_status(res)
  res <- httr::content(res, as = "text", encoding = "UTF-8")
  res <- jsonlite::fromJSON(res, flatten = TRUE)
  res
}
mcga <- function(tbl) {
  x <- colnames(tbl)
  x <- tolower(x)
  x <- gsub("[[:punct:][:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("(^_|_$)", "", x)
  x <- make.unique(x, sep = "_")
  
  colnames(tbl) <- x
  
  tbl
}

#(country:US section_title:finance language:english) AND (thread.site_type:news AND is_first:true)
PRE_ALLOC_MAX <- 30
results <- vector(mode = "list", length = PRE_ALLOC_MAX)

i <- 1
from <- 0
repeat {
  res <- filter_web_content("(country:US section_title:finance language:english) AND (thread.site_type:news AND is_first:true)", 
                            ts = 1528744378547, from = from)
  results[[i]] <- res
  if (res[["moreResultsAvailable"]] > 0) {
    message("Fetching next 100 records...")
    i <- i + 1
    from <-  from + 100  
  } else {
    break
  }
}

discard(results, is.null) %>% 
  map_df(~{ .x$posts}) %>% 
  tbl_df() %>% 
  mcga()
