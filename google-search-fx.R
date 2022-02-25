#################################################################
# Google links scraper
#################################################################
# How this function can be used is demonstrated @ ...
#################################################################
# Content
#################################################################
# Dependencies
# Search query URL generator
# Search query result scraper
#################################################################


#################################################################
# Dependencies
#################################################################
library(RCurl)
library(XML)
library(rvest)
library(xml2)

#################################################################
# Search query URL generator
#################################################################
# parameters: search terms, language, google domain, whether to use quotes, number of pages
get_search_url <- function(search.term, language = 'de',  domain = '.ch', domain.site= ".ch",country="CH",quotes=TRUE, n.pages=1){
  # search term without quotes
  search.term <- gsub(' ', '%20', search.term) 
  # search term without quotes
  if(isTRUE(quotes)) search.term <- paste0('%22', search.term, '%22') 
  # paste everything together
  
  #google.url <- paste0('http://www.google', domain, '/search?hl=', language,'&q=',search.term,"&cr=country",country)
  google.url<- paste0('http://www.google', domain, '/search?q=',search.term,"&lr=lang_", language,"&cr=country",country,"&as_sitesearch=",domain.site,"&hl=",language)
  # if more than one page, add respective suffix and return, else return
  if(n.pages>1){
    return(c(google.url, paste0(google.url, '&ei=q-W9W-2MBoTCwALs5aPwBg&start=', (1:(n.pages-1))*10, '&sa=N')))
  }else{
    return(google.url)
  }
}

# google.url <- search.url[1]
#################################################################
# Search query result scraper
#################################################################
# parameters: google url, whether we want to return raw URLs, whether to drop recursive google URLs (e.g. picture or video recommendations)
get_google_hits <- function(google.url, raw=T, drop.recursives=F) {
  # setting user agent!! CHECK if issue
  # se <- session("https://httpbin.org/user-agent")
  # UA <- se$response$request$options$useragent
  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.212 Safari/537.36"
  httr::set_config(httr::user_agent(UA))
  
  # getting hit URLs
  raw.refs <- google.url %>% 
    session() %>% 
    html_nodes(xpath="//div[@class='yuRUbf']/a") %>% #div[@*class='yuRUbf']
    html_attr('href')
  
  # OLD FUNCTION START---->
  # doc <- getURL(URLencode(google.url), .opts=curlOptions(followlocation=TRUE, cookiefile="nosuchfile"))
  # # strip html
  # html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function(...){})
  # # get the respective nodes
  # nodes <- getNodeSet(html, "//h3//a")
  # # scrape the links
  # raw.refs <- sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]])
  # OLD FUNCTION STOP <----
  
  # drop recommendation links (picture or video recommendations [recursive searches])
  if(isTRUE(drop.recursives)) raw.refs <- raw.refs[!grepl('\\/search\\?q\\=', raw.refs)]
  # determine URL formatting
  if(isTRUE(raw)){
    return(raw.refs)
    }else{
    clean.refs <- gsub('(\\/url\\?q\\=)|(\\&sa.*)', '', raw.refs) %>% 
      sapply(., function(x) URLdecode(URLdecode(x)))
    return(clean.refs)
  }
}
