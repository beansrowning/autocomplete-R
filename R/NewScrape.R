require(jsonlite)
require(pryr)
require(rvest)
require(magrittr)

get_function_urls <- function(pkgname, rdocurl = NULL) {
  # Navigates to the main package page on rdocumentation
  # and strips all the URLs containing function documentation.
  # Args :
  #   pkgname : (str) Package name to look up in rdocumentation
  #               must match the package name on the website exactly.
  #   rdocurl : (str) Path to the packages section of the website
  #               only useful if website is reorganized.
  # Returns :
  #   function_pages : (list) of all URLs containing function documentation

  if (is.null(rdocurl)) {
    # Just incase the site changes
    rdocurl <- "http://www.rdocumentation.org/packages/"
  }
  # Take locally installed R version
  r_version <- substr(R.version.string, 11, 15)

  # Strip all function page URLs from the package page
  function_pages <- read_html(paste0(rdocurl, pkgname, "/versions/", r_version)) %>%
                      html_nodes("tbody td a") %>% html_attr(name = "href") %>%
                      lapply(function(x) paste0(rdocurl, substring(x, 11)))
  return(function_pages)
}

make_snippet <- function(page_html) {
  # Takes in full page HTML from a page containing R function
  # documentation and returns a single snippet or snippets that
  # are found on the page.
  # Args :
  #   page_html : (raw HTML) from read_html containing page source
  # Returns :
  #   snippet : (character vector) containing all snippeets generated from page

  snippet <- vector()
  #Strip the code block
  code_block <- page_html %>% html_nodes("pre code") %>% html_text()

  # Use "(" to count number of functions to index
  n <- length(gregexpr("\\(", code_block)[[1]])
  # Early return if no code block or operators
  if (n < 1 || length(code_block) == 0) {
    return(NULL)
  }
  
  for (i in 1:n){
    snippet[i] <- code_block
  }
}
