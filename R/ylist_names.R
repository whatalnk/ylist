#' Search Ylist
#'
#' Search Japanese species name, return species name.
#'
#' @importFrom pipeR %>>%
#'
#' @param session html session
#' @param query Japanese species name (character)
#' @export
ylist_names <- function(session, query) {
  hyouzyun <- as.character(parse(text = "\u6a19\u6e96"))
  re <- paste0("[[:space:]]", hyouzyun)
  form <- rvest::html_form(session)[[1]] %>>% rvest::set_values("any_field" = query)

  df_res <- rvest::submit_form(session, form) %>>%
    rvest::html_nodes(xpath = "//*[@id='content']/span/span/a") %>>%
    rvest::html_text()

  df_res %>>%
  {dplyr::data_frame(Species = .)} %>>%
    dplyr::filter(grepl(hyouzyun, Species)) %>>%
    dplyr::mutate(Species = gsub(re, "", Species)) %>>%
    tidyr::extract(col = Species, into = c("Species", "Jp.Species"),
                   regex = "([[:print:]]+)[[:space:]]([[:print:]]+)") %>>%
    dplyr::filter(Jp.Species == query) %>>%
    dplyr::mutate(Species = gsub("[[:space:]]$", "", Species))
}

#' Start Session
#'
#' Start Session for YList search
#' @export
ylist.session <- function(){
  rvest::html_session("http://ylist.info/ylist_simple_search.html")
}
