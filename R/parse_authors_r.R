#' Parse the `Authors@R` field from a DESCRIPTION file
#' 
#' Parse the `Authors@R` field from a DESCRIPTION file into a `person` object
#'
#' @param authors_r_string A character containing the `Authors@R` field from a 
#'   `DESCRIPTION` file
#'
#' @return A `person` object
#' 
#' @examples
#' # Read from a DESCRIPTION file directly
#' devtools_description <- system.file("DESCRIPTION", package = "authoritative")
#' authors_r_devtools <- read.dcf(devtools_description, "Authors@R")
#' 
#' parse_authors_r(authors_r_devtools)
#' 
#' # Read from a database of CRAN metadata
#' cran_epidemiology_packages$`Authors@R` |> 
#'   parse_authors_r() |> 
#'   head()
#' 
#' @export
parse_authors_r <- function(authors_r_string) {

  # Sanitize input from pkgsearch / crandb
  authors_r_string <- authors_r_string |> 
    stringi::stri_replace_all_regex(
      "<U\\+([0-9A-Fa-f]+)>",
      "\\u\1"
    ) |> 
    stringi::stri_unescape_unicode()

  lapply(str2expression(authors_r_string), eval)

}