#' Parse the `Author` field from a DESCRIPTION file
#' 
#' Parse the `Author` field from a DESCRIPTION file into a `person` object
#'
#' @param author_string A character containing the `Author` or `Maintainer` 
#'   field from a  `DESCRIPTION` file
#'
#' @return A `person` object
#' 
#' @importFrom utils as.person
#' 
#' @examples
#' # Read from a DESCRIPTION file directly
#' utils_description <- system.file("DESCRIPTION", package = "utils")
#' utils_matrix <- read.dcf(utils_description, "Author")
#' 
#' parse_authors(authors_matrix)
#' 
#' # Read from a database of CRAN metadata
#' cran_epidemiology_packages$Author |> 
#'   parse_authors()
#' 
#' @export
parse_authors <- function(author_string) {
  
  # Sanitize input from pkgsearch / crandb
  author_string <- author_string |> 
    stringi::stri_replace_all_regex(
      "<U\\+([0-9A-Fa-f]+)>",
      "\\u\1"
    ) |> 
    stringi::stri_unescape_unicode()
  
  authors_no_brackets <- author_string |> 
    remove_brackets("(") |> 
    remove_brackets("[") |>
    remove_brackets("<")
  
  authors_no_brackets <- stringi::stri_replace_all_fixed(
    authors_no_brackets,
    "\n",
    " "
  )
  
  separators <- c(
    "\\s*,\\s*",
    "\\band\\b",
    "\\bwith contributions (of|from|by)\\b"
  )
  
  authors_person <- authors_no_brackets |> 
    stringi::stri_split_regex(paste(separators, collapse = "|")) |> 
    lapply(trimws) |> 
    lapply(as.person)
  
  if (length(authors_person) == 1) {
    authors_person <- authors_person[[1]]
  }
  
  return(authors_person)
}

