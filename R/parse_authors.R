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
    stringi::stri_replace_all_fixed(
      "<U+000a>",
      " "
    )
  
  authors_no_brackets <- author_string |> 
    remove_brackets("(") |> 
    remove_brackets("[") |>
    remove_brackets("<")

  authors_person <- authors_no_brackets |> 
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_replace_all_regex("\\bwith contributions\\s?(of|from|by)\\s?", ", ") |> 
    stringi::stri_replace_all_regex("\\band\\b", ", ") |> 
    stringi::stri_split_regex("\\s*,\\s*") |> 
    lapply(trimws)
  
  if (length(authors_person) == 1) {
    authors_person <- authors_person[[1]]
  }
  
  return(authors_person)
}

