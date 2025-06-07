#' Parse the `Author` field from a DESCRIPTION file
#'
#' Parse the `Author` field from a DESCRIPTION file into a `person` object
#'
#' @param author_string A character containing the `Author` or `Maintainer`
#'   field from a  `DESCRIPTION` file
#'
#' @return A character vector, or a list of character vectors of length equals
#' to the length of `author_string`
#'
#' @importFrom utils as.person
#'
#' @examples
#' # Read from a DESCRIPTION file directly
#' utils_description <- system.file("DESCRIPTION", package = "utils")
#' utils_authors <- read.dcf(utils_description, "Author")
#'
#' parse_authors(utils_authors)
#'
#' # Read from a database of CRAN metadata
#' cran_epidemiology_packages$Author |>
#'   parse_authors() |>
#'   unlist() |>
#'   unique() |>
#'   sort()
#'
#' @export
parse_authors <- function(author_string) {
  # Sanitize input from pkgsearch / crandb
  author_string <- stringi::stri_replace_all_fixed(
    author_string,
    "<U+000a>",
    " "
  )

  authors_no_brackets <- author_string |>
    remove_brackets("(") |>
    remove_brackets("[") |>
    remove_brackets("<")

  authors_person <- authors_no_brackets |>
    # Extra common strings
    stringi::stri_replace_all_regex("\\bet\\.? al\\.?\\b", " ") |>
    stringi::stri_replace_all_regex("\\b(Prof|Dr|Mr|Mrs|Ms)\\.?\\s*", "") |>
    # Separators
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_replace_all_regex(
      "\\b(with contributions?|contributed datasets) (of|from|by)\\b:?",
      ", "
    ) |>
    stringi::stri_replace_all_regex("\\bR [pP]ort [bB]y\\s+", ", ") |>
    stringi::stri_replace_all_regex("\\band\\b", ", ") |>
    stringi::stri_split_regex("\\s*(,\\s*)+") |>
    # Clean string boundaries
    lapply(function(x) stringi::stri_replace_all_regex(x, "\\.$", "")) |>
    lapply(trimws) |>
    # For strings terminating with ","
    lapply(function(x) setdiff(x, ""))

  if (length(authors_person) == 1) {
    authors_person <- authors_person[[1]]
  }

  return(authors_person)
}
