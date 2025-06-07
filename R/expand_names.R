#' Expand names from abbreviated forms or initials
#'
#' @param short A character vector of potentially abbreviated names
#' @param expanded A character vector of potentially expanded names
#'
#' @details
#' When you have a list `x`of abbreviated and non-abbreviated names and you want
#' to deduplicate them, this function can be used as `expand_names(x, x)`, which
#' will return the most expanded version available in `x` for each name
#'
#' @return A character vector with the same length as `short`
#'
#' @export
#'
#' @examples
#' expand_names(
#'   c("W A Mozart", "Wolfgang Mozart", "Wolfgang A Mozart"),
#'   "Wolfgang Amadeus Mozart"
#' )
#'
#' # Real-case application example
#' # Deduplicate names in list, as described in "details"
#' epi_pkg_authors <- cran_epidemiology_packages |>
#'   subset(!is.na(`Authors@R`), `Authors@R`, drop = TRUE) |>
#'   parse_authors_r() |>
#'   # Drop email, role, ORCID and format as string rather than person object
#'   lapply(function(x) format(x, include = c("given", "family"))) |>
#'   unlist()
#'
#' # With all duplicates
#' length(unique(epi_pkg_authors))
#'
#' # Deduplicate
#' epi_pkg_authors_normalized <- expand_names(epi_pkg_authors, epi_pkg_authors)
#'
#' length(unique(epi_pkg_authors_normalized))
#'
expand_names <- function(short, expanded) {
  # Sort by string length to ensure match() gives priority to expanded names
  expanded <- expanded[order(nchar(expanded), decreasing = TRUE)]

  shortened <- data.frame(
    shortened_middle_name = stringi::stri_replace_all_regex(
      expanded,
      "\\s(\\w)[^\\s]*(?=\\s)",
      " $1 "
    ),
    no_middle_name = stringi::stri_replace_all_regex(
      expanded,
      "\\s[^\\s]+(?=\\s)",
      " "
    ),
    shortened_first_name = stringi::stri_replace_all_regex(
      expanded,
      "^(\\w)[^\\s]*\\s",
      "$1 "
    ),
    shortened_all_names = stringi::stri_replace_all_regex(
      expanded,
      "^(\\w)[^\\s]*\\s+(\\w)[^\\s]*(?=\\s)",
      "$1 $2"
    ),
    single_last_name = stringi::stri_replace_all_regex(
      expanded,
      "^((\\w)[^\\s]*(?=\\s)(\\s[^\\s]{2,}(?=\\s))+)\\s[^\\s]+$",
      "$1"
    )
  )
  for (i in seq_along(shortened)) {
    shortened[[i]] <- stringi::stri_replace_all_regex(
      shortened[[i]],
      "\\s+",
      " "
    )
  }

  hits <- vapply(shortened, \(x) match(short, x), integer(length(short)))

  longest_match <- apply(as.matrix(hits), 1, function(x) {
    if (all(is.na(x))) {
      return(NA)
    }
    return(min(x, na.rm = TRUE))
  })

  short[!is.na(longest_match)] <- expanded[longest_match[!is.na(longest_match)]]

  return(short)
}
