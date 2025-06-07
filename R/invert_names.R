#' Invert 'LastName FirstName' to 'FirstName LastName' (or the reverse)
#'
#' @param names A character vector of potentially inverted names
#' @param correct_names A character vector of correct names
#'
#' @details
#' When you have a list `x` of mixed 'First Last' and 'Last First' names, but no
#' source of truth and you want to deduplicate them, this function can be used
#' as `expand_names(x, x)`, which will return the most common version available
#' in `x` for each name.
#'
#' @return A character vector with the same length as `names`
#'
#' @export
#'
#' @examples
#' invert_names(
#'   c("Wolfgang Mozart", "Mozart Wolfgang"),
#'   "Wolfgang Mozart"
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
#' epi_pkg_authors_normalized <- invert_names(epi_pkg_authors, epi_pkg_authors)
#'
#' length(unique(epi_pkg_authors_normalized))
#'
invert_names <- function(names, correct_names) {
  # Deal with the case where both first last and last first are in correct_names
  inverted_correct <- stringi::stri_replace_all_regex(
    correct_names,
    "^([^[:space:]]+)[[:space:]]+([^[:space:]]+)$",
    "$2 $1"
  )
  correct_df <- merge(
    table(correct_names),
    table(inverted_correct),
    by.x = "correct_names",
    by.y = "inverted_correct",
    all.x = TRUE
  )
  correct_names <- as.character(correct_df$correct_names[
    is.na(correct_df$Freq.y) | correct_df$Freq.x > correct_df$Freq.y
  ])

  # Cases that include multiple first or last names are too complex so we
  # restrict ourselves to the simpler first last == last first
  inverted <- stringi::stri_replace_all_regex(
    names,
    "^([^[:space:]]+)[[:space:]]+([^[:space:]]+)$",
    "$2 $1"
  )

  mtch <- match(inverted, correct_names)

  names[!is.na(mtch)] <- correct_names[mtch[!is.na(mtch)]]

  return(names)
}
